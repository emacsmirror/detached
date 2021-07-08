;;; dtache.el --- Dtache core -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://www.gitlab.com/niklaseklund/dtache.git
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience processes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Dtache allows a program to be seamlessly executed in an environment
;; that is isolated from Emacs.  This package provides the core
;; implementation.  Dtache sessions is supposed to be created and
;; interacted with through a front end package such as `dtache-shell'.

;; The package requires the program dtach[1] to be installed.
;;
;; [1] https://github.com/crigler/dtach

;;; Code:

;;;; Requirements

(require 'emacsql-sqlite)
(require 'tramp-sh)
(require 'autorevert)

;;;; Variables

(defvar dtache-session-directory nil
  "The directory to store `dtache' sessions.")
(defvar dtache-db-directory user-emacs-directory
  "The directory to store `dtache' database.")
(defvar dtache-db nil
  "The connection to the `dtache' database.")
(defvar dtache-program "dtach"
  "The `dtach' program.")
(defvar dtache-shell "bash"
  "Shell to run the dtach command in.")
(defvar dtache-metadata-annotators '((:git-branch . dtache--session-git-branch))
  "An alist of annotators for metadata.")
(defvar dtache-max-command-length 95
  "Maximum length of displayed command.")
(defvar dtache-attach-alternate-function #'dtache-open-log
  "Alternate function to use when attaching to inactive sessions.")
(defvar dtache-shell-history-file nil
  "File to store history.")

(defconst dtache-socket-ext ".socket"
  "The file name extension for the socket for `dtache-program'.")
(defconst dtache-log-ext ".log"
  "The file name extension for combined stdout and stderr.")
(defconst dtache-stdout-ext ".stdout"
  "The file name extension for stdout.")
(defconst dtache-stderr-ext ".stderr"
  "The file name extension for stderr.")
(defconst dtache-eof-message "\\[EOF - dtach terminating\\]\^M"
  "Message printed when `dtach' finishes.")
(defconst dtache-detached-message "\\[detached\\]\^M"
  "Message printed when `dtach' finishes.")
(defconst dtache-detach-character "\C-\\"
  "Character used to detach from a session.")
(defvar dtache-degraded-list '()
  "Regexps that should run in dedgraded mode.")
(defvar dtache-tail-interval 2
  "Interval in seconds for dtache to tail.")

;;;;; Private

(defvar dtache--dtach-mode nil "Mode of operation.")
(defvar dtache--session-candidates nil "An alist of session candidates.")
(defvar dtache--current-session nil "The current session.")

;;;; Data structures

(cl-defstruct (dtache-session (:constructor dtache--session-create)
                              (:conc-name dtache--session-))
  (id nil :read-only t)
  (command nil :read-only t)
  (working-directory nil :read-only t)
  (creation-time nil :read-only t)
  (session-directory nil :read-only t)
  (metadata nil :read-only t)
  (host nil :read-only t)
  (degraded nil :read-only t)
  (duration nil)
  (log-size nil)
  (active nil))

;;;; Functions

;;;;; Session

(defun dtache-select-session ()
  "Return selected session."
  (let* ((candidates (dtache-session-candidates))
         (candidate
          (completing-read "Select session: "
                           (lambda (str pred action)
                             (pcase action
                               ('metadata '(metadata (category . dtache)
                                                     (cycle-sort-function . identity)
                                                     (display-sort-function . identity)))
                               (`(boundaries . ,_) nil)
                               ('nil (try-completion str candidates pred))
                               ('t (all-completions str candidates pred))
                               (_ (test-completion str candidates pred))))
                           nil t nil 'dtache-session-history)))
    (dtache-decode-session candidate)))

(defun dtache-session-file (session file)
  "Return the path to SESSION's FILE."
  (let ((file-name
         (concat
          (dtache--session-id session)
          (pcase file
            ('socket dtache-socket-ext)
            ('log dtache-log-ext)
            ('stdout dtache-stdout-ext)
            ('stderr dtache-stderr-ext))))
        (directory (concat
                    (file-remote-p default-directory)
                    (dtache--session-session-directory session))))
    (expand-file-name file-name directory)))

(defun dtache-update-sessions ()
  "Update sessions in the database."
  (thread-last (dtache--db-select-active-sessions (dtache--host))
    (seq-remove (lambda (session)
                  (when (dtache--session-dead-p session)
                    (dtache--db-remove-session session)
                    t)))
    (seq-map #'dtache--session-update)
    (seq-map #'dtache--db-update-session)))

(defun dtache-cleanup-sessions ()
  "Remove dead sessions from the database."
  (thread-last (dtache--db-select-host-sessions (dtache--host))
    (seq-filter #'dtache--session-dead-p)
    (seq-map #'dtache--db-remove-session)))

(defun dtache-session-command (session)
  "Return SESSION's command."
  (dtache--session-command session))

(defun dtache-session-candidates ()
  "Return an alist of session candidates."
  (dtache-update-sessions)
  (let* ((sessions (nreverse
                    (dtache--db-select-host-sessions (dtache--host)))))
    (setq dtache--session-candidates
          (seq-map (lambda (session)
                     `(,(dtache-encode-session session) . ,session))
                   sessions))))

;;;;; Database

(defun dtache-db-initialize ()
  "Initialize the `dtache' database."
  (unless (file-exists-p dtache-db-directory)
    (make-directory dtache-db-directory t))
  (unless dtache-db
    (setq dtache-db
          (emacsql-sqlite
           (expand-file-name "dtache.db" dtache-db-directory)))
    (emacsql dtache-db
             [:create-table
              :if :not :exists dtache-sessions
              ([(id text :primary-key) host active dtache-session])])))

;;;;; Shell

(defun dtache-override-shell-history (orig-fun &rest args)
  "Override history to read `dtache-shell-history-file' in ORIG-FUN with ARGS.

This function also makes sure that the HISTFILE is disabled for local shells."
  (cl-letf (((getenv "HISTFILE") ""))
    (advice-add 'comint-read-input-ring :around #'dtache--shell-comint-read-input-ring-a)
    (apply orig-fun args)))

(defun dtache-save-shell-history ()
  "Add hook to save history when killing `shell' buffer."
  (add-hook 'kill-buffer-hook #'dtache--shell-save-history 0 t))

;;;;; Other

(defun dtache-setup ()
  "Setup `dtache'."
  (advice-add 'shell :around #'dtache-override-shell-history)
  (add-hook 'shell-mode-hook #'dtache-save-shell-history))

(defun dtache-dtach-command (session)
  "Return a dtach command for SESSION."
  (let* ((directory (dtache--session-session-directory session))
         (file-name (dtache--session-id session))
         (socket (concat directory file-name dtache-socket-ext))
         ;; Construct the command line
         (commandline (dtache--output-command session))
         (dtach-mode (if (dtache--session-degraded session)
                         "-n"
                       dtache--dtach-mode)))
    (format "%s %s %s -z %s -c %s" dtache-program dtach-mode socket dtache-shell (shell-quote-argument commandline))))

(defun dtache-degraded-p (command)
  "Return t if COMMAND should run in degreaded mode."
  (if (thread-last dtache-degraded-list
        (seq-filter (lambda (regexp)
                      (string-match-p regexp command)))
        (length)
        (= 0))
      nil
    t))

(defun dtache-session-notify-command (session)
  "Append notify-send to SESSION's command."
  (let* ((command (dtache--session-command session))
         (emacs-icon
          (concat data-directory
                  "images/icons/hicolor/scalable/apps/emacs.svg")))
    (if (file-remote-p default-directory)
        command
      (concat
       command
       (format " && notify-send \"Dtache finished: %s\"" command)
       (format " --icon %s" emacs-icon)))))

(defun dtache-metadata ()
  "Return a property list with metadata."
  (let ((metadata '()))
    (seq-doseq (annotator dtache-metadata-annotators)
      (setq metadata (plist-put metadata (car annotator) (funcall (cdr annotator)))))
    metadata))

(defun dtache-encode-session (session)
  "Encode SESSION as a string."
  (let ((command
         (dtache--session-truncate-command session))
        (id
         (dtache--session-short-id session)))
    (concat
     command
     "  "
     (propertize id 'face 'font-lock-comment-face))))

(defun dtache-decode-session (candidate)
  "Return the session that match CANDIDATE."
  (cdr (assoc candidate dtache--session-candidates)))

(defun dtache-create-session-directory ()
  "Create session directory if it doesn't exist."
  (let ((directory
         (concat
          (file-remote-p default-directory)
          dtache-session-directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))))

;;;; Commands

;;;###autoload
(defun dtache-compile-session (session)
  "Open log of SESSION in `compilation-mode'."
  (interactive
   (list (dtache-select-session)))
  (let ((buffer-name
         (format "*dtache-compile-%s*"
                 (dtache--session-short-id session)))
        (file
         (dtache-session-file session 'log)))
    (when (file-exists-p file)
      (with-current-buffer (get-buffer-create buffer-name)
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (insert-file-contents file)
        (setq-local default-directory
                    (dtache--session-working-directory session))
        (compilation-mode))
      (pop-to-buffer buffer-name))))

;;;###autoload
(defun dtache-copy-session-log (session)
  "Copy SESSION's log."
  (interactive
   (list (dtache-select-session)))
  (dtache--file-content
   (dtache-session-file session 'log)))

;;;###autoload
(defun dtache-copy-session-command (session)
  "Copy SESSION command."
  (interactive
   (list (dtache-select-session)))
  (kill-new (dtache--session-command session)))

;;;###autoload
(defun dtache-insert-session-command (session)
  "Insert SESSION."
  (interactive
   (list (dtache-select-session)))
  (insert (dtache--session-command session)))

;;;###autoload
(defun dtache-remove-session (session)
  "Remove SESSION."
  (interactive
   (list (dtache-select-session)))
  (if (dtache--session-active-p session)
      (message "Kill session first before removing it.")
    (dtache--db-remove-session session)))

;;;###autoload
(defun dtache-kill-session (session)
  "Send a TERM signal to SESSION."
  (interactive
   (list (dtache-select-session)))
  (let* ((pids (flatten-list
                (dtache--session-child-pids
                 (dtache--session-pid session)))))
    (seq-doseq (pid pids)
      (apply #'process-file
             `("kill" nil nil nil ,pid)))))

;;;###autoload
(defun dtache-open-log (session)
  "Open SESSION's log."
  (interactive
   (list (dtache-select-session)))
  (dtache--open-file session 'log))

;;;###autoload
(defun dtache-tail-log (session)
  "Tail SESSION's log."
  (interactive
   (list (dtache-select-session)))
  (if (dtache--session-active-p session)
      (dtache--tail-file session 'log)
    (dtache--open-file session 'log)))

;;;###autoload
(defun dtache-open-stdout (session)
  "Open SESSION's stdout."
  (interactive
   (list (dtache-select-session)))
  (dtache--open-file session 'stdout))

;;;###autoload
(defun dtache-open-stderr (session)
  "Open SESSION's stderr."
  (interactive
   (list (dtache-select-session)))
  (dtache--open-file session 'stderr))

;;;###autoload
(defun dtache-attach-to-session (session)
  "Attach to SESSION."
  (interactive
   (list (dtache-select-session)))
  (if (dtache--session-active-p session)
      (dtache--attach-to-session session)
    (funcall dtache-attach-alternate-function session)))

;;;###autoload
(defun dtache-quit-tail-log ()
  "Quit `dtache' tail log.

The log can have been updated, but that is not done by the user but
rather the tail mode.  To avoid a promtp `buffer-modified-p' is set to
nil before closing."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer-and-window))

;;;; Support functions

;;;;; Session

(cl-defgeneric dtache--attach-to-session (session)
  "Attach to SESSION.")

(defun dtache--create-session (command)
  "Create a `dtache' session from COMMAND."
  (let ((session
         (dtache--session-create :id (dtache--create-id command)
                                 :command command
                                 :working-directory default-directory
                                 :degraded (dtache-degraded-p command)
                                 :creation-time (time-to-seconds (current-time))
                                 :session-directory (file-name-as-directory dtache-session-directory)
                                 :host (dtache--host)
                                 :metadata (dtache-metadata)
                                 :active t)))
    (dtache--db-insert-session session)
    session))

(defun dtache--session-pid (session)
  "Return SESSION's pid."
  (let* ((socket (concat
                  (dtache--session-session-directory session)
                  (dtache--session-id session)
                  dtache-socket-ext))
         (regexp (concat "dtach -c " socket))
         (ps-args '("aux" "-w")))
    (with-temp-buffer
      (apply #'process-file `("ps" nil t nil ,@ps-args))
      (buffer-substring-no-properties (point-min) (point-max))
      (goto-char (point-min))
      (search-forward-regexp regexp nil t)
      (elt (split-string (thing-at-point 'line) " " t) 1))))

(defun dtache--session-child-pids (pid)
  "Return a list of pids for all child processes including PID."
  (let ((pids `(,pid))
        (child-processes
         (split-string
          (shell-command-to-string (format "pgrep -P %s" pid))
          "\n" t)))
    (seq-do (lambda (pid)
              (push (dtache--session-child-pids pid) pids))
            child-processes)
    pids))

(defun dtache--session-truncate-command (session)
  "Return a truncated string representation of SESSION's command."
  (let ((command (dtache--session-command session))
        (part-length (- dtache-max-command-length 3)))
    (if (<= (length command) dtache-max-command-length)
        (let ((padding-length (- dtache-max-command-length (length command))))
          (concat command (make-string padding-length ?\s)))
      (concat
       (substring  command 0 (/ part-length 2))
       "..."
       (substring command (- (length command) (/ part-length 2)) (length command))))))

(defun dtache--session-update (session)
  "Update the `dtache' SESSION."
  (setf (dtache--session-active session) (dtache--session-active-p session))
  (setf (dtache--session-duration session) (dtache--duration session))
  (setf (dtache--session-log-size session) (file-attribute-size
                                            (file-attributes
                                             (dtache-session-file session 'log))))
  session)

(defun dtache--session-git-branch ()
  "Return current git branch."
  (let ((git-directory (locate-dominating-file "." ".git")))
    (when git-directory
      (let ((args '("name-rev" "--name-only" "HEAD")))
        (with-temp-buffer
          (apply #'process-file `("git" nil t nil ,@args))
          (string-trim (buffer-string)))))))

(defun dtache--session-short-id (session)
  "Return the short representation of the SESSION's id."
  (let ((id (dtache--session-id session)))
    (substring  id (- (length id) 8) (length id))))

(defun dtache--session-active-p (session)
  "Return t if SESSION is active."
  (file-exists-p
   (dtache-session-file session 'socket)))

(defun dtache--session-dead-p (session)
  "Return t if SESSION is dead."
  (not
   (file-exists-p
    (dtache-session-file session 'log))))

;;;;; Database

(defun dtache--db-insert-session (session)
  "Insert SESSION into the database."
  (let ((id (dtache--session-id session))
        (host (dtache--session-host session))
        (active (dtache--session-active session)))
    (emacsql dtache-db `[:insert
                         :into dtache-sessions
                         :values ([,id ,host ,active ,session])])))

(defun dtache--db-update-session (session)
  "Update the database with SESSION."
  (let ((id (dtache--session-id session)))
    (emacsql dtache-db [:update dtache-sessions
                        :set (= dtache-session $s2)
                        :where (= id $s1)]
             id session)
    (emacsql dtache-db [:update dtache-sessions
                        :set (= active $s2)
                        :where (= id $s1)]
             id (dtache--session-active session))))

(defun dtache--db-remove-session (session)
  "Remove SESSION from the database."
  (let ((id (dtache--session-id session)))
    (emacsql dtache-db [:delete
                        :from dtache-sessions
                        :where (= id $s1)]
             id)))

(defun dtache--db-select-session (id)
  "Return the session with ID from the database."
  (caar
   (emacsql dtache-db [:select dtache-session
                       :from dtache-sessions
                       :where (= id $s1)]
            id)))

(defun dtache--db-select-host-sessions (host)
  "Return all HOST sessions from the database."
  (let ((sessions
         (emacsql dtache-db
                  [:select dtache-session
                   :from dtache-sessions
                   :where (=  host $s1)]
                  host)))
    (seq-map #'car sessions)))

(defun dtache--db-select-active-sessions (host)
  "Return all active HOST sessions from the database."
  (let ((sessions
         (emacsql dtache-db
                  [:select dtache-session
                   :from dtache-sessions
                   :where (= host $s1) :and (=  active $s2)]
                  host t)))
    (seq-map #'car sessions)))

;;;;; Shell

(defun dtache--shell-comint-read-input-ring-a (orig-fun &rest args)
  "Set `comint-input-ring-file-name' before calling ORIG-FUN with ARGS."
  (with-connection-local-variables
   (let ((comint-input-ring-file-name
          (concat
           (file-remote-p default-directory)
           dtache-shell-history-file)))
     (apply orig-fun args)
     (advice-remove 'comint-read-input-ring #'dtache--shell-comint-read-input-ring-a))))

(defun dtache--shell-save-history ()
  "Save `shell' history."
  (with-connection-local-variables
   (let ((comint-input-ring-file-name
          (concat
           (file-remote-p default-directory)
           dtache-shell-history-file)))
     (comint-write-input-ring))))

;;;;; Other

(defun dtache--output-command (session)
  "Return output command for SESSION."
  (if (dtache--session-degraded session)
      (dtache--output-to-file-command session)
    (dtache--output-to-both-command session)))

(defun dtache--output-to-file-command (session)
  "Return a command to send SESSION's output directly to log."
  (let* ((command (dtache-session-command session))
         (directory (dtache--session-session-directory session))
         (file-name (dtache--session-id session))
         (log (concat directory file-name dtache-log-ext)))
    ;; Construct the command line
    ;;   echo &> log
    (format "{ %s; } &> %s" command log)))

(defun dtache--output-to-both-command (session)
  "Return a command to send SESSION's output to both shell and log."
  (let* ((command (dtache-session-command session))
         (directory (dtache--session-session-directory session))
         (file-name (dtache--session-id session))
         (stdout (concat directory file-name dtache-stdout-ext))
         (stderr (concat directory file-name dtache-stderr-ext))
         (log (concat directory file-name dtache-log-ext)))
    ;; Construct the command line
    ;;   { { echo stdout; echo stderr >&2; } >>(tee stdout ); } 2>>(tee stderr) | tee log
    (format "{ { %s; }%s }%s %s"
            (format "%s" command)
            (format " > >(tee %s );" stdout)
            (format " 2> >(tee %s )" stderr)
            (format " | tee %s" log))))

(defun dtache--host ()
  "Return name of host."
  (if-let ((remote-host (file-remote-p default-directory))
           (regexp (rx "/" (one-or-more alpha) ":" (group (regexp ".*")) ":")))
      (progn
        (string-match regexp remote-host)
        (match-string 1 remote-host))
    "localhost"))

(defun dtache--file-content (file)
  "Copy FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (kill-new (buffer-string))))

(defun dtache--duration (session)
  "Return the time duration of the SESSION.

Modification time is not reliable whilst a session is active.  Instead
the current time is used."
  ;; TODO: Consider calculating a time offset between host and remote
  ;; computer
  (if (dtache--session-active session)
      (- (time-to-seconds) (dtache--session-creation-time session))
    (- (time-to-seconds
        (file-attribute-modification-time
         (file-attributes
          (dtache-session-file session 'log))))
       (dtache--session-creation-time session))))

(defun dtache--open-file (session file)
  "Oen SESSION's FILE."
  (let* ((file-path
          (dtache-session-file session file)))
    (if (file-exists-p file-path)
        (progn
          (find-file-other-window file-path)
          (setq-local default-directory (dtache--session-working-directory session))
          (dtache-log-mode)
          (goto-char (point-max)))
      (message "Dtache can't find file: %s" file-path))))

(defun dtache--tail-file (session file)
  "Tail SESSION's FILE."
  (let* ((file-path
          (dtache-session-file session file)))
    (when (file-exists-p file-path)
      (find-file-other-window file-path)
      (dtache-tail-mode)
      (goto-char (point-max)))))

(defun dtache--create-id (command)
  "Return a hash identifier for COMMAND."
  (let ((current-time (current-time-string)))
    (secure-hash 'md5 (concat command current-time))))

;;;; Major mode

(defvar dtache-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-buffer-and-window)
    map)
  "Keymap for `dtache-log-mode'.")

(define-derived-mode dtache-log-mode nil "Dtache Log"
  "Major mode for dtache logs."
  (read-only-mode t))

(defvar dtache-tail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'dtache-quit-tail-log)
    map)
  "Keymap for `dtache-tail-mode'.")

(define-derived-mode dtache-tail-mode auto-revert-tail-mode "Dtache Tail"
  "Major mode for tailing dtache logs."
  (setq-local auto-revert-interval dtache-tail-interval)
  (auto-revert-set-timer)
  (setq-local auto-revert-verbose nil)
  (auto-revert-tail-mode)
  (read-only-mode t))

(provide 'dtache)

;;; dtache.el ends here
