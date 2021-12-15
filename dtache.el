;;; dtache.el --- Dispatch and interact with dtache sessions -*- lexical-binding: t -*-

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
;; that is isolated from Emacs.  This package provides functionality
;; for the user to launch detached commands with
;; `dtache-shell-command', which is inspired by `async-shell-command'.
;; Another function `dtache-start-session' is supposed to be used by
;; other functions or packages.  This is also useful if the user wants
;; to advice packages to use it in favor of for example `compile'.

;; To manage the sessions the user can either use
;; `dtache-list-sessions' for a tabulated list interface, or
;; `dtache-open-session' for a `completing-read' equivalent.

;; The package requires the program dtach[1] to be installed.
;;
;; [1] https://github.com/crigler/dtach

;;; Code:

;;;; Requirements

(require 'autorevert)
(require 'filenotify)
(require 'tramp)

;;;; Variables

(defvar dtache-session-directory nil
  "The directory to store `dtache' sessions.")
(defvar dtache-db-directory user-emacs-directory
  "The directory to store `dtache' database.")
(defvar dtache-dtach-program "dtach"
  "The name of the `dtach' program.")
(defvar dtache-shell-program "bash"
  "Shell to run the dtach command in.")
(defvar dtache-env "dtache-env"
  "The name of the `dtache' program.")
(defvar dtache-max-command-length nil
  "Maximum length of displayed command.")
(defvar dtache-redirect-only-regexps '()
  "Regexps for commands that should be run with redirect only.")
(defvar dtache-tail-interval 2
  "Interval in seconds for the update rate when tailing a session.")
(defvar dtache-session-type nil
  "Variable to specify the origin of the session.")
(defvar dtache-open-session-function nil
  "Custom function to use to open a session.")
(defvar dtache-session-callback-function nil
  "Custom function to callback when a session finish.")
(defvar dtache-session-status-function #'dtache-session-exit-code-status
  "Custom function to deduce the status of a session.")
(defvar dtache-compile-hooks nil
  "Hooks to run when compiling a session.")
(defvar dtache-metadata-annotators-alist nil
  "An alist of annotators for metadata.")

;;;;; Private

(defvar dtache--sessions-initialized nil
  "Sessions are initialized.")
(defvar dtache--dtach-mode nil
  "Mode of operation for dtach.")
(defvar dtache--sessions nil
  "A list of sessions.")
(defvar dtache--remote-session-timer nil
  "Timer object for remote polling.")

;;;; Data structures

(cl-defstruct (dtache-session (:constructor dtache--session-create)
                              (:conc-name dtache--session-))
  (id nil :read-only t)
  (command nil :read-only t)
  (type nil :read-only t)
  (open-function nil :read-only t)
  (callback-function nil :read-only t)
  (status-function nil :read-only t)
  (working-directory nil :read-only t)
  (creation-time nil :read-only t)
  (session-directory nil :read-only t)
  (metadata nil :read-only t)
  (host nil :read-only t)
  (redirect-only nil :read-only t)
  (status nil)
  (duration nil)
  (output-size nil)
  (active nil))

;;;; Commands

;;;###autoload
(defun dtache-shell-command (command)
  "Execute COMMAND asynchronously with `dtache'."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Dtache shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Dtache shell command: ")
                        nil nil)))
  (let* ((inhibit-message t)
         (dtache-session-type 'standard))
    (dtache-start-session command)))

;;;###autoload
(defun dtache-list-sessions ()
  "List `dtache' sessions."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create "*dtache-sessions*"))
  (dtache-sessions-mode)
  (dtache-update-sessions)
  (let* ((tabulated-list-entries
          (seq-map #'dtache-get-sesssion-entry dtache--sessions)))
    (tabulated-list-print t)))

;;;###autoload
(defun dtache-open-session (session)
  "Open a `dtache' SESSION."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (if-let ((open-function
            (dtache--session-open-function session)))
      (funcall open-function session)
    (dtache-open-dwim session)))

;;;###autoload
(defun dtache-compile-session (session)
  "Open log of SESSION in `compilation-mode'."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (let ((buffer-name
         (format "*dtache-compile-%s*"
                 (dtache--session-short-id session)))
        (file
         (dtache-session-file session 'log))
        (tramp-verbose 1))
    (when (file-exists-p file)
      (with-current-buffer (get-buffer-create buffer-name)
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (insert (dtache-session-output session))
        (setq-local default-directory
                    (dtache--session-working-directory session))
        (run-hooks 'dtache-compile-hooks)
        (dtache-log-mode)
        (compilation-minor-mode)
        (setq-local font-lock-defaults '(compilation-mode-font-lock-keywords t))
        (font-lock-mode)
        (read-only-mode))
      (pop-to-buffer buffer-name))))

;;;###autoload
(defun dtache-rerun-session (session)
  "Rerun SESSION."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (let* ((default-directory
           (dtache--session-working-directory session))
         (dtache-open-session-function
          (dtache--session-open-function session))
         (dtache-session-callback-function
          (dtache--session-callback-function session)))
    (dtache-start-session (dtache--session-command session))))

;;;###autoload
(defun dtache-copy-session-output (session)
  "Copy SESSION's log."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (with-temp-buffer
    (insert (dtache-session-output session))
    (kill-new (buffer-string))))

;;;###autoload
(defun dtache-copy-session-command (session)
  "Copy SESSION command."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (kill-new (dtache--session-command session)))

;;;###autoload
(defun dtache-insert-session-command (session)
  "Insert SESSION."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (insert (dtache--session-command session)))

;;;###autoload
(defun dtache-remove-session (session)
  "Remove SESSION."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (if (dtache--session-active-p session)
      (message "Kill session first before removing it.")
    (dtache--db-remove-session session)))

;;;###autoload
(defun dtache-kill-session (session)
  "Send a TERM signal to SESSION."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (let* ((pid (dtache--session-pid session)))
    (when pid
      (dtache--kill-processes pid))))

;;;###autoload
(defun dtache-open-output (session)
  "Open SESSION's output."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (let* ((buffer-name
          (format "*dtache-output-%s*"
                  (dtache--session-short-id session)))
         (file-path
          (dtache-session-file session 'log))
         (tramp-verbose 1))
    (if (file-exists-p file-path)
        (progn
          (with-current-buffer (get-buffer-create buffer-name)
            (insert (dtache-session-output session))
            (setq-local default-directory (dtache--session-working-directory session))
            (dtache-log-mode)
            (goto-char (point-max)))
          (pop-to-buffer buffer-name))
      (message "Dtache can't find file: %s" file-path))))

;;;###autoload
(defun dtache-tail-output (session)
  "Tail SESSION's output."
  (interactive
   (list (if (eq major-mode 'dtache-sessions-mode)
             (tabulated-list-get-id)
           (dtache-select-session))))
  (if (dtache--session-active-p session)
      (let* ((file-path
              (dtache-session-file session 'log))
             (tramp-verbose 1))
        (when (file-exists-p file-path)
          (find-file-other-window file-path)
          (dtache-tail-mode)
          (goto-char (point-max))))
    (dtache-open-output session)))

;;;###autoload
(defun dtache-diff-session (session1 session2)
  "Diff SESSION1 with SESSION2."
  (interactive
   (list
    (dtache-select-session)
    (dtache-select-session)))
  (dtache--create-diff-buffer session1)
  (dtache--create-diff-buffer session2)
  (let ((buffer1 (format "*dtache-diff-%s*"
                         (dtache--session-short-id session1)))
        (buffer2 (format "*dtache-diff-%s*"
                         (dtache--session-short-id session2))))
    (ediff-buffers buffer1 buffer2)))

;;;###autoload
(defun dtache-quit-tail-output ()
  "Quit `dtache' tail log.

The log can have been updated, but that is not done by the user but
rather the tail mode.  To avoid a promtp `buffer-modified-p' is set to
nil before closing."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer-and-window))

;;;; Functions

;;;;; Session

(defun dtache-start-session (command)
  "Start a `dtache' session running COMMAND."
  (let* ((dtache--dtach-mode 'new)
         (session (dtache--create-session command))
         (dtache-command (dtache-dtach-command session)))
    (dtache-setup-notification session)
    (apply #'start-file-process
           `("dtache" nil ,dtache-dtach-program ,@dtache-command))))

(defun dtache-select-session ()
  "Return selected session."
  (dtache-update-sessions)
  (dtache-completing-read dtache--sessions))

(defun dtache-update-sessions ()
  "Update `dtache' sessions.

Sessions running on  current host or localhost are updated."
  (let ((current-host (dtache--host))
        (updated-sessions))
    (setq updated-sessions
          (seq-map (lambda (it)
                     (if (and (or (string= current-host (dtache--session-host it))
                                  (string= "localhost" (dtache--session-host it)))
                              (or (dtache--session-active it)
                                  (dtache--session-deactivated-p it)))
                         (dtache-update-session it)
                       it))
                   dtache--sessions))
    (dtache--db-update-sessions updated-sessions)))

(defun dtache-session-file (session file)
  "Return the path to SESSION's FILE."
  (let ((file-name
         (concat
          (dtache--session-id session)
          (pcase file
            ('socket ".socket")
            ('log ".log"))))
        (directory (concat
                    (file-remote-p (dtache--session-working-directory session))
                    (dtache--session-session-directory session))))
    (expand-file-name file-name directory)))

(defun dtache-session-candidates (sessions)
  "Return an alist of SESSIONS candidates."
  (seq-map (lambda (it)
             (let ((s (format #("%s\0%s" 2 5 (invisible t))
                              (dtache--session-truncate-command  it)
                              (dtache--session-short-id it))))
               (prog1 s (put-text-property 0 1 'dtache--data it s))))
           sessions))

(defun dtache-update-session (session)
  "Update SESSION."
  (when (dtache--session-deactivated-p session)
    (progn
      (setf (dtache--session-active session) nil)
      (setf (dtache--session-duration session)
            (dtache--duration session))
      (when-let ((status (dtache--session-status-function session)))
        (setf (dtache--session-status session) (funcall status session)))
      (dtache-session-finish-notification session)
      (when-let ((callback (dtache--session-callback-function session)))
        (funcall callback session))))
  (setf (dtache--session-output-size session)
        (file-attribute-size (file-attributes
                              (dtache-session-file session 'log))))
  session)

(defun dtache-initialize ()
  "Initialize `dtache'."

  ;; Initialize sessions
  (unless dtache--sessions-initialized
    (unless (file-exists-p dtache-db-directory)
      (make-directory dtache-db-directory t))

    (setq dtache--sessions
          (thread-last (dtache--db-select-sessions)
                       ;; Remove missing local sessions
                       (seq-remove (lambda (it)
                                     (and (string= "localhost" (dtache--session-host it))
                                          (dtache--session-missing-p it))))
                       ;; Update local active sessions
                       (seq-map (lambda (it)
                                  (if (and (string= "localhost" (dtache--session-host it))
                                           (dtache--session-active it))
                                      (dtache-update-session it)
                                    it)))))

    ;; Setup notifications
    (thread-last dtache--sessions
      (seq-filter #'dtache--session-active)
      (seq-do #'dtache-setup-notification))))

(defun dtache-update-remote-sessions ()
  "Update active remote sessions."
  (let ((predicate
         (lambda (s) (and (not (string= "localhost" (dtache--session-host s)))
                     (dtache--session-active s)))))

    ;; Update sessions
    (thread-last dtache--sessions
      (seq-map (lambda (it)
                 (if (funcall predicate it)
                     (dtache-update-session it)
                   it)))
      (dtache--db-update-sessions))

    ;; Cancel timer if no active remote sessions
    (unless (> (seq-count predicate dtache--sessions) 0)
      (cancel-timer dtache--remote-session-timer)
      (setq dtache--remote-session-timer nil))))

(defun dtache-cleanup-host-sessions (host)
  "Run cleanuup on HOST sessions."
  (dtache--db-update-sessions
   (seq-remove
    (lambda (it)
      (and (string= host (dtache--session-host it))
           (dtache--session-missing-p it)))
    dtache--sessions)))

(defun dtache-session-exit-code-status (session)
  "Return status based on exit-code in SESSION."
  (with-temp-buffer
    (insert-file-contents (dtache-session-file session 'log))
    (goto-char (point-max))
    (if (string-match "Dtache session finished" (thing-at-point 'line t))
        'success
      'failure)))

(defun dtache-session-output (session)
  "Return content of SESSION's output."
  (let* ((filename (dtache-session-file session 'log))
         (status (dtache--session-status session))
         (remove-dtache-message (not (eq status 'unknown))))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-max))
      (when remove-dtache-message
        (line-move -3)
        (end-of-line))
      (buffer-substring (point-min) (point)))))

(defun dtache-session-finish-notification (session)
  "Send a notification when SESSION finish."
  (let ((status (pcase (dtache--session-status session)
                  ('success "Dtache finished")
                  ('failure "Dtache failed")) ))
    (message "%s: %s" status (dtache--session-command session))))

(defun dtache-open-dwim (session)
  "Open SESSION in a do what I mean fashion."
  (cond ((dtache--session-active session)
         (dtache-tail-output session))
        ((eq 'success (dtache--session-status session))
         (dtache-open-output session))
        ((eq 'failure (dtache--session-status session))
         (dtache-compile-session session))
        ;; TODO: Inactive sessions should never have status unknown,
        ;; need to investigate why that happens
        (t (progn (message "Unknown status of session.")
                  (dtache-open-output session)))))

;;;;; Other

(defun dtache-completing-read (sessions)
  "Select a session from SESSIONS through `completing-read'."
  (let* ((candidates (dtache-session-candidates sessions))
         (metadata '(metadata
                     (category . dtache)
                     (cycle-sort-function . identity)
                     (display-sort-function . identity)))
         (coll (lambda (string predicate action)
                 (if (eq action 'metadata)
                     metadata
                   (complete-with-action action candidates string predicate))))
         (cand (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook 'after-change-functions 'dtache--eat-cookie nil t))
                 (completing-read "Select session: " coll nil t nil
                                  'dtache-session-history))))
    (get-text-property 0 'dtache--data (car (member cand candidates)))))

(defun dtache-setup-notification (session)
  "Setup notification for SESSION."
  (if (file-remote-p default-directory)
      (dtache--create-remote-session-timer)
    (dtache--add-end-of-session-notification session)))

(defun dtache-dtach-command (session)
  "Return a dtach command for SESSION."
  (with-connection-local-variables
   (let* ((directory (dtache--session-session-directory session))
          (file-name (dtache--session-id session))
          (socket (concat directory file-name ".socket"))
          ;; Construct the command line
          (command (dtache--magic-command session))
          (dtache--dtach-mode (if (dtache--session-redirect-only session)
                                  'new
                                dtache--dtach-mode)))
     `(,(dtache--dtach-arg) ,socket "-z" ,dtache-shell-program "-c" ,command))))

(defun dtache-redirect-only-p (command)
  "Return t if COMMAND should run in degreaded mode."
  (if (thread-last dtache-redirect-only-regexps
        (seq-filter (lambda (regexp)
                      (string-match-p regexp command)))
        (length)
        (= 0))
      nil
    t))

(defun dtache-metadata ()
  "Return a property list with metadata."
  (let ((metadata '()))
    (seq-doseq (annotator dtache-metadata-annotators-alist)
      (push `(,(car annotator) . ,(funcall (cdr annotator))) metadata))
    metadata))

(defun dtache-create-session-directory ()
  "Create session directory if it doesn't exist."
  (let ((directory
         (concat
          (file-remote-p default-directory)
          dtache-session-directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))))

(defun dtache-get-working-directory ()
  "Return an abreviated working directory path."
  (let* ((remote (file-remote-p default-directory))
         (full-home (if remote (expand-file-name remote) (expand-file-name "~")))
         (short-home (if remote (concat remote "~/") "~")))
    (replace-regexp-in-string full-home
                              short-home
                              (expand-file-name default-directory))))

;;;; Support functions

;;;;; Session

(defun dtache--create-session (command)
  "Create a `dtache' session from COMMAND."
  (dtache-create-session-directory)
  (let ((session
         (dtache--session-create :id (dtache--create-id command)
                                 :command command
                                 :type dtache-session-type
                                 :open-function dtache-open-session-function
                                 :callback-function dtache-session-callback-function
                                 :status-function dtache-session-status-function
                                 :working-directory (dtache-get-working-directory)
                                 :redirect-only (dtache-redirect-only-p command)
                                 :creation-time (time-to-seconds (current-time))
                                 :status 'unknown
                                 :output-size 0
                                 :session-directory (file-name-as-directory dtache-session-directory)
                                 :host (dtache--host)
                                 :metadata (dtache-metadata)
                                 :active t)))
    ;; Update list of sessions
    (push session dtache--sessions)
    ;; Update database
    (dtache--db-update-sessions dtache--sessions)
    session))

(defun dtache--session-pid (session)
  "Return SESSION's pid."
  (let* ((socket
          (concat
           (dtache--session-session-directory session)
           (dtache--session-id session)
           ".socket"))
         (regexp (rx-to-string `(and "dtach " (or "-n " "-c ") ,socket)))
         (ps-args '("aux" "-w")))
    (with-temp-buffer
      (apply #'process-file `("ps" nil t nil ,@ps-args))
      (goto-char (point-min))
      (when (search-forward-regexp regexp nil t)
        (elt (split-string (thing-at-point 'line) " " t) 1)))))

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
  (if (null dtache-max-command-length)
      (dtache--session-command session)
    (let ((command (dtache--session-command session))
          (part-length (- dtache-max-command-length 3)))
      (if (<= (length command) dtache-max-command-length)
          (let ((padding-length (- dtache-max-command-length (length command))))
            (concat command (make-string padding-length ?\s)))
        (concat
         (substring command 0 (/ part-length 2))
         "..."
         (substring command (- (length command) (/ part-length 2)) (length command)))))))

(defun dtache--session-update (session)
  "Update the `dtache' SESSION."
  (setf (dtache--session-active session) (dtache--session-active-p session))
  (setf (dtache--session-output-size session) (file-attribute-size
                                            (file-attributes
                                             (dtache-session-file session 'log))))
  session)

(defun dtache--session-short-id (session)
  "Return the short representation of the SESSION's id."
  (let ((id (dtache--session-id session)))
    (substring  id (- (length id) 8) (length id))))

(defun dtache--session-active-p (session)
  "Return t if SESSION is active."
  (file-exists-p
   (dtache-session-file session 'socket)))

(defun dtache--session-deactivated-p (session)
  "Return t if SESSION has been deactivated."
  (and
   (dtache--session-active session)
   (not (file-exists-p (dtache-session-file session 'socket)))))

(defun dtache--session-missing-p (session)
  "Return t if SESSION is missing."
  (not
   (file-exists-p
    (dtache-session-file session 'log))))

(defun dtache--create-remote-session-timer ()
  "Create a new remote session and trigger timer."
  (unless dtache--remote-session-timer
    (setq dtache--remote-session-timer
          (run-with-timer 10 60 #'dtache-update-remote-sessions))))

(defun dtache--create-diff-buffer (session)
  "Create a diff buffer for SESSION."
  (let ((buffer-name
         (format "*dtache-diff-%s*"
                 (dtache--session-short-id session))))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "Command: %s\n" (dtache--session-command session)))
      (insert (format "Working directory: %s\n" (dtache--working-dir-str session)))
      (insert (format "Status: %s\n" (dtache--session-status session)))
      (insert (format "Created at: %s\n" (dtache--creation-str session)))
      (insert (format "Id: %s\n" (dtache--session-id session)))
      (insert (format "Metadata: %s\n" (dtache--metadata-str session)))
      (insert (format "Duration: %s\n" (dtache--duration-str session)))
      (insert "\n")
      (insert (dtache-session-output session)))))

;;;;; Database

(defun dtache--db-select-sessions ()
  "Return all sessions stored in database."
  (let ((db (expand-file-name "dtache.db" dtache-db-directory)))
    (when (file-exists-p db)
      (with-temp-buffer
        (insert-file-contents db)
        (cl-assert (eq (point) (point-min)))
        (read (current-buffer))))))

(defun dtache--db-remove-session (session)
  "Remove SESSION from database."
  (let ((id (dtache--session-id session)))
    (setq dtache--sessions
          (seq-remove (lambda (it)
                        (string= id (dtache--session-id it)))
                      dtache--sessions))
    (dtache--db-update-sessions dtache--sessions)))

(defun dtache--db-update-session (session)
  "Update SESSION in database."
  (let ((id (dtache--session-id session)))
    (setq dtache--sessions
          (seq-map (lambda (it)
                     (if (string= (dtache--session-id it) id)
                         session
                       it))
                   dtache--sessions))
    (dtache--db-update-sessions dtache--sessions)))

(defun dtache--db-update-sessions (sessions)
  "Write SESSIONS to database."
  (setq dtache--sessions sessions)
  (let ((db (expand-file-name "dtache.db" dtache-db-directory)))
    (with-temp-file db
      (prin1 dtache--sessions (current-buffer)))))

;;;;; Other

(defun dtache--dtach-arg ()
  "Return dtach argument based on mode."
  (pcase dtache--dtach-mode
    ('new "-n")
    ('create "-c")
    ('attach "-a")
    (_ "-n")))

(defun dtache--add-end-of-session-notification (session)
  "Trigger an event when SESSION is stopped."
  (file-notify-add-watch
   (dtache-session-file session 'socket)
   '(change)
   (lambda (event)
     (pcase-let ((`(,_ ,action ,_) event))
       (when (eq action 'deleted)
         ;; Update session
         (setf (dtache--session-output-size session) (file-attribute-size
                                                   (file-attributes
                                                    (dtache-session-file session 'log))))

         (setf (dtache--session-active session) nil)
         (setf (dtache--session-duration session)
               (- (time-to-seconds) (dtache--session-creation-time session)))

         ;; Update session in database
         (dtache--db-update-session session)

         ;; Update status
         (when-let ((status (dtache--session-status-function session)))
           (setf (dtache--session-status session) (funcall status session)))

         ;; Send notification
         (dtache-session-finish-notification session)

         ;; Execute callback
         (when-let ((callback (dtache--session-callback-function session)))
           (funcall callback session)))))))

(defun dtache--eat-cookie (&rest _)
  "Eat the disambiguation cookie in the minibuffer."
  (let* ((pos (minibuffer-prompt-end))
         (max (point-max)))
    (while (and (< pos max) (/= 0 (char-after pos)))
      (setq pos (1+ pos)))
    (when (< pos max)
      (add-text-properties pos (next-property-change pos nil max)
                           '(invisible t rear-nonsticky t)))))

(defun dtache--kill-processes (pid)
  "Kill PID and all of its children."
  (let ((child-processes
         (split-string
          (shell-command-to-string (format "pgrep -P %s" pid))
          "\n" t)))
    (seq-do (lambda (pid) (dtache--kill-processes pid)) child-processes)
    (apply #'process-file `("kill" nil nil nil ,pid))))

(defun dtache--magic-command (session)
  "Return the magic dtache command for SESSION.

If SESSION is redirect-only fallback to a command that doesn't rely on tee.
Otherwise use tee to log stdout and stderr individually."
  (let* ((command (string-join
                   `(,dtache-env
                     ,dtache-shell-program "-c" "-i"
                     ,(shell-quote-argument (format "\"%s\"" (dtache--session-command session)))) " "))
         (directory (dtache--session-session-directory session))
         (file-name (dtache--session-id session))
         (log (concat directory file-name ".log")))
    (if (dtache--session-redirect-only session)
        (format "{ %s; } &> %s" command log)
      (format "{ %s; } 2>&1 | tee %s" command log))))

(defun dtache--host ()
  "Return name of host."
  (or
   (file-remote-p default-directory 'host)
   "localhost"))

(defun dtache--duration (session)
  "Return the time duration of the SESSION.

Modification time is not reliable whilst a session is active.  Instead
the current time is used."
  (- (time-to-seconds
      (file-attribute-modification-time
       (file-attributes
        (dtache-session-file session 'log))))
     (dtache--session-creation-time session)))

(defun dtache--create-id (command)
  "Return a hash identifier for COMMAND."
  (let ((current-time (current-time-string)))
    (secure-hash 'md5 (concat command current-time))))

;;;;; UI

(defun dtache--metadata-str (session)
  "Return SESSION's metadata as a string."
  (string-join
   (thread-last (dtache--session-metadata session)
     (seq-filter (lambda (it) (cdr it)))
     (seq-map
      (lambda (it)
        (concat (symbol-name (car it)) ": " (cdr it)))))
   " "))

(defun dtache--duration-str (session)
  "Return SESSION's duration time."
  (let* ((time
          (round (if (dtache--session-active session)
                     (- (time-to-seconds) (dtache--session-creation-time session))
                   (dtache--session-duration session))))
         (hours (/ time 3600))
         (minutes (/ (mod time 3600) 60))
         (seconds (mod time 60)))
    (cond ((> time (* 60 60)) (format "%sh %sm %ss" hours minutes seconds))
          ((> time 60) (format "%sm %ss" minutes seconds))
          (t (format "%ss" seconds)))))

(defun dtache--creation-str (session)
  "Return SESSION's creation time."
  (format-time-string
   "%b %d %H:%M"
   (dtache--session-creation-time session)))

(defun dtache--size-str (session)
  "Return the size of SESSION's output."
  (file-size-human-readable
   (dtache--session-output-size session)))

(defun dtache--status-str (session)
  "Return string if SESSION has failed."
  (pcase (dtache--session-status session)
    ('failure "!")
    ('success " ")
    ('unknown " ")))

(defun dtache--active-str (session)
  "Return string if SESSION is active."
  (if (dtache--session-active session)
      "*"
    " "))

(defun dtache--working-dir-str (session)
  "Return working directory of SESSION."
  (let ((working-directory
         (dtache--session-working-directory session)))
    (if-let ((remote (file-remote-p working-directory)))
        (string-remove-prefix remote working-directory)
        working-directory)))

;;;; Major modes

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
    (define-key map (kbd "q") #'dtache-quit-tail-output)
    map)
  "Keymap for `dtache-tail-mode'.")

(define-derived-mode dtache-tail-mode auto-revert-tail-mode "Dtache Tail"
  "Major mode for tailing dtache logs."
  (setq-local auto-revert-interval dtache-tail-interval)
  (setq-local tramp-verbose 1)
  (setq-local auto-revert-remote-files t)
  (defvar revert-buffer-preserve-modes)
  (setq-local revert-buffer-preserve-modes nil)
  (auto-revert-set-timer)
  (setq-local auto-revert-verbose nil)
  (auto-revert-tail-mode)
  (read-only-mode t))

;;;; Tabulated list interface

(define-derived-mode dtache-sessions-mode tabulated-list-mode "Dtache Sessions"
  "Dtache sessions."
  (setq tabulated-list-format
        `[("Command" ,(or dtache-max-command-length 50) nil)
          ("Active" 10 nil)
          ("Status" 10 nil)
          ("Host" 20 nil)
          ("Directory" 40 nil)
          ("Metadata" 30 nil)
          ("Duration" 10 nil)
          ("Size" 10 nil)
          ("Created" 20 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun dtache-get-sesssion-entry (session)
  "Return expected format of SESSION."
  `(,session
    [,(dtache--session-command session)
     ,(dtache--active-str session)
     ,(dtache--status-str session)
     ,(dtache--session-host session)
     ,(dtache--working-dir-str session)
     ,(dtache--metadata-str session)
     ,(dtache--duration-str session)
     ,(dtache--size-str session)
     ,(dtache--creation-str session)]))

(let ((map dtache-sessions-mode-map))
  (define-key map (kbd "<return>") #'dtache-open-session)
  (define-key map (kbd "c") #'dtache-compile-session)
  (define-key map (kbd "d") #'dtache-remove-session)
  (define-key map (kbd "k") #'dtache-kill-session)
  (define-key map (kbd "o") #'dtache-open-output)
  (define-key map (kbd "r") #'dtache-rerun-session)
  (define-key map (kbd "t") #'dtache-tail-output)
  (define-key map (kbd "w") #'dtache-copy-session-command)
  (define-key map (kbd "W") #'dtache-copy-session-output))

(provide 'dtache)

;;; dtache.el ends here
