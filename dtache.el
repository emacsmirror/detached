;;; dtache.el --- Core dtache -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://www.gitlab.com/niklaseklund/dtache.git
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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

;; This package provides a backend implementation for dtache
;; sessions.  Dtache is supposed to be interfaced through other
;; packages, such a package is `dtache-shell' which brings dtache into
;; shell buffers.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'comint)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'f)
(require 'projectile)

;;;; Variables

(defvar dtache-session-directory nil
  "The directory to store `dtache' sessions.")
(defvar dtache-db-directory user-emacs-directory
  "The directory to store `dtache' database.")
(defvar dtache-db nil
  "The connection to the `dtache' database.")
(defconst dtache-program "dtach"
  "The `dtach' program.")

(defconst dtache-shell "bash"
  "Shell to run the dtach command in.")
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

(defvar dtache-block-list '("^$" "^cd.*" "^mkdir.*" "^touch.*" "^alias.*")
  "A list of regexps that are blocked and should not be sent to `dtache'.")
(defvar dtache-max-command-length 95
  "Maximum length of displayed command.")

;;;;; Private

(defvar dtache--sessions nil "A list of the current sessions.")

;;;; Data structures

(cl-defstruct (dtache-session (:constructor dtache--session-create)
                              (:conc-name dtache--session-))
  (id nil :read-only t)
  (command nil :read-only t)
  (directory nil :read-only t)
  (creation-time nil :read-only t)
  (git nil :read-only t)
  (duration nil)
  (log-size nil)
  (stderr-p nil)
  (active nil))

;;;; Interfaces

(cl-defgeneric dtache-attach (session)
  "A context aware function to attach to SESSION.")

(cl-defmethod dtache-attach (session &context (major-mode shell-mode))
  "Attach to a dtache SESSION when MAJOR-MODE is `shell-mode'.

`comint-add-to-input-history' is temporarily disabled to avoid
cluttering the comint-history with dtach commands."
  (unless (process-running-child-p (get-process (buffer-name)))
    (let* ((socket (dtache--session-file session 'socket))
	       (input (concat dtache-program " -a " (shell-quote-argument socket))))
      (goto-char (point-max))
      (insert input)
      (cl-letf (((symbol-function 'comint-add-to-input-history)
	             (lambda (_) t)))
        (comint-send-input)))))

;;;; Commands

;;;###autoload
(defun dtache-compile-session (session)
  "Open log of SESSION in `compilation-mode'."
  (interactive
   (list (dtache-select-session)))
  (let ((buffer-name
         (format "*dtache-compile-%s*"
                 (dtache--session-short-id session))))
    (when (f-exists-p (dtache--session-file session 'log))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (insert-file-contents (dtache--session-file session 'log))
        (setq-local default-directory (dtache--session-directory session))
        (compilation-mode))
      (pop-to-buffer buffer-name))))

;;;###autoload
(defun dtache-copy-session-content (session)
  "Copy content of SESSION."
  (interactive
   (list (dtache-select-session)))
  (dtache--file-content (dtache--session-file session 'log)))

;;;###autoload
(defun dtache-copy-session (session)
  "Copy SESSION."
  (interactive
   (list (dtache-select-session)))
  (kill-new (dtache--session-command session)))

;;;###autoload
(defun dtache-insert-session (session)
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
  (if (not (dtache--session-active-p session))
      (message "Session is already inactive.")
    (let* ((default-directory (dtache--session-directory session))
           (process-group (prin1-to-string (dtache--session-process-group session))))
      (shell-command (format "kill -- -%s" process-group)))))

;;;###autoload
(defun dtache-open-log (session)
  "Open SESSION's log."
  (interactive
   (list (dtache-select-session)))
  (dtache--open-file session 'log))

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
      (dtache-attach session)
    (dtache-open-log session)))

;;;; Functions

(defun dtache-setup ()
  "Setup `dtache'."
  ;; Safety first
  (unless (executable-find "dtach")
    (error "`dtache' requires program `dtach' to be installed"))
  (unless dtache-session-directory
    (error "`dtache-session-directory' must be configured"))
  (unless dtache-db-directory
    (error "`dtache-db-directory' must be configured"))
  ;; Setup
  (unless (file-exists-p dtache-session-directory)
    (make-directory dtache-session-directory t))
  (unless (file-exists-p dtache-db-directory)
    (make-directory dtache-db-directory t))
  (dtache-db-initialize))

;;;;; Database

(defun dtache-db-initialize ()
  "Initialize the `dtache' database."
  (setq dtache-db
        (emacsql-sqlite
         (expand-file-name "dtache.db" dtache-db-directory)))
  (emacsql dtache-db
           [:create-table
            :if :not :exists dtache-sessions
            ([(id text :primary-key) dtache-session])]))

(defun dtache-db-reinitialize ()
  "Reinitialize the `dtache' database."
  (let ((db-alive
         (and (emacsql-sqlite-connection-p dtache-db)
              (emacsql-live-p dtache-db))))
    (when db-alive
      (emacsql-close dtache-db))
    (dtache-db-initialize)))

;;;;; Sessions

(defun dtache-select-session ()
  "Return selected session."
  (let* ((sessions (dtache--sessions))
         (selected
          (completing-read "Select session: "
                           (lambda (str pred action)
                             (pcase action
                               ('metadata '(metadata (category . dtache)
                                                     (cycle-sort-function . identity)
                                                     (display-sort-function . identity)))
                               (`(boundaries . ,_) nil)
                               ('nil (try-completion str sessions pred))
                               ('t (all-completions str sessions pred))
                               (_ (test-completion str sessions pred))))
                           nil t nil 'dtache-session-history)))
    (dtache-session-decode selected)))

(defun dtache-update-sessions ()
  "Update sessions in the database."
  (let ((sessions (dtache--db-select-sessions)))
    (-some->> sessions
      (-filter #'dtache--session-active)
      (-map #'dtache-session--update)
      (-map #'dtache--db-update-session))))

(defun dtache-cleanup-sessions ()
  "Remove dead sessions from the database."
  (let ((sessions (dtache--db-select-sessions)))
    (-some->> sessions
      (-filter #'dtache--session-dead-p)
      (-map #'dtache--db-remove-session))))

(defun dtache-session-command (session)
  "Return a dtach command for SESSION."
  (let* ((command (dtache--session-command session))
         (stdout (dtache--session-file session 'stdout))
         (stderr (dtache--session-file session 'stderr))
         (stdout+stderr (dtache--session-file session 'log))
         (socket (dtache--session-file session 'socket))
         ;; Construct the command line
         ;;   { { echo stdout; echo stderr >&2; } >>(tee stdout ); } 2>>(tee stderr) | tee log
         (commandline (format "{ { %s; }%s }%s %s"
                              (format "%s" command)
                              (format " > >(tee %s );" stdout)
                              (format " 2> >(tee %s )" stderr)
                              (format " | tee %s" stdout+stderr))))
    (format "%s -c %s -z %s -c %s" dtache-program socket dtache-shell (shell-quote-argument commandline))))

(defun dtache-create-session (command)
  "Create a `dtache' session from COMMAND."
  (let ((session
         (dtache--session-create :id (dtache--create-id command)
                                 :command command
                                 :directory default-directory
                                 :creation-time (time-to-seconds (current-time))
                                 :git (dtache--session-git-info)
                                 :active t)))
    (dtache--db-insert-session session)
    session))

;;;;; String representations

(defun dtache-session-encode (session)
  "Encode SESSION as a string."
  (let ((command
         (dtache--session-truncate-command session))
        (hash
         (dtache--session-short-id session)))
    (s-concat
     command
     "  "
     (propertize hash 'face 'font-lock-comment-face))))

(defun dtache-session-decode (str)
  "Decode STR to a session."
  (cdr (assoc str dtache--sessions)))

;;;; Support functions

(defun dtache--command-string (session)
  "Return SESSION's command as a string."
  (let ((command (dtache--session-command session)))
    (if (< (length command) dtache-max-command-length)
      (s-pad-right dtache-max-command-length " " command)
      (s-concat
       (s-truncate (/ dtache-max-command-length 2) command)
       (s-right (/ dtache-max-command-length 2) command)))))

(defun dtache--session-truncate-command (session)
  "Return a truncated string representation of SESSION's command."
  (let ((command (dtache--session-command session))
        (part-length (- dtache-max-command-length 3)))
    (if (<= (length command) dtache-max-command-length)
        (s-pad-right dtache-max-command-length " " command)
      (s-concat
       (s-left  (/ part-length 2) command)
       "..."
       (s-right (/ part-length 2) command)))))

(defun dtache-session--update (session)
  "Update the `dtache' SESSION."
  (setf (dtache--session-active session) (dtache--session-active-p session))
  (setf (dtache--session-duration session) (dtache--duration session))
  (setf (dtache--session-log-size session) (file-attribute-size
                                            (file-attributes
                                             (dtache--session-file session 'log))))
  (setf (dtache--session-stderr-p session) (> (file-attribute-size
                                               (file-attributes
                                                (dtache--session-file session 'stderr))) 0))
  session)

(defun dtache--session-git-info ()
  "Return current git branch."
  (let ((git-directory (locate-dominating-file "." ".git")))
    (when git-directory
      (let ((args '("name-rev" "--name-only" "HEAD")))
        (with-temp-buffer
          (apply #'call-process `("git" nil t nil ,@args))
          (s-trim (buffer-string)))))))

(defun dtache--file-content (file)
  "Copy FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (kill-new (buffer-string))))

(defun dtache--sessions ()
  "Return an alist of sessions."
  (dtache-update-sessions)
  (let ((sessions (nreverse (dtache--db-select-sessions))))
    (setq dtache--sessions
          (--map `(,(dtache-session-encode it) . ,it) sessions))))

(defun dtache--duration (session)
  "Return the time duration of the SESSION.

Modification time is not reliable whilst a session is active.  Instead
the current time is used."
  (if (dtache--session-active session)
      (- (time-to-seconds) (dtache--session-creation-time session))
    (- (time-to-seconds
        (file-attribute-modification-time
         (file-attributes
          (dtache--session-file session 'log))))
       (dtache--session-creation-time session))))

(defun dtache--open-file (session file)
  "Oen SESSION's FILE."
  (let ((buffer-name (format "*dtache-%s-%s*" file
                             (dtache--session-short-id session))))
    (when (f-exists-p (dtache--session-file session file))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert-file-contents (dtache--session-file session file))
        (setq-local default-directory (dtache--session-directory session)))
      (pop-to-buffer buffer-name)
      (dtache-log-mode))))

(defun dtache--create-id (command)
  "Return a hash identifier for COMMAND."
  (let ((current-time (current-time-string)))
    (secure-hash 'md5 (concat  command current-time))))

;;;;; Sessions

(defun dtache--session-short-id (session)
  "Return the short representation of the SESSION's id."
  (s-right 8 (dtache--session-id session)))

(defun dtache--session-active-p (session)
  "Return t if SESSION is active."
  (f-exists-p (dtache--session-file session 'socket)))

(defun dtache--session-dead-p (session)
  "Return t if SESSION is dead."
  (not (f-exists-p (dtache--session-file session 'log))))

(defun dtache--session-process-group (session)
  "Return the process id for SESSION."
  (let* ((socket (f-filename (dtache--session-file session 'socket))))
    (-find (lambda (process)
             (let-alist (process-attributes process)
               (when (s-matches-p socket .args)
                 .pgrp)))
           (list-system-processes))))

(defun dtache--session-file (session file)
  "Return path to SESSION's FILE."
  (expand-file-name
   (concat (dtache--session-id session)
           (dtache--session-extension file))
   dtache-session-directory))

(defun dtache--session-extension (file)
  "Return extensions of FILE."
  (pcase file
    ('socket dtache-socket-ext)
    ('log dtache-log-ext)
    ('stdout dtache-stdout-ext)
    ('stderr dtache-stderr-ext)))

;;;;; Database

(defun dtache--db-insert-session (session)
  "Insert SESSION into the database."
  (let ((id (dtache--session-id session)))
    (emacsql dtache-db `[:insert
                         :into dtache-sessions
                         :values ([,id ,session])])))

(defun dtache--db-update-session (session)
  "Update the database with SESSION."
  (let ((id (dtache--session-id session)))
    (emacsql dtache-db [:update dtache-sessions
                        :set (= dtache-session $s2)
                        :where (= id $s1)]
             id session)))

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

(defun dtache--db-select-sessions ()
  "Return all sessions from the database."
  (let ((sessions
         (emacsql dtache-db
                  [:select dtache-session
                   :from dtache-sessions])))
    (-map #'car sessions)))

;;;; Major modes

(defvar dtache-log-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `dtache-log-mode'.")

(define-derived-mode dtache-log-mode nil "Dtache Log"
  "Major mode for dtache logs.")

(provide 'dtache)

;;; dtache.el ends here
