;;; dtache.el --- Run and interact with detached shell commands -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://www.gitlab.com/niklaseklund/dtache.git
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience processes

;; This file is part of GNU Emacs.

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

;; The dtache package allows users to run shell commands detached from
;; Emacs.  These commands are launched in sessions, using the program
;; dtach[1].  These sessions can be easily created through the command
;; `dtache-shell-command', or any of the commands provided by the
;; `dtache-shell', `dtache-eshell' and `dtache-compile' extensions.

;; When a session is created, dtache makes sure that Emacs is attached
;; to it the same time, which makes it a seamless experience for the
;; users.  The `dtache' package internally creates a `dtache-session'
;; for all commands.

;; [1] https://github.com/crigler/dtach

;;; Code:

;;;; Requirements

(require 'ansi-color)
(require 'autorevert)
(require 'notifications)
(require 'filenotify)
(require 'simple)
(require 'tramp)

(declare-function dtache-eshell-get-dtach-process "dtache-eshell")

;;;; Variables

;;;;; Customizable

(defcustom dtache-session-directory (expand-file-name "dtache" (temporary-file-directory))
  "The directory to store sessions."
  :type 'string
  :group 'dtache)

(defcustom dtache-db-directory user-emacs-directory
  "The directory to store the `dtache' database."
  :type 'string
  :group 'dtache)

(defcustom dtache-dtach-program "dtach"
  "The name of the `dtach' program."
  :type 'string
  :group 'dtache)

(defcustom dtache-shell-program shell-file-name
  "Path to the shell to run the dtach command in."
  :type 'string
  :group 'dtache)

(defcustom dtache-env nil
  "The name of, or path to, the `dtache' environment script."
  :type 'string
  :group 'dtache)

(defcustom dtache-annotation-format
  '((:width 3 :function dtache--state-str :face dtache-state-face)
    (:width 3 :function dtache--status-str :face dtache-failure-face)
    (:width 10 :function dtache--host-str :face dtache-host-face)
    (:width 40 :function dtache--working-dir-str :face dtache-working-dir-face)
    (:width 30 :function dtache--metadata-str :face dtache-metadata-face)
    (:width 10 :function dtache--duration-str :face dtache-duration-face)
    (:width 8 :function dtache--size-str :face dtache-size-face)
    (:width 12 :function dtache--creation-str :face dtache-creation-face))
  "The format of the annotations."
  :type '(repeat symbol)
  :group 'dtache)

(defcustom dtache-max-command-length 90
  "Maximum length of displayed command."
  :type 'integer
  :group 'dtache)

(defcustom dtache-tail-interval 2
  "Interval in seconds for the update rate when tailing a session."
  :type 'integer
  :group 'dtache)

(defcustom dtache-shell-command-session-action
  '(:attach dtache-shell-command-attach-session
            :view dtache-view-dwim
            :run dtache-shell-command)
  "Actions for a session created with `dtache-shell-command'."
  :type 'plist
  :group 'dtache)

(defcustom dtache-shell-command-initial-input t
  "Variable to control initial command input for `dtache-shell-command'.
If set to a non nil value the latest entry to
`dtache-shell-command-history' will be used as the initial input in
`dtache-shell-command' when it is used as a command."
  :type 'bool
  :group 'dtache)

(defcustom dtache-nonattachable-commands nil
  "A list of commands which `dtache' should consider nonattachable."
  :type '(repeat (regexp :format "%v"))
  :group 'dtache)

(defcustom dtache-notification-function #'dtache-state-transition-notifications-message
  "Variable to set which function to use to issue a notification."
  :type 'function
  :group 'dtache)

(defcustom dtache-detach-key "C-c C-d"
  "Variable to set the keybinding for detaching."
  :type 'string
  :group 'dtache)

(defcustom dtache-filter-ansi-sequences t
  "Variable to instruct `dtache' to use `ansi-filter'."
  :type 'bool
  :group 'dtache)

(defcustom dtache-log-mode-hook '()
  "Hook for customizing `dtache-log' mode."
  :type 'hook
  :group 'dtache)

;;;;; Public

(defvar dtache-enabled nil)
(defvar dtache-session-mode nil
  "Mode of operation for session.
Valid values are: create, new and attach")
(defvar dtache-session-origin nil
  "Variable to specify the origin of the session.")
(defvar dtache-session-action nil
  "A property list of actions for a session.")
(defvar dtache-shell-command-history nil
  "History of commands run with `dtache-shell-command'.")
(defvar dtache-local-session nil
  "If set to t enforces a local session.")

(defvar dtache-compile-session-hooks nil
  "Hooks to run when compiling a session.")
(defvar dtache-metadata-annotators-alist nil
  "An alist of annotators for metadata.")

(defconst dtache-session-version "0.5.0"
  "The version of `dtache-session'.
This version is encoded as [package-version].[revision].")

(defvar dtache-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'dtache-attach-session)
    (define-key map "c" #'dtache-compile-session)
    (define-key map "d" #'dtache-delete-session)
    (define-key map "i" #'dtache-insert-session-command)
    (define-key map "k" #'dtache-kill-session)
    (define-key map "r" #'dtache-rerun-session)
    (define-key map "t" #'dtache-tail-session)
    (define-key map "v" #'dtache-view-session)
    (define-key map "w" #'dtache-copy-session-command)
    (define-key map "W" #'dtache-copy-session)
    (define-key map "=" #'dtache-diff-session)
    map))

;;;;; Faces

(defgroup dtache-faces nil
  "Faces used by `dtache'."
  :group 'dtache
  :group 'faces)

(defface dtache-metadata-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight metadata in `dtache'.")

(defface dtache-failure-face
  '((t :inherit error))
  "Face used to highlight failure in `dtache'.")

(defface dtache-state-face
  '((t :inherit success))
  "Face used to highlight state in `dtache'.")

(defface dtache-duration-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight duration in `dtache'.")

(defface dtache-size-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight size in `dtache'.")

(defface dtache-creation-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight date in `dtache'.")

(defface dtache-working-dir-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight working directory in `dtache'.")

(defface dtache-host-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight host in `dtache'.")

(defface dtache-identifier-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight identifier in `dtache'.")

;;;;; Private

(defvar dtache--sessions-initialized nil
  "Sessions are initialized.")
(defvar dtache--sessions nil
  "A list of sessions.")
(defvar dtache--watched-session-directories nil
  "An alist where values are a (directory . descriptor).")
(defvar dtache--db-watch nil
  "A descriptor to the `dtache-db-directory'.")
(defvar dtache--buffer-session nil
  "The `dtache-session' session in current buffer.")
(defvar dtache--current-session nil
  "The current session.")
(make-variable-buffer-local 'dtache--buffer-session)
(defvar dtache--session-candidates nil
  "An alist of session candidates.")

(defconst dtache--shell-command-buffer "*Dtache Shell Command*"
  "Name of the `dtache-shell-command' buffer.")
(defconst dtache--dtach-eof-message "\\[EOF - dtach terminating\\]"
  "Message printed when `dtach' terminates.")
(defconst dtache--dtach-detached-message "\\[detached\\]\^M"
  "Message printed when detaching from `dtach'.")
(defconst dtache--dtach-detach-character "\C-\\"
  "Character used to detach from a session.")

;;;; Data structures

(cl-defstruct (dtache-session (:constructor dtache--session-create)
                              (:conc-name dtache--session-))
  (id nil :read-only t)
  (command nil :read-only t)
  (origin nil :read-only t)
  (working-directory nil :read-only t)
  (directory nil :read-only t)
  (metadata nil :read-only t)
  (host nil :read-only t)
  (attachable nil :read-only t)
  (action nil :read-only t)
  (time nil)
  (status nil)
  (size nil)
  (state nil))

;;;; Macros

(defmacro dtache-connection-local-variables (&rest body)
  "A macro that conditionally use `connection-local-variables' when executing BODY."
  `(if dtache-local-session
       (progn
         ,@body)
     (with-connection-local-variables
      (progn
        ,@body))))

;;;; Commands

;;;###autoload
(defun dtache-shell-command (command &optional suppress-output)
  "Execute COMMAND with `dtache'.

Optionally SUPPRESS-OUTPUT if prefix-argument is provided."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Dtache shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Dtache shell command: ")
                        (when dtache-shell-command-initial-input
                          (car dtache-shell-command-history))
                        'dtache-shell-command-history)
    current-prefix-arg))
  (let* ((dtache-session-origin (or dtache-session-origin 'shell-command))
         (dtache-session-action (or dtache-session-action
                                    dtache-shell-command-session-action))
         (dtache--current-session (dtache-create-session command)))
    (dtache-start-session command suppress-output)))

;;;###autoload
(defun dtache-open-session (session)
  "Open a `dtache' SESSION."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (if (eq 'active (dtache--session-state session))
        (dtache-tail-session session)
      (if-let ((view-fun (plist-get (dtache--session-action session) :view)))
          (funcall view-fun session)
        (dtache-view-dwim session)))))

;;;###autoload
(defun dtache-compile-session (session)
  "Compile SESSION.

The session is compiled by opening its output and enabling
`compilation-minor-mode'."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (let ((buffer-name "*dtache-session-output*")
          (file
           (dtache--session-file session 'log))
          (tramp-verbose 1))
      (when (file-exists-p file)
        (with-current-buffer (get-buffer-create buffer-name)
          (setq-local buffer-read-only nil)
          (erase-buffer)
          (insert (dtache--session-output session))
          (setq-local default-directory
                      (dtache--session-working-directory session))
          (run-hooks 'dtache-compile-session-hooks)
          (dtache-log-mode)
          (compilation-minor-mode)
          (setq dtache--buffer-session session)
          (setq-local font-lock-defaults '(compilation-mode-font-lock-keywords t))
          (font-lock-mode)
          (read-only-mode))
        (pop-to-buffer buffer-name)))))

;;;###autoload
(defun dtache-rerun-session (session &optional suppress-output)
  "Rerun SESSION, optionally SUPPRESS-OUTPUT."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))
         current-prefix-arg))
  (when (dtache-valid-session session)
    (let* ((default-directory
             (dtache--session-working-directory session))
           (dtache-session-action (dtache--session-action session))
           (command (dtache--session-command session)))
      (if suppress-output
          (dtache-start-session command suppress-output)
        (if-let ((run-fun (plist-get (dtache--session-action session) :run)))
            (funcall run-fun command)
          (dtache-start-session command))))))

;;;###autoload
(defun dtache-attach-session (session)
  "Attach to SESSION."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (if (or (eq 'inactive (dtache--session-state session))
            (not (dtache--session-attachable session)))
        (dtache-open-session session)
      (if-let ((attach-fun (plist-get (dtache--session-action session) :attach)))
          (funcall attach-fun session)
        (dtache-shell-command-attach-session session)))))

;;;###autoload
(defun dtache-copy-session (session)
  "Copy SESSION's output."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (with-temp-buffer
      (insert (dtache--session-output session))
      (kill-new (buffer-string)))))

;;;###autoload
(defun dtache-copy-session-command (session)
  "Copy SESSION's command."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (kill-new (dtache--session-command session))))

;;;###autoload
(defun dtache-insert-session-command (session)
  "Insert SESSION's command."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (insert (dtache--session-command session))))

;;;###autoload
(defun dtache-delete-session (session)
  "Delete SESSION."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (if (eq 'active (dtache--determine-session-state session))
        (message "Kill session first before removing it.")
      (dtache--db-remove-entry session))))

;;;###autoload
(defun dtache-kill-session (session &optional delete)
  "Send a TERM signal to SESSION.

Optionally DELETE the session if prefix-argument is provided."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))
         current-prefix-arg))
  (when (dtache-valid-session session)
    (when-let* ((default-directory (dtache--session-directory session))
                (pid (dtache--session-pid session)))
      (dtache--kill-processes pid))
    (when delete
      (dtache--db-remove-entry session))))

;;;###autoload
(defun dtache-view-session (session)
  "View the SESSION."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (let* ((buffer-name "*dtache-session-output*")
           (file-path
            (dtache--session-file session 'log))
           (tramp-verbose 1))
      (if (file-exists-p file-path)
          (progn
            (with-current-buffer (get-buffer-create buffer-name)
              (setq-local buffer-read-only nil)
              (erase-buffer)
              (insert (dtache--session-output session))
              (setq-local default-directory (dtache--session-working-directory session))
              (dtache-log-mode)
              (setq dtache--buffer-session session)
              (goto-char (point-max)))
            (pop-to-buffer buffer-name))
        (message "Dtache can't find file: %s" file-path)))))

;;;###autoload
(defun dtache-tail-session (session)
  "Tail the SESSION."
  (interactive
   (list (dtache-completing-read (dtache-get-sessions))))
  (when (dtache-valid-session session)
    (if (eq 'active (dtache--determine-session-state session))
        (let* ((file-path
                (dtache--session-file session 'log))
               (tramp-verbose 1))
          (when (file-exists-p file-path)
            (find-file-other-window file-path)
            (setq dtache--buffer-session session)
            (dtache-tail-mode)
            (goto-char (point-max))))
      (dtache-view-session session))))

;;;###autoload
(defun dtache-diff-session (session1 session2)
  "Diff SESSION1 with SESSION2."
  (interactive
   (let ((sessions (dtache-get-sessions)))
     `(,(dtache-completing-read sessions)
       ,(dtache-completing-read sessions))))
  (when (and (dtache-valid-session session1)
             (dtache-valid-session session2))
    (let ((buffer1 "*dtache-session-output-1*")
          (buffer2 "*dtache-session-output-2*"))
      (with-current-buffer (get-buffer-create buffer1)
        (erase-buffer)
        (insert (dtache--session-header session1))
        (insert (dtache--session-output session1)))
      (with-current-buffer (get-buffer-create buffer2)
        (erase-buffer)
        (insert (dtache--session-header session2))
        (insert (dtache--session-output session2)))
      (ediff-buffers buffer1 buffer2))))

;;;###autoload
(defun dtache-detach-session ()
  "Detach from session in current buffer.

This command is only activated if `dtache--buffer-session' is an
active session.  For sessions created with `dtache-compile' or
`dtache-shell-command', the command will also kill the window."
  (interactive)
  (if (dtache-session-p dtache--buffer-session)
      (if (eq major-mode 'dtache-tail-mode)
          (dtache-quit-tail-session)
          (if-let ((command-or-compile
                    (cond ((string-match dtache--shell-command-buffer (buffer-name)) t)
                          ((string-match "\*dtache-compilation" (buffer-name)) t)
                          ((eq major-mode 'dtache-log-mode) t)
                          ((eq major-mode 'dtache-tail-mode) t)
                          (t nil))))
              ;; `dtache-shell-command' or `dtache-compile'
              (let ((kill-buffer-query-functions nil))
                (when-let ((process (get-buffer-process (current-buffer))))
                  (comint-simple-send process dtache--dtach-detach-character)
                  (message "[detached]"))
                (setq dtache--buffer-session nil)
                (kill-buffer-and-window))
            (if (eq 'active (dtache--determine-session-state dtache--buffer-session))
                ;; `dtache-eshell'
                (if-let ((process (and (eq major-mode 'eshell-mode)
                                       (dtache-eshell-get-dtach-process))))
                    (progn
                      (setq dtache--buffer-session nil)
                      (process-send-string process dtache--dtach-detach-character))
                  ;; `dtache-shell'
                  (let ((process (get-buffer-process (current-buffer))))
                    (comint-simple-send process dtache--dtach-detach-character)
                    (setq dtache--buffer-session nil)))
              (message "No active dtache-session found in buffer."))))
    (message "No dtache-session found in buffer.")))

;;;###autoload
(defun dtache-delete-sessions (&optional all-hosts)
  "Delete `dtache' sessions which belong to the current host, unless ALL-HOSTS."
  (interactive "P")
  (let* ((host-name (car (dtache--host)))
         (sessions (if all-hosts
                       (dtache-get-sessions)
                     (seq-filter (lambda (it)
                                   (string= (car (dtache--session-host it)) host-name))
                                 (dtache-get-sessions)))))
    (seq-do #'dtache--db-remove-entry sessions)))

;;;###autoload
(defun dtache-quit-tail-session ()
  "Quit `dtache' tail session.

The log can have been updated, but that is not done by the user but
rather the tail mode.  To avoid a promtp `buffer-modified-p' is set to
nil before closing."
  (interactive)
  (set-buffer-modified-p nil)
  (setq dtache--buffer-session nil)
  (kill-buffer-and-window))

;;;; Functions

;;;;; Session

(defun dtache-create-session (command)
  "Create a `dtache' session from COMMAND."
  (dtache-connection-local-variables
   (dtache--create-session-directory)
   (let ((session
          (dtache--session-create :id (intern (dtache--create-id command))
                                  :command command
                                  :origin dtache-session-origin
                                  :action dtache-session-action
                                  :working-directory (dtache--get-working-directory)
                                  :attachable (dtache-attachable-command-p command)
                                  :time `(:start ,(time-to-seconds (current-time)) :end 0.0 :duration 0.0 :offset 0.0)
                                  :status '(unknown . 0)
                                  :size 0
                                  :directory (if dtache-local-session dtache-session-directory
                                               (concat (file-remote-p default-directory) dtache-session-directory))
                                  :host (dtache--host)
                                  :metadata (dtache-metadata)
                                  :state 'unknown)))
     (dtache--db-insert-entry session)
     (dtache--watch-session-directory (dtache--session-directory session))
     session)))

(defun dtache-start-session (command &optional suppress-output)
  "Start a `dtache' session running COMMAND.

Optionally SUPPRESS-OUTPUT."
  (let ((inhibit-message t)
        (dtache-enabled t)
        (dtache--current-session
         (or dtache--current-session
             (dtache-create-session command))))
    (if-let ((run-in-background
              (and (or suppress-output
                       (eq dtache-session-mode 'create)
                       (not (dtache--session-attachable dtache--current-session)))))
             (dtache-session-mode 'create))
        (progn (setq dtache-enabled nil)
               (if dtache-local-session
                   (apply #'start-process-shell-command
                          `("dtache" nil ,(dtache-dtach-command dtache--current-session t)))
                 (apply #'start-file-process-shell-command
                        `("dtache" nil ,(dtache-dtach-command dtache--current-session t)))))
      (cl-letf* ((dtache-session-mode 'create-and-attach)
                 ((symbol-function #'set-process-sentinel) #'ignore)
                 (buffer (get-buffer-create dtache--shell-command-buffer)))
        (when (get-buffer-process buffer)
          (setq buffer (generate-new-buffer (buffer-name buffer))))
        (setq dtache-enabled nil)
        (funcall #'async-shell-command (dtache-dtach-command dtache--current-session t) buffer)
        (with-current-buffer buffer (setq dtache--buffer-session dtache--current-session))))))

(defun dtache-session-candidates (sessions)
  "Return an alist of SESSIONS candidates."
  (setq dtache--session-candidates
        (thread-last sessions
                     (seq-map (lambda (it)
                                `(,(dtache--session-truncate-command it)
                                  . ,it)))
                     (dtache--session-deduplicate)
                     (seq-map (lambda (it)
                                ;; Max width is the ... padding + width of identifier
                                (setcar it (truncate-string-to-width (car it) (+ 3 6 dtache-max-command-length) 0 ?\s))
                                it)))))

(defun dtache-session-annotation (item)
  "Associate ITEM to a session and return ts annotation."
  (let ((session (cdr (assoc item dtache--session-candidates))))
    (mapconcat
     #'identity
     (cl-loop for annotation in dtache-annotation-format
              collect (let ((str (funcall (plist-get annotation :function) session)))
                        (truncate-string-to-width
                         (propertize str 'face (plist-get annotation :face))
                         (plist-get annotation :width)
                         0 ?\s)))
     "   ")))

;;;###autoload
(defun dtache-setup ()
  "Initialize `dtache'."

  ;; Initialize sessions
  (unless dtache--sessions-initialized
    (unless (file-exists-p dtache-db-directory)
      (make-directory dtache-db-directory t))
    (dtache--db-initialize)
    (setq dtache--db-watch
      (file-notify-add-watch dtache-db-directory
                             '(change attribute-change)
                             #'dtache--db-directory-event))
    (setq dtache--sessions-initialized t)

    ;; Remove missing local sessions
    (thread-last (dtache--db-get-sessions)
                 (seq-filter (lambda (it) (eq 'local (cdr (dtache--session-host it)))))
                 (seq-filter #'dtache--session-missing-p)
                 (seq-do #'dtache--db-remove-entry))

    ;; Validate sessions with unknown state
    (dtache--validate-unknown-sessions)

    ;; Update transitioned sessions
    (thread-last (dtache--db-get-sessions)
                 (seq-filter (lambda (it) (eq 'active (dtache--session-state it))))
                 (seq-remove (lambda (it) (when (dtache--session-missing-p it)
                                       (dtache--db-remove-entry it)
                                       t)))
                 (seq-filter #'dtache--state-transition-p)
                 (seq-do #'dtache--session-state-transition-update))

    ;; Watch session directories with active sessions
    (thread-last (dtache--db-get-sessions)
                 (seq-filter (lambda (it) (eq 'active (dtache--session-state it))))
                 (seq-map #'dtache--session-directory)
                 (seq-uniq)
                 (seq-do #'dtache--watch-session-directory))

    ;; Other
    (add-hook 'shell-mode-hook #'dtache-shell-mode)))

(defun dtache-valid-session (session)
  "Ensure that SESSION is valid.

If session is not valid trigger an automatic cleanup on SESSION's host."
  (when (dtache-session-p session)
    (if (not (dtache--session-missing-p session))
        t
      (let ((host (dtache--session-host session)))
        (message "Session does not exist. Initiate sesion cleanup on host %s" (car host))
        (dtache--cleanup-host-sessions host)
        nil))))

(defun dtache-session-exit-code-status (session)
  "Return status based on exit-code in SESSION."
  (if (null dtache-env)
      `(unknown . 0)
    (let ((dtache-env-message
           (with-temp-buffer
             (insert-file-contents (dtache--session-file session 'log))
             (goto-char (point-max))
             (thing-at-point 'line t)))
          (success-message "Dtache session finished")
          (failure-message (rx "Dtache session exited abnormally with code " (group (one-or-more digit)))))
      (cond ((string-match success-message dtache-env-message) `(success . 0))
            ((string-match failure-message dtache-env-message)
             `(failure . ,(string-to-number (match-string 1 dtache-env-message))))
            (t `(unknown . 0))))))

(defun dtache-state-transitionion-echo-message (session)
  "Issue a notification when SESSION transitions from active to inactive.
This function uses the echo area."
  (let ((status (pcase (car (dtache--session-status session))
                  ('success "Dtache finished")
                  ('failure "Dtache failed")
                  ('unknown "Dtache finished"))))
    (message "%s [%s]: %s" status (car (dtache--session-host session)) (dtache--session-command session))))

(defun dtache-state-transition-notifications-message (session)
  "Issue a notification when SESSION transitions from active to inactive.
This function uses the `notifications' library."
  (let ((status (car (dtache--session-status session)))
        (host (car (dtache--session-host session))))
    (notifications-notify
     :title (pcase status
              ('success (format "Dtache finished [%s]" host))
              ('failure (format "Dtache failed [%s]" host))
              ('unknown (format "Dtache finished [%s]" host)))
     :body (dtache--session-command session)
     :urgency (pcase status
                ('success 'normal)
                ('failure 'critical)
                ('unknown 'normal)))))

(defun dtache-view-dwim (session)
  "View SESSION in a do what I mean fashion."
  (let ((status (car (dtache--session-status session))))
    (cond ((eq 'success status)
           (dtache-view-session session))
          ((eq 'failure status)
           (dtache-compile-session session))
          ((eq 'unknown status)
           (dtache-view-session session))
          (t (message "Dtache session is in an unexpected state.")))))

(defun dtache-get-sessions ()
  "Return validitated sessions."
  (dtache--validate-unknown-sessions)
  (dtache--db-get-sessions))

(defun dtache-shell-command-attach-session (session)
  "Attach to SESSION with `async-shell-command'."
  (let* ((dtache--current-session session)
         (dtache-session-mode 'attach)
         (inhibit-message t))
    (if (not (dtache--session-attachable session))
        (dtache-tail-session session)
      (cl-letf* (((symbol-function #'set-process-sentinel) #'ignore)
                 (buffer (get-buffer-create dtache--shell-command-buffer))
                 (default-directory (dtache--session-working-directory session))
                 (dtach-command (dtache-dtach-command session t)))
        (when (get-buffer-process buffer)
          (setq buffer (generate-new-buffer (buffer-name buffer))))
        (funcall #'async-shell-command dtach-command buffer)
        (with-current-buffer buffer (setq dtache--buffer-session dtache--current-session))))))

;;;;; Other

(cl-defgeneric dtache-dtach-command (entity &optional concat)
  "Return dtach command for ENTITY optionally CONCAT.")

(cl-defgeneric dtache-dtach-command ((command string) &optional concat)
  "Return dtach command for COMMAND.

Optionally CONCAT the command return command into a string."
  (dtache-dtach-command (dtache-create-session command) concat))

(cl-defgeneric dtache-dtach-command ((session dtache-session) &optional concat)
  "Return dtach command for SESSION.

Optionally CONCAT the command return command into a string."
  (dtache-connection-local-variables
   (let* ((dtache-session-mode (cond ((eq dtache-session-mode 'attach) 'attach)
                                     ((not (dtache--session-attachable session)) 'create)
                                     (t dtache-session-mode)))
          (socket (dtache--session-file session 'socket t))
          (dtach-arg (dtache--dtach-arg)))
     (setq dtache--buffer-session session)
     (if (eq dtache-session-mode 'attach)
         (if concat
             (mapconcat #'identity
                        `(,dtache-dtach-program
                          ,dtach-arg
                          ,socket
                          "-r none")
                        " ")
           `(,dtach-arg ,socket "-r none"))
       (if concat
           (mapconcat #'identity
                      `(,dtache-dtach-program
                        ,dtach-arg
                        ,socket "-z"
                        ,dtache-shell-program "-c"
                        ,(shell-quote-argument (dtache--dtache-command session)))
                      " ")
         `(,dtach-arg ,socket "-z"
                      ,dtache-shell-program "-c"
                      ,(dtache--dtache-command session)))))))

(defun dtache-attachable-command-p (command)
  "Return t if COMMAND is attachable."
  (if (thread-last dtache-nonattachable-commands
                   (seq-filter (lambda (regexp)
                                 (string-match-p regexp command)))
                   (length)
                   (= 0))
      t
    nil))

(defun dtache-metadata ()
  "Return a property list with metadata."
  (let ((metadata '()))
    (seq-doseq (annotator dtache-metadata-annotators-alist)
      (push `(,(car annotator) . ,(funcall (cdr annotator))) metadata))
    metadata))

(defun dtache-completing-read (sessions)
  "Select a session from SESSIONS through `completing-read'."
  (let* ((candidates (dtache-session-candidates sessions))
         (metadata `(metadata
                     (category . dtache)
                     (cycle-sort-function . identity)
                     (display-sort-function . identity)
                     (annotation-function . dtache-session-annotation)
                     (affixation-function .
                                          ,(lambda (cands)
                                             (seq-map (lambda (s)
                                                        `(,s nil ,(dtache-session-annotation s)))
                                                      cands)))))
         (collection (lambda (string predicate action)
                       (if (eq action 'metadata)
                           metadata
                         (complete-with-action action candidates string predicate))))
         (cand (completing-read "Select session: " collection nil t)))
    (dtache--decode-session cand)))

;;;; Support functions

;;;;; Session

(defun dtache--session-pid (session)
  "Return SESSION's pid."
  (let* ((socket
          (expand-file-name
           (concat (symbol-name (dtache--session-id session)) ".socket")
           (or
            (file-remote-p default-directory 'localname)
            default-directory))))
    (car
     (split-string
      (with-temp-buffer
        (apply #'process-file `("pgrep" nil t nil "-f" ,(shell-quote-argument (format "dtach -. %s" socket))))
        (buffer-string))
      "\n" t))))

(defun dtache--session-truncate-command (session)
  "Return a truncated string representation of SESSION's command."
  (let ((command (dtache--session-command session)))
    (if (<= (length command) dtache-max-command-length)
        command
      (concat
       (substring command 0 (/ dtache-max-command-length 2))
       "..."
       (substring command (- (length command) (/ dtache-max-command-length 2)) (length command))))))

(defun dtache--determine-session-state (session)
  "Return t if SESSION is active."
  (if (file-exists-p
       (dtache--session-file session 'socket))
      'active
    'inactive))

(defun dtache--state-transition-p (session)
  "Return t if SESSION has transitioned from active to inactive."
  (and
   (eq 'active (dtache--session-state session))
   (eq 'inactive (dtache--determine-session-state session))))

(defun dtache--session-missing-p (session)
  "Return t if SESSION is missing."
  (not
   (file-exists-p
    (dtache--session-file session 'log))))

(defun dtache--session-header (session)
  "Return header for SESSION."
  (mapconcat
   #'identity
   `(,(format "Command: %s" (dtache--session-command session))
     ,(format "Working directory: %s" (dtache--working-dir-str session))
     ,(format "Host: %s" (car (dtache--session-host session)))
     ,(format "Id: %s" (symbol-name (dtache--session-id session)))
     ,(format "Status: %s" (car (dtache--session-status session)))
     ,(format "Exit-code: %s" (cdr (dtache--session-status session)))
     ,(format "Metadata: %s" (dtache--metadata-str session))
     ,(format "Created at: %s" (dtache--creation-str session))
     ,(format "Duration: %s\n" (dtache--duration-str session))
     "")
   "\n"))

(defun dtache--session-deduplicate (sessions)
  "Make car of SESSIONS unique by adding an identifier to it."
  (let* ((ht (make-hash-table :test #'equal :size (length sessions)))
         (identifier-width 6)
         (reverse-sessions (seq-reverse sessions)))
    (dolist (session reverse-sessions)
      (if-let (count (gethash (car session) ht))
          (setcar session (format "%s%s" (car session)
                                  (truncate-string-to-width
                                   (propertize (format " (%s)" (puthash (car session) (1+ count) ht)) 'face 'dtache-identifier-face)
                                   identifier-width 0 ?\s)))
        (puthash (car session) 0 ht)
        (setcar session (format "%s%s" (car session) (make-string identifier-width ?\s)))))
    (seq-reverse reverse-sessions)))

(defun dtache--decode-session (item)
  "Return the session assicated with ITEM."
  (cdr (assoc item dtache--session-candidates)))

(defun dtache--validate-unknown-sessions ()
  "Validate `dtache' sessions with state unknown."
  (thread-last (dtache--db-get-sessions)
               (seq-filter (lambda (it) (eq 'unknown (dtache--session-state it))))
               (seq-do (lambda (it)
                         (if (dtache--session-missing-p it)
                             (dtache--db-remove-entry it)
                           (setf (dtache--session-state it) 'active)
                           (dtache--db-update-entry it))))))

(defun dtache--session-file (session file &optional local)
  "Return the full path to SESSION's FILE.

Optionally make the path LOCAL to host."
  (let* ((file-name
          (concat
           (symbol-name
            (dtache--session-id session))
           (pcase file
             ('socket ".socket")
             ('log ".log"))))
         (remote-local-path (file-remote-p (expand-file-name file-name (dtache--session-directory session)) 'localname))
         (full-path (expand-file-name file-name (dtache--session-directory session))))
    (if (and local remote-local-path)
        remote-local-path
      full-path)))

(defun dtache--cleanup-host-sessions (host)
  "Run cleanuup on HOST sessions."
  (let ((host-name (car host)))
    (thread-last (dtache--db-get-sessions)
                 (seq-filter (lambda (it) (string= host-name (car (dtache--session-host it)))))
                 (seq-filter #'dtache--session-missing-p)
                 (seq-do #'dtache--db-remove-entry))))

(defun dtache--session-output (session)
  "Return content of SESSION's output."
  (let* ((filename (dtache--session-file session 'log))
         (dtache-message (rx (regexp "\n?\nDtache session ") (or "finished" "exited"))))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (let ((beginning (point))
            (end (if (search-forward-regexp dtache-message nil t)
                     (match-beginning 0)
                   (point-max))))
        (buffer-substring beginning end)))))

(defun dtache--create-session-directory ()
  "Create session directory if it doesn't exist."
  (let ((directory
         (concat
          (file-remote-p default-directory)
          dtache-session-directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))))

(defun dtache--get-working-directory ()
  "Return an abbreviated working directory path."
  (if-let (remote (file-remote-p default-directory))
      (replace-regexp-in-string  (expand-file-name remote)
                                 (concat remote "~/")
                                 (expand-file-name default-directory))
    (abbreviate-file-name default-directory)))

;;;;; Database

(defun dtache--db-initialize ()
  "Return all sessions stored in database."
  (let ((db (expand-file-name "dtache.db" dtache-db-directory)))
    (when (file-exists-p db)
      (with-temp-buffer
        (insert-file-contents db)
        (cl-assert (bobp))
        (when (string= (dtache--db-session-version) dtache-session-version)
          (setq dtache--sessions
                (read (current-buffer))))))))

(defun dtache--db-session-version ()
  "Return `dtache-session-version' from database."
  (let ((header (thing-at-point 'line))
        (regexp (rx "Dtache Session Version: " (group (one-or-more (or digit punct))))))
    (string-match regexp header)
    (match-string 1 header)))

(defun dtache--db-insert-entry (session)
  "Insert SESSION into `dtache--sessions' and update database."
  (push `(,(dtache--session-id session) . ,session) dtache--sessions)
  (dtache--db-update-sessions))

(defun dtache--db-remove-entry (session)
  "Remove SESSION from `dtache--sessions', delete log and update database."
  (let ((log (dtache--session-file session 'log)))
    (when (file-exists-p log)
      (delete-file log)))
  (setq dtache--sessions
        (assq-delete-all (dtache--session-id session) dtache--sessions ))
  (dtache--db-update-sessions))

(defun dtache--db-update-entry (session &optional update)
  "Update SESSION in `dtache--sessions' optionally UPDATE database."
  (setf (alist-get (dtache--session-id session) dtache--sessions) session)
  (when update
    (dtache--db-update-sessions)))

(defun dtache--db-get-session (id)
  "Return session with ID."
  (alist-get id dtache--sessions))

(defun dtache--db-get-sessions ()
  "Return all sessions stored in the database."
  (seq-map #'cdr dtache--sessions))

(defun dtache--db-update-sessions ()
  "Write `dtache--sessions' to database."
  (let ((db (expand-file-name "dtache.db" dtache-db-directory)))
    (with-temp-file db
      (insert (format ";; Dtache Session Version: %s\n\n" dtache-session-version))
      (prin1 dtache--sessions (current-buffer)))))

;;;;; Other

(defun dtache--dtach-arg ()
  "Return dtach argument based on `dtache-session-mode'."
  (pcase dtache-session-mode
    ('create "-n")
    ('create-and-attach "-c")
    ('attach "-a")
    (_ (error "`dtache-session-mode' has an unknown value"))))

(defun dtache--session-state-transition-update (session)
  "Update SESSION due to state transition."
  ;; Update session
  (let ((session-size (file-attribute-size
                       (file-attributes
                        (dtache--session-file session 'log))))
        (session-time (dtache--update-session-time session) )
        (status-fun (or (plist-get (dtache--session-action session) :status)
                        #'dtache-session-exit-code-status)))
    (setf (dtache--session-size session) session-size)
    (setf (dtache--session-time session) session-time)
    (setf (dtache--session-state session) 'inactive)
    (setf (dtache--session-status session) (funcall status-fun session)))

  ;; Send notification
  (funcall dtache-notification-function session)

  ;; Update session in database
  (dtache--db-update-entry session t)

  ;; Execute callback
  (when-let ((callback (plist-get (dtache--session-action session) :callback)))
    (funcall callback session)))

(defun dtache--kill-processes (pid)
  "Kill PID and all of its children."
  (let ((child-processes
         (split-string
          (with-temp-buffer
            (apply #'process-file `("pgrep" nil t nil "-P" ,pid))
            (buffer-string))
          "\n" t)))
    (seq-do (lambda (pid) (dtache--kill-processes pid)) child-processes)
    (apply #'process-file `("kill" nil nil nil ,pid))))

(defun dtache--dtache-command (session)
  "Return the dtache command for SESSION.

If SESSION is nonattachable fallback to a command that doesn't rely on tee."
  (let* ((log (dtache--session-file session 'log t))
         (begin-shell-group (if (string= "fish" (file-name-nondirectory dtache-shell-program))
                                "begin;"
                              "{"))
         (end-shell-group (if (or (string= "fish" (file-name-nondirectory dtache-shell-program)))
                              "end"
                            "}"))
         (redirect
          (if (dtache--session-attachable session)
              (format "2>&1 | tee %s" log)
            (format "&> %s" log)))
         (env (if dtache-env dtache-env (format "%s -c" dtache-shell-program)))
         (command
          (shell-quote-argument
           (dtache--session-command session))))
    (format "%s %s %s; %s %s" begin-shell-group env command end-shell-group redirect)))

(defun dtache--host ()
  "Return a cons with (host . type)."
  (let ((remote (file-remote-p default-directory)))
    `(,(if remote (file-remote-p default-directory 'host) (system-name)) . ,(if remote 'remote 'local))))

(defun dtache--ansi-color-tail ()
  "Apply `ansi-color' on tail output."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region auto-revert-tail-pos (point-max))))

(defun dtache--update-session-time (session &optional approximate)
  "Update SESSION's time property.

If APPROXIMATE, use latest modification time of SESSION's
log to deduce the end time."
  (let* ((start-time (plist-get (dtache--session-time session) :start))
         (end-time))
    (if approximate
        (setq end-time
              (time-to-seconds
               (file-attribute-modification-time
                (file-attributes
                 (dtache--session-file session 'log)))))
      (setq end-time (time-to-seconds)))
    `(:start ,start-time :end ,end-time :duration ,(- end-time start-time))))

(defun dtache--create-id (command)
  "Return a hash identifier for COMMAND."
  (let ((current-time (current-time-string)))
    (secure-hash 'md5 (concat command current-time))))

(defun dtache--dtache-env-message-filter (str)
  "Remove `dtache-env' message in STR."
  (replace-regexp-in-string "\n?Dtache session.*\n?" "" str))

(defun dtache--dtach-eof-message-filter (str)
  "Remove `dtache--dtach-eof-message' in STR."
  (replace-regexp-in-string (format "\n?%s\^M\n" dtache--dtach-eof-message) "" str))

(defun dtache--dtach-detached-message-filter (str)
  "Remove `dtache--dtach-detached-message' in STR."
  (replace-regexp-in-string (format "\n?%s\n" dtache--dtach-detached-message) "" str))

(defun dtache--watch-session-directory (session-directory)
  "Watch for events in SESSION-DIRECTORY."
  (unless (alist-get session-directory dtache--watched-session-directories
                     nil nil #'string=)
    (push
     `(,session-directory . ,(file-notify-add-watch
                              session-directory
                              '(change)
                              #'dtache--session-directory-event))
     dtache--watched-session-directories)))

(defun dtache--session-directory-event (event)
  "Act on an EVENT in a directory in `dtache--watched-session-directories'.

If event is caused by the deletion of a socket, locate the related
session and trigger a state transition."
  (pcase-let* ((`(,_ ,action ,file) event))
    (when (and (eq action 'deleted)
               (string= "socket" (file-name-extension file)))
      (when-let* ((id (intern (file-name-base file)))
                  (session (dtache--db-get-session id))
                  (session-directory (dtache--session-directory session)))

        ;; Update session
        (dtache--session-state-transition-update session)

        ;; Remove session directory from `dtache--watch-session-directory'
        ;; if there is no active session associated with the directory
        (unless
            (thread-last (dtache--db-get-sessions)
                         (seq-filter (lambda (it) (eq 'active (dtache--session-state it))))
                         (seq-map #'dtache--session-directory)
                         (seq-uniq)
                         (seq-filter (lambda (it) (string= it session-directory))))
          (file-notify-rm-watch
           (alist-get session-directory dtache--watched-session-directories))
          (setq dtache--watched-session-directories
                (assoc-delete-all session-directory dtache--watched-session-directories)))))))

(defun dtache--db-directory-event (event)
  "Act on EVENT in `dtache-db-directory'.

If event is cased by an update to the `dtache' database, re-initialize
`dtache--sessions'."
  (pcase-let* ((`(,_descriptor ,action ,file) event)
               (database-updated  (and (string= "dtache.db" file)
                                       (eq 'attribute-changed action))))
    (when database-updated)
    (dtache--db-initialize)))

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
  (let* ((duration (if (eq 'active (dtache--session-state session))
                       (- (time-to-seconds) (plist-get (dtache--session-time session) :start))
                     (plist-get
                      (dtache--session-time session) :duration)))
         (time (round duration))
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
   (plist-get
    (dtache--session-time session) :start)))

(defun dtache--size-str (session)
  "Return the size of SESSION's output."
  (if (eq 'active (dtache--session-state session))
      ""
      (file-size-human-readable
       (dtache--session-size session))))

(defun dtache--status-str (session)
  "Return string if SESSION has failed."
  (pcase (car (dtache--session-status session))
    ('failure "!")
    ('success " ")
    ('unknown " ")))

(defun dtache--state-str (session)
  "Return string based on SESSION state."
  (if (eq 'active (dtache--session-state session))
      "*"
    " "))

(defun dtache--working-dir-str (session)
  "Return working directory of SESSION."
  (let ((working-directory
         (dtache--session-working-directory session)))
    (if-let ((remote (file-remote-p working-directory)))
        (string-remove-prefix remote working-directory)
      working-directory)))

(defun dtache--host-str (session)
  "Return host name of SESSION."
  (car (dtache--session-host session)))

;;;; Minor modes

(defvar dtache-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd dtache-detach-key) #'dtache-detach-session)
    map)
  "Keymap for `dtache-shell-mode'.")

;;;###autoload
(define-minor-mode dtache-shell-mode
  "Integrate `dtache' in `shell-mode'."
  :lighter " dtache-shell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if dtache-shell-mode
      (progn
        (add-hook 'comint-preoutput-filter-functions #'dtache--dtache-env-message-filter 0 t)
        (add-hook 'comint-preoutput-filter-functions #'dtache--dtach-eof-message-filter 0 t))
    (remove-hook 'comint-preoutput-filter-functions #'dtache--dtache-env-message-filter t)
    (remove-hook 'comint-preoutput-filter-functions #'dtache--dtach-eof-message-filter t)))

;;;; Major modes

(defvar dtache-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd dtache-detach-key) #'dtache-detach-session)
    map)
  "Keymap for `dtache-log-mode'.")

;;;###autoload
(define-derived-mode dtache-log-mode nil "Dtache Log"
  "Major mode for `dtache' logs."
  (when dtache-filter-ansi-sequences
    (ansi-color-apply-on-region (point-min) (point-max)))
  (read-only-mode t))

(defvar dtache-tail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd dtache-detach-key) #'dtache-detach-session)
    map)
  "Keymap for `dtache-tail-mode'.")

;;;###autoload
(define-derived-mode dtache-tail-mode auto-revert-tail-mode "Dtache Tail"
  "Major mode to tail `dtache' logs."
  (setq-local auto-revert-interval dtache-tail-interval)
  (setq-local tramp-verbose 1)
  (setq-local auto-revert-remote-files t)
  (defvar revert-buffer-preserve-modes)
  (setq-local revert-buffer-preserve-modes nil)
  (auto-revert-set-timer)
  (setq-local auto-revert-verbose nil)
  (auto-revert-tail-mode)
  (when dtache-filter-ansi-sequences
    (add-hook 'after-revert-hook #'dtache--ansi-color-tail nil t)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (read-only-mode t))

(provide 'dtache)

;;; dtache.el ends here
