;;; detached.el --- A package to launch, and manage, detached processes -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://sr.ht/~niklaseklund/detached.el/
;; Version: 0.6
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

;; The detached package allows users to run shell commands detached from
;; Emacs.  These commands are launched in sessions, using the program
;; dtach[1].  These sessions can be easily created through the command
;; `detached-shell-command', or any of the commands provided by the
;; `detached-shell', `detached-eshell' and `detached-compile' extensions.

;; When a session is created, detached makes sure that Emacs is attached
;; to it the same time, which makes it a seamless experience for the
;; users.  The `detached' package internally creates a `detached-session'
;; for all commands.

;; [1] https://github.com/crigler/dtach

;;; Code:

;;;; Requirements

(require 'ansi-color)
(require 'autorevert)
(require 'comint)
(require 'notifications)
(require 'filenotify)
(require 'simple)
(require 'tramp)

(declare-function detached-eshell-get-dtach-process "detached-eshell")

;;;; Variables

;;;;; Customizable

(defcustom detached-session-directory (expand-file-name "detached" (temporary-file-directory))
  "The directory to store sessions."
  :type 'string
  :group 'detached)

(defcustom detached-db-directory user-emacs-directory
  "The directory to store the `detached' database."
  :type 'string
  :group 'detached)

(defcustom detached-dtach-program "dtach"
  "The name of the `dtach' program."
  :type 'string
  :group 'detached)

(defcustom detached-shell-program shell-file-name
  "Path to the shell to run the dtach command in."
  :type 'string
  :group 'detached)

(defcustom detached-show-output-on-attach nil
  "If set to t show the session output when attaching to it."
  :type 'bool
  :group 'detached)

(defcustom detached-show-output-command (executable-find "cat")
  "The command to be run to show a sessions output."
  :type 'string
  :group 'detached)

(defcustom detached-env nil
  "The name of, or path to, the `detached' environment script."
  :type 'string
  :group 'detached)

(defcustom detached-env-plain-text-commands nil
  "A list of regexps for commands to run in plain-text mode."
  :type 'list
  :group 'detached)

(defcustom detached-annotation-format
  '((:width 3 :padding 2 :function detached--status-str :face detached-failure-face)
    (:width 3 :padding 4 :function detached--state-str :face detached-state-face)
    (:width 10 :padding 4 :function detached--host-str :face detached-host-face)
    (:width 40 :padding 4 :function detached--working-dir-str :face detached-working-dir-face)
    (:width 40 :padding 4 :function detached--metadata-str :face detached-metadata-face)
    (:width 10 :padding 4 :function detached--duration-str :face detached-duration-face)
    (:width 8 :padding 4 :function detached--size-str :face detached-size-face)
    (:width 12 :padding 4 :function detached--creation-str :face detached-creation-face))
  "The format of the annotations."
  :type '(repeat symbol)
  :group 'detached)

(defcustom detached-command-format
  '(:width 90 :padding 4 :function detached-command-str)
  "The format for displaying the command."
  :type 'integer
  :group 'detached)

(defcustom detached-tail-interval 2
  "Interval in seconds for the update rate when tailing a session."
  :type 'integer
  :group 'detached)

(defcustom detached-open-active-session-action 'attach
  "How to open an active session, allowed values are `attach' and `tail'."
  :type 'symbol
  :group 'detached)

(defcustom detached-shell-command-session-action
  '(:attach detached-shell-command-attach-session
            :view detached-view-dwim
            :run detached-shell-command)
  "Actions for a session created with `detached-shell-command'."
  :type 'plist
  :group 'detached)

(defcustom detached-shell-command-initial-input t
  "Variable to control initial command input for `detached-shell-command'.
If set to a non nil value the latest entry to
`detached-shell-command-history' will be used as the initial input in
`detached-shell-command' when it is used as a command."
  :type 'bool
  :group 'detached)

(defcustom detached-nonattachable-commands nil
  "A list of commands which `detached' should consider nonattachable."
  :type '(repeat (regexp :format "%v"))
  :group 'detached)

(defcustom detached-notification-function #'detached-state-transition-notifications-message
  "Variable to set which function to use to issue a notification."
  :type 'function
  :group 'detached)

(defcustom detached-detach-key "C-c C-d"
  "Variable to set the keybinding for detaching."
  :type 'string
  :group 'detached)

(defcustom detached-filter-ansi-sequences t
  "Variable to instruct `detached' to use `ansi-filter'."
  :type 'bool
  :group 'detached)

(defcustom detached-log-mode-hook '()
  "Hook for customizing `detached-log' mode."
  :type 'hook
  :group 'detached)

(defcustom detached-shell-mode-filter-functions
  '(detached--detached-env-message-filter
    detached--dtach-eof-message-filter)
  "A list of filter functions that are run in `detached-shell-mode'."
  :type 'list
  :group 'detached)

;;;;; Public

(defvar detached-enabled nil)
(defvar detached-session-mode nil
  "Mode of operation for session.
Valid values are: create, new and attach")
(defvar detached-session-origin nil
  "Variable to specify the origin of the session.")
(defvar detached-session-action nil
  "A property list of actions for a session.")
(defvar detached-shell-command-history nil
  "History of commands run with `detached-shell-command'.")
(defvar detached-local-session nil
  "If set to t enforces a local session.")

(defvar detached-compile-session-hooks nil
  "Hooks to run when compiling a session.")
(defvar detached-metadata-annotators-alist nil
  "An alist of annotators for metadata.")

(defconst detached-session-version "0.6.1"
  "The version of `detached-session'.
This version is encoded as [package-version].[revision].")

;;;;; Faces

(defgroup detached-faces nil
  "Faces used by `detached'."
  :group 'detached
  :group 'faces)

(defface detached-metadata-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight metadata in `detached'.")

(defface detached-failure-face
  '((t :inherit error))
  "Face used to highlight failure in `detached'.")

(defface detached-state-face
  '((t :inherit success))
  "Face used to highlight state in `detached'.")

(defface detached-duration-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight duration in `detached'.")

(defface detached-size-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight size in `detached'.")

(defface detached-creation-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight date in `detached'.")

(defface detached-working-dir-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight working directory in `detached'.")

(defface detached-host-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight host in `detached'.")

(defface detached-identifier-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight identifier in `detached'.")

;;;;; Private

(defvar detached--sessions-initialized nil
  "Sessions are initialized.")
(defvar detached--sessions nil
  "A list of sessions.")
(defvar detached--watched-session-directories nil
  "An alist where values are a (directory . descriptor).")
(defvar detached--db-watch nil
  "A descriptor to the `detached-db-directory'.")
(defvar detached--buffer-session nil
  "The `detached-session' session in current buffer.")
(defvar detached--current-session nil
  "The current session.")
(make-variable-buffer-local 'detached--buffer-session)
(defvar detached--session-candidates nil
  "An alist of session candidates.")
(defvar detached--annotation-widths nil
  "An alist of widths to use for annotation.")

(defconst detached--shell-command-buffer "*Detached Shell Command*"
  "Name of the `detached-shell-command' buffer.")
(defconst detached--dtach-eof-message "\\[EOF - dtach terminating\\]"
  "Message printed when `dtach' terminates.")
(defconst detached--dtach-detached-message "\\[detached\\]\^M"
  "Message printed when detaching from `dtach'.")
(defconst detached--dtach-detach-character "\C-\\"
  "Character used to detach from a session.")

;;;; Data structures

(cl-defstruct (detached-session (:constructor detached--session-create)
                              (:conc-name detached--session-))
  (id nil :read-only t)
  (command nil :read-only t)
  (origin nil :read-only t)
  (working-directory nil :read-only t)
  (directory nil :read-only t)
  (metadata nil :read-only t)
  (host nil :read-only t)
  (attachable nil :read-only t)
  (env-mode nil :read-only t)
  (action nil :read-only t)
  (time nil)
  (status nil)
  (size nil)
  (state nil))

;;;; Macros

(defmacro detached-connection-local-variables (&rest body)
  "A macro that conditionally use `connection-local-variables' when executing BODY."
  `(if detached-local-session
       (progn
         ,@body)
     (with-connection-local-variables
      (progn
        ,@body))))

;;;; Commands

;;;###autoload
(defun detached-shell-command (command &optional suppress-output)
  "Execute COMMAND with `detached'.

Optionally SUPPRESS-OUTPUT if prefix-argument is provided."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Detached shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Detached shell command: ")
                        (when detached-shell-command-initial-input
                          (car detached-shell-command-history))
                        'detached-shell-command-history)
    current-prefix-arg))
  (let* ((detached-session-origin (or detached-session-origin 'shell-command))
         (detached-session-action (or detached-session-action
                                    detached-shell-command-session-action))
         (detached--current-session (detached-create-session command)))
    (detached-start-session command suppress-output)))

;;;###autoload
(defun detached-open-session (session)
  "Open a `detached' SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (if (eq 'active (detached--session-state session))
        (pcase detached-open-active-session-action
          ('attach (detached-attach-session session))
          ('tail (detached-tail-session session))
          (_ (message "`detached-open-active-session-action' has an incorrect value")))
      (if-let ((view-fun (plist-get (detached--session-action session) :view)))
          (funcall view-fun session)
        (detached-view-dwim session)))))

;;;###autoload
(defun detached-compile-session (session)
  "Compile SESSION.

The session is compiled by opening its output and enabling
`compilation-minor-mode'."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (let ((buffer-name "*detached-session-output*")
          (file
           (detached--session-file session 'log))
          (tramp-verbose 1))
      (when (file-exists-p file)
        (with-current-buffer (get-buffer-create buffer-name)
          (setq-local buffer-read-only nil)
          (erase-buffer)
          (insert (detached--session-output session))
          (setq-local default-directory
                      (detached--session-working-directory session))
          (run-hooks 'detached-compile-session-hooks)
          (detached-log-mode)
          (compilation-minor-mode)
          (setq detached--buffer-session session)
          (setq-local font-lock-defaults '(compilation-mode-font-lock-keywords t))
          (font-lock-mode)
          (read-only-mode))
        (pop-to-buffer buffer-name)))))

;;;###autoload
(defun detached-rerun-session (session &optional suppress-output)
  "Rerun SESSION, optionally SUPPRESS-OUTPUT."
  (interactive
   (list (detached-completing-read (detached-get-sessions))
         current-prefix-arg))
  (when (detached-valid-session session)
    (let* ((default-directory
            (detached--session-working-directory session))
           (detached-session-action (detached--session-action session))
           (command (detached--session-command session)))
      (if suppress-output
          (detached-start-session command suppress-output)
        (if-let ((run-fun (plist-get (detached--session-action session) :run)))
            (funcall run-fun command)
          (detached-start-session command))))))

;;;###autoload
(defun detached-attach-session (session)
  "Attach to SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (if (or (eq 'inactive (detached--session-state session))
            (not (detached--session-attachable session)))
        (detached-open-session session)
      (if-let ((attach-fun (plist-get (detached--session-action session) :attach)))
          (funcall attach-fun session)
        (detached-shell-command-attach-session session)))))

;;;###autoload
(defun detached-copy-session (session)
  "Copy SESSION's output."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (with-temp-buffer
      (insert (detached--session-output session))
      (when (eq 'terminal-data (detached--session-env-mode session))
        ;; Enable `detached-log-mode' to parse ansi-escape sequences
        (detached-log-mode))
      (kill-new (buffer-string)))))

;;;###autoload
(defun detached-copy-session-command (session)
  "Copy SESSION's command."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (kill-new (detached--session-command session))))

;;;###autoload
(defun detached-insert-session-command (session)
  "Insert SESSION's command."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (insert (detached--session-command session))))

;;;###autoload
(defun detached-delete-session (session)
  "Delete SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (if (eq 'active (detached--determine-session-state session))
        (message "Kill session first before removing it.")
      (detached--db-remove-entry session))))

;;;###autoload
(defun detached-kill-session (session &optional delete)
  "Send a TERM signal to SESSION.

Optionally DELETE the session if prefix-argument is provided."
  (interactive
   (list (detached-completing-read (detached-get-sessions))
         current-prefix-arg))
  (when (detached-valid-session session)
    (when-let* ((default-directory (detached--session-directory session))
                (pid (detached--session-pid session)))
      (detached--kill-processes pid))
    (when delete
      (detached--db-remove-entry session))))

;;;###autoload
(defun detached-view-session (session)
  "View the SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (let* ((buffer-name "*detached-session-output*")
           (file-path
            (detached--session-file session 'log))
           (tramp-verbose 1))
      (if (file-exists-p file-path)
          (progn
            (with-current-buffer (get-buffer-create buffer-name)
              (setq-local buffer-read-only nil)
              (erase-buffer)
              (insert (detached--session-output session))
              (setq-local default-directory (detached--session-working-directory session))
              (detached-log-mode)
              (setq detached--buffer-session session)
              (goto-char (point-max)))
            (pop-to-buffer buffer-name))
        (message "Detached can't find file: %s" file-path)))))

;;;###autoload
(defun detached-tail-session (session)
  "Tail the SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (if (eq 'active (detached--determine-session-state session))
        (let* ((file-path
                (detached--session-file session 'log))
               (tramp-verbose 1))
          (when (file-exists-p file-path)
            (find-file-other-window file-path)
            (setq detached--buffer-session session)
            (detached-tail-mode)
            (goto-char (point-max))))
      (detached-view-session session))))

;;;###autoload
(defun detached-diff-session (session1 session2)
  "Diff SESSION1 with SESSION2."
  (interactive
   (let ((sessions (detached-get-sessions)))
     `(,(detached-completing-read sessions)
       ,(detached-completing-read sessions))))
  (when (and (detached-valid-session session1)
             (detached-valid-session session2))
    (let ((buffer1 "*detached-session-output-1*")
          (buffer2 "*detached-session-output-2*"))
      (with-current-buffer (get-buffer-create buffer1)
        (erase-buffer)
        (insert (detached--session-header session1))
        (insert (detached--session-output session1))
        (when (eq 'terminal-data (detached--session-env-mode session1))
          ;; Enable `detached-log-mode' to parse ansi-escape sequences
          (detached-log-mode)))
      (with-current-buffer (get-buffer-create buffer2)
        (erase-buffer)
        (insert (detached--session-header session2))
        (insert (detached--session-output session2))
        (when (eq 'terminal-data (detached--session-env-mode session2))
          ;; Enable `detached-log-mode' to parse ansi-escape sequences
          (detached-log-mode)))
      (ediff-buffers buffer1 buffer2))))

;;;###autoload
(defun detached-detach-session ()
  "Detach from session in current buffer.

This command is only activated if `detached--buffer-session' is an
active session.  For sessions created with `detached-compile' or
`detached-shell-command', the command will also kill the window."
  (interactive)
  (if (detached-session-p detached--buffer-session)
      (if (eq major-mode 'detached-tail-mode)
          (detached-quit-tail-session)
          (if-let ((command-or-compile
                    (cond ((string-match detached--shell-command-buffer (buffer-name)) t)
                          ((string-match "\*detached-compilation" (buffer-name)) t)
                          ((eq major-mode 'detached-log-mode) t)
                          ((eq major-mode 'detached-tail-mode) t)
                          (t nil))))
              ;; `detached-shell-command' or `detached-compile'
              (let ((kill-buffer-query-functions nil))
                (when-let ((process (get-buffer-process (current-buffer))))
                  (comint-simple-send process detached--dtach-detach-character)
                  (message "[detached]"))
                (setq detached--buffer-session nil)
                (kill-buffer-and-window))
            (if (eq 'active (detached--determine-session-state detached--buffer-session))
                ;; `detached-eshell'
                (if-let ((process (and (eq major-mode 'eshell-mode)
                                       (detached-eshell-get-dtach-process))))
                    (progn
                      (setq detached--buffer-session nil)
                      (process-send-string process detached--dtach-detach-character))
                  ;; `detached-shell'
                  (let ((process (get-buffer-process (current-buffer))))
                    (comint-simple-send process detached--dtach-detach-character)
                    (setq detached--buffer-session nil)))
              (message "No active detached-session found in buffer."))))
    (message "No detached-session found in buffer.")))

;;;###autoload
(defun detached-delete-sessions (&optional all-hosts)
  "Delete `detached' sessions which belong to the current host, unless ALL-HOSTS."
  (interactive "P")
  (let* ((host-name (car (detached--host)))
         (sessions (if all-hosts
                       (detached-get-sessions)
                     (seq-filter (lambda (it)
                                   (string= (car (detached--session-host it)) host-name))
                                 (detached-get-sessions)))))
    (seq-do #'detached--db-remove-entry sessions)))

;;;###autoload
(defun detached-quit-tail-session ()
  "Quit `detached' tail session.

The log can have been updated, but that is not done by the user but
rather the tail mode.  To avoid a promtp `buffer-modified-p' is set to
nil before closing."
  (interactive)
  (set-buffer-modified-p nil)
  (setq detached--buffer-session nil)
  (kill-buffer-and-window))

;;;; Functions

;;;;; Session

(defun detached-create-session (command)
  "Create a `detached' session from COMMAND."
  (detached-connection-local-variables
   (detached--create-session-directory)
   (let ((session
          (detached--session-create :id (intern (detached--create-id command))
                                  :command command
                                  :origin detached-session-origin
                                  :action detached-session-action
                                  :working-directory (detached--get-working-directory)
                                  :attachable (detached-attachable-command-p command)
                                  :time `(:start ,(time-to-seconds (current-time)) :end 0.0 :duration 0.0 :offset 0.0)
                                  :status '(unknown . 0)
                                  :size 0
                                  :directory (if detached-local-session detached-session-directory
                                               (concat (file-remote-p default-directory) detached-session-directory))
                                  :env-mode (detached--env-mode command)
                                  :host (detached--host)
                                  :metadata (detached-metadata)
                                  :state 'unknown)))
     (detached--db-insert-entry session)
     (detached--watch-session-directory (detached--session-directory session))
     session)))

;;;###autoload
(defun detached-start-session (command &optional suppress-output)
  "Start a `detached' session running COMMAND.

Optionally SUPPRESS-OUTPUT."
  (let ((inhibit-message t)
        (detached-enabled t)
        (detached--current-session
         (or detached--current-session
             (detached-create-session command))))
    (if-let ((run-in-background
              (and (or suppress-output
                       (eq detached-session-mode 'create)
                       (not (detached--session-attachable detached--current-session)))))
             (detached-session-mode 'create))
        (progn (setq detached-enabled nil)
               (if detached-local-session
                   (apply #'start-process-shell-command
                          `("detached" nil ,(detached-dtach-command detached--current-session t)))
                 (apply #'start-file-process-shell-command
                        `("detached" nil ,(detached-dtach-command detached--current-session t)))))
      (cl-letf* ((detached-session-mode 'create-and-attach)
                 ((symbol-function #'set-process-sentinel) #'ignore)
                 (buffer (get-buffer-create detached--shell-command-buffer)))
        (when (get-buffer-process buffer)
          (setq buffer (generate-new-buffer (buffer-name buffer))))
        (setq detached-enabled nil)
        (funcall #'async-shell-command (detached-dtach-command detached--current-session t) buffer)
        (with-current-buffer buffer (setq detached--buffer-session detached--current-session))))))

(defun detached-session-candidates (sessions)
  "Return an alist of SESSIONS candidates."
  (when sessions
    (setq detached--annotation-widths
          (detached--annotation-widths sessions detached-annotation-format))
    (let ((command-length
           (thread-last sessions
                        (seq-map #'detached--session-command)
                        (seq-map #'length)
                        (seq-max)
                        (min (plist-get detached-command-format ':width)))))
      (let ((command-fun (plist-get detached-command-format ':function)))
        (setq detached--session-candidates
              (thread-last sessions
                           (seq-map (lambda (it)
                                      `(,(apply command-fun `(,it ,command-length))
                                        . ,it)))
                           (detached--session-deduplicate)
                           (seq-map (lambda (it)
                                      `(,(concat (car it)
                                                 (make-string (plist-get detached-command-format :padding) ?\s))
                                        . ,(cdr it))))))))))

(defun detached-session-annotation (item)
  "Associate ITEM to a session and return ts annotation."
  (let ((session (cdr (assoc item detached--session-candidates))))
    (mapconcat
     #'identity
     (cl-loop for annotation in detached-annotation-format
              collect (let ((str (funcall (plist-get annotation :function) session))
                            (width (alist-get (plist-get annotation :function) detached--annotation-widths)))
                        (when (> width 0)
                          (concat
                           (truncate-string-to-width
                            (propertize str 'face (plist-get annotation :face))
                            width
                            0 ?\s)
                           (make-string (plist-get annotation :padding) ?\s)
                           ))))
     "")))

;;;###autoload
(defun detached-initialize-sessions ()
  "Initialize `detached' sessions from the database."

  ;; Initialize sessions
  (unless detached--sessions-initialized
    (unless (file-exists-p detached-db-directory)
      (make-directory detached-db-directory t))
    (detached--db-initialize)
    (setq detached--db-watch
      (file-notify-add-watch detached-db-directory
                             '(change attribute-change)
                             #'detached--db-directory-event))
    (setq detached--sessions-initialized t)

    ;; Remove missing local sessions
    (thread-last (detached--db-get-sessions)
                 (seq-filter (lambda (it) (eq 'local (cdr (detached--session-host it)))))
                 (seq-filter #'detached--session-missing-p)
                 (seq-do #'detached--db-remove-entry))

    ;; Validate sessions with unknown state
    (detached--validate-unknown-sessions)

    ;; Update transitioned sessions
    (thread-last (detached--db-get-sessions)
                 (seq-filter (lambda (it) (eq 'active (detached--session-state it))))
                 (seq-remove (lambda (it) (when (detached--session-missing-p it)
                                       (detached--db-remove-entry it)
                                       t)))
                 (seq-filter #'detached--state-transition-p)
                 (seq-do #'detached--session-state-transition-update))

    ;; Watch session directories with active sessions
    (thread-last (detached--db-get-sessions)
                 (seq-filter (lambda (it) (eq 'active (detached--session-state it))))
                 (seq-map #'detached--session-directory)
                 (seq-uniq)
                 (seq-do #'detached--watch-session-directory))))

(defun detached-valid-session (session)
  "Ensure that SESSION is valid.

If session is not valid trigger an automatic cleanup on SESSION's host."
  (when (detached-session-p session)
    (if (not (detached--session-missing-p session))
        t
      (let ((host (detached--session-host session)))
        (message "Session does not exist. Initiate sesion cleanup on host %s" (car host))
        (detached--cleanup-host-sessions host)
        nil))))

(defun detached-session-exit-code-status (session)
  "Return status based on exit-code in SESSION."
  (if (null detached-env)
      `(unknown . 0)
    (let ((detached-env-message
           (with-temp-buffer
             (insert-file-contents (detached--session-file session 'log))
             (goto-char (point-max))
             (thing-at-point 'line t)))
          (success-message "Detached session finished")
          (failure-message (rx "Detached session exited abnormally with code " (group (one-or-more digit)))))
      (cond ((string-match success-message detached-env-message) `(success . 0))
            ((string-match failure-message detached-env-message)
             `(failure . ,(string-to-number (match-string 1 detached-env-message))))
            (t `(unknown . 0))))))

(defun detached-state-transitionion-echo-message (session)
  "Issue a notification when SESSION transitions from active to inactive.
This function uses the echo area."
  (let ((status (pcase (car (detached--session-status session))
                  ('success "Detached finished")
                  ('failure "Detached failed")
                  ('unknown "Detached finished"))))
    (message "%s [%s]: %s" status (car (detached--session-host session)) (detached--session-command session))))

(defun detached-state-transition-notifications-message (session)
  "Issue a notification when SESSION transitions from active to inactive.
This function uses the `notifications' library."
  (let ((status (car (detached--session-status session)))
        (host (car (detached--session-host session))))
    (notifications-notify
     :title (pcase status
              ('success (format "Detached finished [%s]" host))
              ('failure (format "Detached failed [%s]" host))
              ('unknown (format "Detached finished [%s]" host)))
     :body (detached--session-command session)
     :urgency (pcase status
                ('success 'normal)
                ('failure 'critical)
                ('unknown 'normal)))))

(defun detached-view-dwim (session)
  "View SESSION in a do what I mean fashion."
  (let ((status (car (detached--session-status session))))
    (cond ((eq 'success status)
           (detached-view-session session))
          ((eq 'failure status)
           (detached-compile-session session))
          ((eq 'unknown status)
           (detached-view-session session))
          (t (message "Detached session is in an unexpected state.")))))

(defun detached-get-sessions ()
  "Return validated sessions."
  (detached--validate-unknown-sessions)
  (detached--db-get-sessions))

(defun detached-shell-command-attach-session (session)
  "Attach to SESSION with `async-shell-command'."
  (let* ((detached--current-session session)
         (detached-session-mode 'attach)
         (inhibit-message t))
    (if (not (detached--session-attachable session))
        (detached-tail-session session)
      (cl-letf* (((symbol-function #'set-process-sentinel) #'ignore)
                 (buffer (get-buffer-create detached--shell-command-buffer))
                 (default-directory (detached--session-working-directory session))
                 (dtach-command (detached-dtach-command session t)))
        (when (get-buffer-process buffer)
          (setq buffer (generate-new-buffer (buffer-name buffer))))
        (funcall #'async-shell-command dtach-command buffer)
        (with-current-buffer buffer (setq detached--buffer-session detached--current-session))))))

;;;;; Other

(cl-defgeneric detached-dtach-command (entity &optional concat)
  "Return dtach command for ENTITY optionally CONCAT.")

(cl-defgeneric detached-dtach-command ((command string) &optional concat)
  "Return dtach command for COMMAND.

Optionally CONCAT the command return command into a string."
  (detached-dtach-command (detached-create-session command) concat))

(cl-defgeneric detached-dtach-command ((session detached-session) &optional concat)
  "Return dtach command for SESSION.

Optionally CONCAT the command return command into a string."
  (detached-connection-local-variables
   (let* ((detached-session-mode (cond ((eq detached-session-mode 'attach) 'attach)
                                     ((not (detached--session-attachable session)) 'create)
                                     (t detached-session-mode)))
          (socket (detached--session-file session 'socket t))
          (log (detached--session-file session 'log t))
          (dtach-arg (detached--dtach-arg)))
     (setq detached--buffer-session session)
     (if (eq detached-session-mode 'attach)
         (if concat
             (mapconcat #'identity
                        `(,(when detached-show-output-on-attach
                             (concat detached-show-output-command " " log ";"))
                          ,detached-dtach-program
                          ,dtach-arg
                          ,socket
                          "-r none")
                        " ")
           (append
            (when detached-show-output-on-attach
              `(,detached-show-output-command  ,(concat log ";")))
            `(,detached-dtach-program ,dtach-arg ,socket "-r" "none")))
       (if concat
           (mapconcat #'identity
                      `(,detached-dtach-program
                        ,dtach-arg
                        ,socket "-z"
                        ,detached-shell-program "-c"
                        ,(shell-quote-argument (detached--detached-command session)))
                      " ")
         `(,detached-dtach-program
           ,dtach-arg ,socket "-z"
                      ,detached-shell-program "-c"
                      ,(detached--detached-command session)))))))

(defun detached-attachable-command-p (command)
  "Return t if COMMAND is attachable."
  (if (thread-last detached-nonattachable-commands
                   (seq-filter (lambda (regexp)
                                 (string-match-p regexp command)))
                   (length)
                   (= 0))
      t
    nil))

(defun detached-metadata ()
  "Return a property list with metadata."
  (let ((metadata '()))
    (seq-doseq (annotator detached-metadata-annotators-alist)
      (push `(,(car annotator) . ,(funcall (cdr annotator))) metadata))
    metadata))

(defun detached-completing-read (sessions)
  "Select a session from SESSIONS through `completing-read'."
  (let* ((candidates (detached-session-candidates sessions))
         (metadata `(metadata
                     (category . detached)
                     (cycle-sort-function . identity)
                     (display-sort-function . identity)
                     (annotation-function . detached-session-annotation)
                     (affixation-function .
                                          ,(lambda (cands)
                                             (seq-map (lambda (s)
                                                        `(,s nil ,(detached-session-annotation s)))
                                                      cands)))))
         (collection (lambda (string predicate action)
                       (if (eq action 'metadata)
                           metadata
                         (complete-with-action action candidates string predicate))))
         (cand (completing-read "Select session: " collection nil t)))
    (detached--decode-session cand)))

(defun detached-command-str (session max-length)
  "Return SESSION's command as a string restrict it to MAX-LENGTH."
  (let ((command (detached--session-command session)))
    (if (<= (length command) max-length)
        (truncate-string-to-width
         command
         max-length
         0 ?\s)
      (concat (substring (detached--session-command session) 0 (- max-length 3)) "..."))))

;;;; Support functions

;;;;; Session

(defun detached--session-pid (session)
  "Return SESSION's pid."
  (let* ((socket
          (expand-file-name
           (concat (symbol-name (detached--session-id session)) ".socket")
           (or
            (file-remote-p default-directory 'localname)
            default-directory))))
    (car
     (split-string
      (with-temp-buffer
        (apply #'process-file `("pgrep" nil t nil "-f" ,(shell-quote-argument (format "dtach -. %s" socket))))
        (buffer-string))
      "\n" t))))

(defun detached--determine-session-state (session)
  "Return t if SESSION is active."
  (if (file-exists-p
       (detached--session-file session 'socket))
      'active
    'inactive))

(defun detached--state-transition-p (session)
  "Return t if SESSION has transitioned from active to inactive."
  (and
   (eq 'active (detached--session-state session))
   (eq 'inactive (detached--determine-session-state session))))

(defun detached--session-missing-p (session)
  "Return t if SESSION is missing."
  (not
   (file-exists-p
    (detached--session-file session 'log))))

(defun detached--session-header (session)
  "Return header for SESSION."
  (mapconcat
   #'identity
   `(,(format "Command: %s" (detached--session-command session))
     ,(format "Working directory: %s" (detached--working-dir-str session))
     ,(format "Host: %s" (car (detached--session-host session)))
     ,(format "Id: %s" (symbol-name (detached--session-id session)))
     ,(format "Status: %s" (car (detached--session-status session)))
     ,(format "Exit-code: %s" (cdr (detached--session-status session)))
     ,(format "Metadata: %s" (detached--metadata-str session))
     ,(format "Created at: %s" (detached--creation-str session))
     ,(format "Duration: %s\n" (detached--duration-str session))
     "")
   "\n"))

(defun detached--session-deduplicate (sessions)
  "Make car of SESSIONS unique by adding an identifier to it."
  (let* ((ht (make-hash-table :test #'equal :size (length sessions)))
         (occurences
          (thread-last sessions
                       (seq-group-by #'car)
                       (seq-map (lambda (it) (seq-length (cdr it))))
                       (seq-max)))
         (identifier-width (if (> occurences 1)
                               (+ (length (number-to-string occurences)) 3)
                             0))
         (reverse-sessions (seq-reverse sessions)))
    (dolist (session reverse-sessions)
      (if-let (count (gethash (car session) ht))
          (setcar session (format "%s%s" (car session)
                                  (truncate-string-to-width
                                   (propertize (format " (%s)" (puthash (car session) (1+ count) ht)) 'face 'detached-identifier-face)
                                   identifier-width 0 ?\s)))
        (puthash (car session) 0 ht)
        (setcar session (format "%s%s" (car session) (make-string identifier-width ?\s)))))
    (seq-reverse reverse-sessions)))

(defun detached--decode-session (item)
  "Return the session assicated with ITEM."
  (cdr (assoc item detached--session-candidates)))

(defun detached--validate-unknown-sessions ()
  "Validate `detached' sessions with state unknown."
  (thread-last (detached--db-get-sessions)
               (seq-filter (lambda (it) (eq 'unknown (detached--session-state it))))
               (seq-do (lambda (it)
                         (if (detached--session-missing-p it)
                             (detached--db-remove-entry it)
                           (setf (detached--session-state it) 'active)
                           (detached--db-update-entry it))))))

(defun detached--session-file (session file &optional local)
  "Return the full path to SESSION's FILE.

Optionally make the path LOCAL to host."
  (let* ((file-name
          (concat
           (symbol-name
            (detached--session-id session))
           (pcase file
             ('socket ".socket")
             ('log ".log"))))
         (remote-local-path (file-remote-p (expand-file-name file-name (detached--session-directory session)) 'localname))
         (full-path (expand-file-name file-name (detached--session-directory session))))
    (if (and local remote-local-path)
        remote-local-path
      full-path)))

(defun detached--cleanup-host-sessions (host)
  "Run cleanuup on HOST sessions."
  (let ((host-name (car host)))
    (thread-last (detached--db-get-sessions)
                 (seq-filter (lambda (it) (string= host-name (car (detached--session-host it)))))
                 (seq-filter #'detached--session-missing-p)
                 (seq-do #'detached--db-remove-entry))))

(defun detached--session-output (session)
  "Return content of SESSION's output."
  (let* ((filename (detached--session-file session 'log))
         (detached-message (rx (regexp "\n?\nDetached session ") (or "finished" "exited"))))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (let ((beginning (point))
            (end (if (search-forward-regexp detached-message nil t)
                     (match-beginning 0)
                   (point-max))))
        (buffer-substring beginning end)))))

(defun detached--create-session-directory ()
  "Create session directory if it doesn't exist."
  (let ((directory
         (concat
          (file-remote-p default-directory)
          detached-session-directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))))

(defun detached--get-working-directory ()
  "Return an abbreviated working directory path."
  (if-let (remote (file-remote-p default-directory))
      (replace-regexp-in-string  (expand-file-name remote)
                                 (concat remote "~/")
                                 (expand-file-name default-directory))
    (abbreviate-file-name default-directory)))

;;;;; Database

(defun detached--db-initialize ()
  "Return all sessions stored in database."
  (let ((db (expand-file-name "detached.db" detached-db-directory)))
    (when (file-exists-p db)
      (with-temp-buffer
        (insert-file-contents db)
        (cl-assert (bobp))
        (when (string= (detached--db-session-version) detached-session-version)
          (setq detached--sessions
                (read (current-buffer))))))))

(defun detached--db-session-version ()
  "Return `detached-session-version' from database."
  (let ((header (thing-at-point 'line))
        (regexp (rx "Detached Session Version: " (group (one-or-more (or digit punct))))))
    (string-match regexp header)
    (match-string 1 header)))

(defun detached--db-insert-entry (session)
  "Insert SESSION into `detached--sessions' and update database."
  (detached-initialize-sessions)
  (push `(,(detached--session-id session) . ,session) detached--sessions)
  (detached--db-update-sessions))

(defun detached--db-remove-entry (session)
  "Remove SESSION from `detached--sessions', delete log and update database."
  (detached-initialize-sessions)
  (let ((log (detached--session-file session 'log)))
    (when (file-exists-p log)
      (delete-file log)))
  (setq detached--sessions
        (assq-delete-all (detached--session-id session) detached--sessions))
  (detached--db-update-sessions))

(defun detached--db-update-entry (session &optional update)
  "Update SESSION in `detached--sessions' optionally UPDATE database."
  (detached-initialize-sessions)
  (setf (alist-get (detached--session-id session) detached--sessions) session)
  (when update
    (detached--db-update-sessions)))

(defun detached--db-get-session (id)
  "Return session with ID."
  (detached-initialize-sessions)
  (alist-get id detached--sessions))

(defun detached--db-get-sessions ()
  "Return all sessions stored in the database."
  (detached-initialize-sessions)
  (seq-map #'cdr detached--sessions))

(defun detached--db-update-sessions ()
  "Write `detached--sessions' to database."
  (detached-initialize-sessions)
  (let ((db (expand-file-name "detached.db" detached-db-directory)))
    (with-temp-file db
      (insert (format ";; Detached Session Version: %s\n\n" detached-session-version))
      (prin1 detached--sessions (current-buffer)))))

;;;;; Other

(defun detached--dtach-arg ()
  "Return dtach argument based on `detached-session-mode'."
  (pcase detached-session-mode
    ('create "-n")
    ('create-and-attach "-c")
    ('attach "-a")
    (_ (error "`detached-session-mode' has an unknown value"))))

(defun detached--session-state-transition-update (session)
  "Update SESSION due to state transition."
  ;; Update session
  (let ((session-size (file-attribute-size
                       (file-attributes
                        (detached--session-file session 'log))))
        (session-time (detached--update-session-time session) )
        (status-fun (or (plist-get (detached--session-action session) :status)
                        #'detached-session-exit-code-status)))
    (setf (detached--session-size session) session-size)
    (setf (detached--session-time session) session-time)
    (setf (detached--session-state session) 'inactive)
    (setf (detached--session-status session) (funcall status-fun session)))

  ;; Send notification
  (funcall detached-notification-function session)

  ;; Update session in database
  (detached--db-update-entry session t)

  ;; Execute callback
  (when-let ((callback (plist-get (detached--session-action session) :callback)))
    (funcall callback session)))

(defun detached--kill-processes (pid)
  "Kill PID and all of its children."
  (let ((child-processes
         (split-string
          (with-temp-buffer
            (apply #'process-file `("pgrep" nil t nil "-P" ,pid))
            (buffer-string))
          "\n" t)))
    (seq-do (lambda (pid) (detached--kill-processes pid)) child-processes)
    (apply #'process-file `("kill" nil nil nil ,pid))))

(defun detached--detached-command (session)
  "Return the detached command for SESSION.

If SESSION is nonattachable fallback to a command that doesn't rely on tee."
  (let* ((log (detached--session-file session 'log t))
         (begin-shell-group (if (string= "fish" (file-name-nondirectory detached-shell-program))
                                "begin;"
                              "{"))
         (end-shell-group (if (or (string= "fish" (file-name-nondirectory detached-shell-program)))
                              "end"
                            "}"))
         (redirect
          (if (detached--session-attachable session)
              (format "2>&1 | tee %s" log)
            (format "&> %s" log)))
         (env (if detached-env detached-env (format "%s -c" detached-shell-program)))
         (command
          (if detached-env
              (concat (format "%s " (detached--session-env-mode session))
                      (shell-quote-argument (detached--session-command session)))
            (shell-quote-argument (detached--session-command session)))))
    (format "%s %s %s; %s %s" begin-shell-group env command end-shell-group redirect)))

(defun detached--env-mode (command)
  "Return mode to run in `detached-env' based on COMMAND."
  (if (seq-find (lambda (regexp)
                  (string-match-p regexp command))
                detached-env-plain-text-commands)
      'plain-text
    'terminal-data))

(defun detached--host ()
  "Return a cons with (host . type)."
  (let ((remote (file-remote-p default-directory)))
    `(,(if remote (file-remote-p default-directory 'host) (system-name)) . ,(if remote 'remote 'local))))

(defun detached--ansi-color-tail ()
  "Apply `ansi-color' on tail output."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region auto-revert-tail-pos (point-max))))

(defun detached--update-session-time (session &optional approximate)
  "Update SESSION's time property.

If APPROXIMATE, use latest modification time of SESSION's
log to deduce the end time."
  (let* ((start-time (plist-get (detached--session-time session) :start))
         (end-time))
    (if approximate
        (setq end-time
              (time-to-seconds
               (file-attribute-modification-time
                (file-attributes
                 (detached--session-file session 'log)))))
      (setq end-time (time-to-seconds)))
    `(:start ,start-time :end ,end-time :duration ,(- end-time start-time))))

(defun detached--create-id (command)
  "Return a hash identifier for COMMAND."
  (let ((current-time (current-time-string)))
    (secure-hash 'md5 (concat command current-time))))

(defun detached--detached-env-message-filter (str)
  "Remove `detached-env' message in STR."
  (replace-regexp-in-string "\n?Detached session.*\n?" "" str))

(defun detached--dtach-eof-message-filter (str)
  "Remove `detached--dtach-eof-message' in STR."
  (replace-regexp-in-string (format "\n?%s\^M\n" detached--dtach-eof-message) "" str))

(defun detached--dtach-detached-message-filter (str)
  "Remove `detached--dtach-detached-message' in STR."
  (replace-regexp-in-string (format "\n?%s\n" detached--dtach-detached-message) "" str))

(defun detached--watch-session-directory (session-directory)
  "Watch for events in SESSION-DIRECTORY."
  (unless (alist-get session-directory detached--watched-session-directories
                     nil nil #'string=)
    (push
     `(,session-directory . ,(file-notify-add-watch
                              session-directory
                              '(change)
                              #'detached--session-directory-event))
     detached--watched-session-directories)))

(defun detached--session-directory-event (event)
  "Act on an EVENT in a directory in `detached--watched-session-directories'.

If event is caused by the deletion of a socket, locate the related
session and trigger a state transition."
  (pcase-let* ((`(,_ ,action ,file) event))
    (when (and (eq action 'deleted)
               (string= "socket" (file-name-extension file)))
      (when-let* ((id (intern (file-name-base file)))
                  (session (detached--db-get-session id))
                  (session-directory (detached--session-directory session)))

        ;; Update session
        (detached--session-state-transition-update session)

        ;; Remove session directory from `detached--watch-session-directory'
        ;; if there is no active session associated with the directory
        (unless
            (thread-last (detached--db-get-sessions)
                         (seq-filter (lambda (it) (eq 'active (detached--session-state it))))
                         (seq-map #'detached--session-directory)
                         (seq-uniq)
                         (seq-filter (lambda (it) (string= it session-directory))))
          (file-notify-rm-watch
           (alist-get session-directory detached--watched-session-directories))
          (setq detached--watched-session-directories
                (assoc-delete-all session-directory detached--watched-session-directories)))))))

(defun detached--db-directory-event (event)
  "Act on EVENT in `detached-db-directory'.

If event is cased by an update to the `detached' database, re-initialize
`detached--sessions'."
  (pcase-let* ((`(,_descriptor ,action ,file) event)
               (database-updated  (and (string= "detached.db" file)
                                       (eq 'attribute-changed action))))
    (when database-updated)
    (detached--db-initialize)))

(defun detached--annotation-widths (sessions annotation-format)
  "Return widths for ANNOTATION-FORMAT based on SESSIONS."
  (seq-map (lambda (it) (detached--annotation-width sessions it)) annotation-format))

(defun detached--annotation-width (sessions annotation)
  "Determine width for ANNOTATION based on SESSIONS."
  (let ((annotation-fun (plist-get annotation ':function))
        (width (plist-get annotation ':width)))
    `(,annotation-fun .
                      ,(thread-last sessions
                                    (seq-map annotation-fun)
                                    (seq-map #'length)
                                    (seq-max)
                                    (min width)))))

;;;;; UI

(defun detached--metadata-str (session)
  "Return SESSION's metadata as a string."
  (string-join
   (thread-last (detached--session-metadata session)
                (seq-filter (lambda (it) (cdr it)))
                (seq-map
                 (lambda (it)
                   (concat (symbol-name (car it)) ": " (cdr it)))))
   ""))

(defun detached--duration-str (session)
  "Return SESSION's duration time."
  (let* ((duration (if (eq 'active (detached--session-state session))
                       (- (time-to-seconds) (plist-get (detached--session-time session) :start))
                     (plist-get
                      (detached--session-time session) :duration)))
         (time (round duration))
         (hours (/ time 3600))
         (minutes (/ (mod time 3600) 60))
         (seconds (mod time 60)))
    (cond ((> time (* 60 60)) (format "%sh %sm %ss" hours minutes seconds))
          ((> time 60) (format "%sm %ss" minutes seconds))
          (t (format "%ss" seconds)))))

(defun detached--creation-str (session)
  "Return SESSION's creation time."
  (format-time-string
   "%b %d %H:%M"
   (plist-get
    (detached--session-time session) :start)))

(defun detached--size-str (session)
  "Return the size of SESSION's output."
  (if (eq 'active (detached--session-state session))
      ""
      (file-size-human-readable
       (detached--session-size session))))

(defun detached--status-str (session)
  "Return string if SESSION has failed."
  (pcase (car (detached--session-status session))
    ('failure "!")
    ('success "")
    ('unknown "")))

(defun detached--state-str (session)
  "Return string based on SESSION state."
  (if (eq 'active (detached--session-state session))
      "*"
    ""))

(defun detached--working-dir-str (session)
  "Return working directory of SESSION."
  (let ((working-directory
         (detached--session-working-directory session)))
    (if-let ((remote (file-remote-p working-directory)))
        (string-remove-prefix remote working-directory)
      working-directory)))

(defun detached--host-str (session)
  "Return host name of SESSION."
  (car (detached--session-host session)))

;;;; Minor modes

(defvar detached-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    map)
  "Keymap for `detached-shell-mode'.")

;;;###autoload
(define-minor-mode detached-shell-mode
  "Integrate `detached' in `shell-mode'."
  :lighter " detached-shell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if detached-shell-mode
      (dolist (filter detached-shell-mode-filter-functions)
        (add-hook 'comint-preoutput-filter-functions filter 0 t))
     (dolist (filter detached-shell-mode-filter-functions)
        (remove-hook 'comint-preoutput-filter-functions filter t))))

;;;; Major modes

(defvar detached-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    map)
  "Keymap for `detached-log-mode'.")

;;;###autoload
(define-derived-mode detached-log-mode nil "Detached Log"
  "Major mode for `detached' logs."
  (when detached-filter-ansi-sequences
    (comint-carriage-motion (point-min) (point-max))
    (set-buffer-modified-p nil)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (read-only-mode t))

(defvar detached-tail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    map)
  "Keymap for `detached-tail-mode'.")

;;;###autoload
(define-derived-mode detached-tail-mode auto-revert-tail-mode "Detached Tail"
  "Major mode to tail `detached' logs."
  (setq-local auto-revert-interval detached-tail-interval)
  (setq-local tramp-verbose 1)
  (setq-local auto-revert-remote-files t)
  (defvar revert-buffer-preserve-modes)
  (setq-local revert-buffer-preserve-modes nil)
  (auto-revert-set-timer)
  (setq-local auto-revert-verbose nil)
  (auto-revert-tail-mode)
  (when detached-filter-ansi-sequences
    (comint-carriage-motion (point-min) (point-max))
    (set-buffer-modified-p nil)
    (add-hook 'after-revert-hook #'detached--ansi-color-tail nil t)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (read-only-mode t))

(provide 'detached)

;;; detached.el ends here
