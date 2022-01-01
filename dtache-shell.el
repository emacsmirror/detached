;;; dtache-shell.el --- Dtache integration in shell -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://www.gitlab.com/niklaseklund/dtache.git
;; Version: 0.2
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

;; This package provides integration of `dtache' in `shell'.

;;; Code:

;;;; Requirements

(require 'dtache)

;;;; Variables

(defvar dtache-shell-history-file nil
  "File to store history.")
(defvar dtache-shell-block-list '("^$")
  "A list of regexps to block non-supported input.")
(defvar dtache-shell-new-block-list '("^sudo.*")
  "A list of regexps to block from creating a session without attaching.")
(defconst dtache-shell-detach-character "\C-\\"
  "Character used to detach from a session.")

;;;;; Private

(defvar dtache-shell--current-session nil "The current session.")

;;;; Functions

(defun dtache-shell-override-history (orig-fun &rest args)
  "Override history to read `dtache-shell-history-file' in ORIG-FUN with ARGS.

This function also makes sure that the HISTFILE is disabled for local shells."
  (cl-letf (((getenv "HISTFILE") ""))
    (advice-add 'comint-read-input-ring :around #'dtache-shell--comint-read-input-ring-advice)
    (apply orig-fun args)))

(defun dtache-shell-save-history ()
  "Add hook to save history when killing `shell' buffer."
  (add-hook 'kill-buffer-hook #'dtache-shell-save-history 0 t))

(defun dtache-shell-setup ()
  "Setup `dtache-shell'."
  (add-hook 'shell-mode-hook #'dtache-shell-save-history)
  (add-hook 'shell-mode-hook #'dtache-shell-mode)
  (advice-add 'shell :around #'dtache-shell-override-history))

(defun dtache-shell-select-session ()
  "Return selected session."
  (let* ((current-host (dtache--host))
         (sessions
          (thread-last (dtache-get-sessions)
            (seq-filter (lambda (it)
                          (string= (dtache--session-host it) current-host)))
            (seq-filter #'dtache--session-active-p))))
    (dtache-completing-read sessions)))

;;;; Commands

;;;###autoload
(defun dtache-shell-create-session (&optional detach)
  "Create a session and attach to it unless DETACH."
  (interactive "P")
  (let* ((dtache-session-type 'shell)
         (dtache--dtach-mode (if detach 'new 'create))
         (comint-input-sender #'dtache-shell--create-input-sender))
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-new-session ()
  "Create a new session."
  (interactive)
  (let ((dtache-session-type 'shell)
        (dtache--dtach-mode 'new)
        (comint-input-sender #'dtache-shell--create-input-sender))
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-detach ()
  "Detach from session."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (input dtache-shell-detach-character))
    (comint-simple-send proc input)))

;;;###autoload
(defun dtache-shell-attach (session)
  "Attach to SESSION.

`comint-add-to-input-history' is temporarily disabled to avoid
cluttering the comint-history with dtach commands."
  (interactive
   (list (dtache-shell-select-session)))
  (if (dtache--session-active-p session)
      (cl-letf ((dtache-shell--current-session session)
                (comint-input-sender #'dtache-shell--attach-input-sender)
                ((symbol-function 'comint-add-to-input-history) (lambda (_) t)))
        (comint-kill-input)
        (comint-send-input))
    (dtache-open-session session)))

;;;; Support functions

(defun dtache-shell--attach-input-sender (proc _string)
  "Attach to `dtache--session' and send the attach command to PROC."
  (let* ((dtache--dtach-mode 'attach)
         (socket (dtache-session-file dtache-shell--current-session 'socket t))
         (input
          (format "%s %s %s" dtache-dtach-program (dtache--dtach-arg) socket)))
    (comint-simple-send proc input)))

(defun dtache-shell--create-input-sender (proc string)
  "Create a dtache session based on STRING and send to PROC."
  (with-connection-local-variables
   (if-let* ((supported-input
              (not (seq-find
                    (lambda (blocked)
                      (string-match-p blocked string))
                    dtache-shell-block-list)))
             (dtache--dtach-mode
              (if (seq-find
                   (lambda (blocked)
                     (string-match-p blocked string))
                   dtache-shell-new-block-list)
                  'create
                dtache--dtach-mode))
             (command (dtache-dtach-command (substring-no-properties string)))
             (shell-command
              (mapconcat 'identity `(,dtache-dtach-program
                                     ,@(butlast command)
                                     ,(shell-quote-argument (car (last command))))
                         " ")))
       (comint-simple-send proc shell-command)
     (comint-simple-send proc string))))

(defun dtache-shell--comint-read-input-ring-advice (orig-fun &rest args)
  "Set `comint-input-ring-file-name' before calling ORIG-FUN with ARGS."
  (with-connection-local-variables
   (let ((comint-input-ring-file-name
          (concat
           (file-remote-p default-directory)
           dtache-shell-history-file)))
     (apply orig-fun args)
     (advice-remove 'comint-read-input-ring #'dtache-shell--comint-read-input-ring-advice))))

(defun dtache-shell--save-history ()
  "Save `shell' history."
  (with-connection-local-variables
   (let ((comint-input-ring-file-name
          (concat
           (file-remote-p default-directory)
           dtache-shell-history-file)))
     (comint-write-input-ring))))

;;;; Minor mode

(define-minor-mode dtache-shell-mode
  "Integrate `dtache' in shell-mode."
  :lighter "dtache-shell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (with-connection-local-variables
   (if dtache-shell-mode
       (progn
         (add-hook 'comint-preoutput-filter-functions #'dtache--dtache-env-message-filter 0 t)
         (add-hook 'comint-preoutput-filter-functions #'dtache--dtach-eof-message-filter 0 t))
     (remove-hook 'comint-preoutput-filter-functions #'dtache--dtache-env-message-filter t)
     (remove-hook 'comint-preoutput-filter-functions #'dtache--dtach-eof-message-filter t))))

(provide 'dtache-shell)

;;; dtache-shell.el ends here
