;;; dtache-shell.el --- Dtache integration in shell -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Niklas Eklund

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
(defvar dtache-shell-session-action '(:attach dtache-shell-command-attach :view dtache-view-dwim :run dtache-shell-command)
  "Actions for a session created with `dtache-shell'.")

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

;;;###autoload
(defun dtache-shell-setup ()
  "Setup `dtache-shell'."
  (dtache-setup)
  (add-hook 'shell-mode-hook #'dtache-shell-save-history)
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
  (let* ((dtache-session-origin 'shell)
         (dtache-session-action dtache-shell-session-action)
         (dtache-session-mode (if detach 'new 'create))
         (comint-input-sender #'dtache-shell--create-input-sender))
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-attach (session)
  "Attach to SESSION.

`comint-add-to-input-history' is temporarily disabled to avoid
cluttering the comint-history with dtach commands."
  (interactive
   (list (dtache-shell-select-session)))
  (when (dtache-valid-session session)
    (if (and (dtache--session-active-p session)
             (not (dtache--session-redirect-only session)))
        (cl-letf ((dtache--current-session session)
                  (comint-input-sender #'dtache-shell--attach-input-sender)
                  ((symbol-function 'comint-add-to-input-history) (lambda (_) t)))
          (setq dtache--buffer-session session)
          (comint-kill-input)
          (comint-send-input))
      (dtache-open-session session))))

;;;; Support functions

(defun dtache-shell--attach-input-sender (proc _string)
  "Attach to `dtache--session' and send the attach command to PROC."
  (let* ((dtache-session-mode 'attach)
         (input
          (dtache-dtach-command dtache--current-session t)))
    (comint-simple-send proc input)))

(defun dtache-shell--create-input-sender (proc string)
  "Create a dtache session based on STRING and send to PROC."
  (with-connection-local-variables
   (let* ((command (substring-no-properties string))
          (dtache-session-mode (if (dtache-redirect-only-p command)
                                   'new
                                 'create))
          (dtach-command (dtache-dtach-command command t)))
     (comint-simple-send proc dtach-command))))

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

(provide 'dtache-shell)

;;; dtache-shell.el ends here
