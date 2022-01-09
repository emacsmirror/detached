;;; dtache-eshell.el --- Dtache integration in eshell -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Niklas Eklund

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

;; This package provides integration of `dtache' in `eshell'.

;;; Code:

;;;; Requirements

(require 'dtache)
(require 'eshell)
(require 'esh-mode)
(require 'em-hist)

;;;; Variables

(defvar dtache-eshell-session-action '(:attach dtache-shell-command-attach :view dtache-view-dwim :run dtache-shell-command))

;;;; Functions

;;;###autoload
(defun dtache-eshell-setup ()
  "Setup `dtache-eshell'."
  (dtache-setup)
  (add-hook 'eshell-prepare-command-hook #'dtache-eshell-maybe-create-session)
  (add-hook 'eshell-mode-hook #'dtache-eshell-mode))

(defun dtache-eshell-select-session ()
  "Return selected session."
  (let* ((current-host (dtache--host))
         (sessions
          (thread-last (dtache-get-sessions)
            (seq-filter (lambda (it)
                          (string= (dtache--session-host it) current-host)))
            (seq-filter #'dtache--session-active-p))))
    (dtache-completing-read sessions)))

(defun dtache-eshell-maybe-create-session ()
  "Create a session if `dtache-eshell-command' value is t."
  (when dtache-enabled
    (let* ((dtache-session-mode 'create)
           (dtache-session-action dtache-eshell-session-action)
           (command (mapconcat #'identity
                               `(,eshell-last-command-name
                                 ,@eshell-last-arguments)
                               " "))
           (session (dtache-create-session command)))
      (setq eshell-last-arguments (dtache-dtach-command session))
      (setq dtache--buffer-session session)
      (setq dtache-enabled nil)
      (setq eshell-last-command-name "dtach"))))

(defun dtache-eshell-get-dtach-process ()
  "Return `eshell' process if `dtache' is running."
  (when-let* ((process (and eshell-process-list (caar eshell-process-list))))
    (and (string= (process-name process) "dtach")
         process)))

;;;; Commands

;;;###autoload
(defun dtache-eshell-create-session (&optional detach)
  "Create a session and attach to it.

If prefix-argument directly DETACH from the session."
  (interactive "P")
  (let* ((dtache-session-origin 'eshell)
         (dtache-session-mode (if detach 'new 'create))
         (dtache-enabled t)
         (dtache--current-session nil))
    (call-interactively #'eshell-send-input)))

;;;###autoload
(defun dtache-eshell-attach (session)
  "Attach to SESSION."
  (interactive
   (list (dtache-eshell-select-session)))
  (when (dtache-valid-session session)
    (if (and (dtache--session-active-p session)
             (not (dtache--session-redirect-only session)))
        (cl-letf* ((dtache-session-mode 'attach)
                   (input
                    (dtache-dtach-command session t))
                   ((symbol-function #'eshell-add-to-history) #'ignore))
          (eshell-kill-input)
          ;; Hide the input from the user
          (let ((begin (point))
                (end))
            (insert input)
            (setq end (point))
            (overlay-put (make-overlay begin end) 'invisible t)
            (insert " "))
          (setq dtache--buffer-session session)
          (call-interactively #'eshell-send-input))
      (dtache-open-session session))))

;;;; Minor mode

;;;###autoload
(define-minor-mode dtache-eshell-mode
  "Integrate `dtache' in eshell-mode."
  :lighter "dtache-eshell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (make-local-variable 'eshell-preoutput-filter-functions)
  (if dtache-eshell-mode
      (progn
        (add-hook 'eshell-preoutput-filter-functions #'dtache--dtache-env-message-filter)
        (add-hook 'eshell-preoutput-filter-functions #'dtache--dtach-eof-message-filter))
    (remove-hook 'eshell-preoutput-filter-functions #'dtache--dtache-env-message-filter)
    (remove-hook 'eshell-preoutput-filter-functions #'dtache--dtach-eof-message-filter)))

(provide 'dtache-eshell)

;;; dtache-eshell.el ends here
