;;; detached-eshell.el --- Detached integration for eshell -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

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

;; This is a `detached' extension which provides integration for `eshell'.

;;; Code:

;;;; Requirements

(require 'detached)
(require 'eshell)
(require 'esh-mode)
(require 'esh-ext)
(require 'em-hist)

;;;; Variables

(defcustom detached-eshell-session-action
  '(:attach detached-shell-command-attach-session
            :view detached-view-dwim
            :run detached-shell-command)
  "Actions for a session created with `detached-eshell'."
  :group 'detached
  :type 'plist)

;;;; Functions

(defun detached-eshell-select-session ()
  "Return selected session."
  (let* ((host-name (car (detached--host)))
         (sessions
          (thread-last (detached-get-sessions)
                       (seq-filter (lambda (it)
                                     (string= (car (detached--session-host it)) host-name)))
                       (seq-filter (lambda (it) (eq 'active (detached--determine-session-state it)))))))
    (detached-completing-read sessions)))

(defun detached-eshell-get-dtach-process ()
  "Return `eshell' process if `detached' is running."
  (when-let* ((process (and eshell-process-list (caar eshell-process-list))))
    (and (string= (process-name process) "dtach")
         process)))

;;;; Commands

;;;###autoload
(defun detached-eshell-send-input (&optional detach)
  "Create a session and attach to it.

If prefix-argument directly DETACH from the session."
  (interactive "P")
  (let* ((detached-session-origin 'eshell)
         (detached-session-mode (if detach 'create 'create-and-attach))
         (detached-enabled t)
         (detached--current-session nil))
    (advice-add #'eshell-external-command :around #'detached-eshell-external-command)
    (call-interactively #'eshell-send-input)))

;;;###autoload
(defun detached-eshell-attach-session (session)
  "Attach to SESSION."
  (interactive
   (list (detached-eshell-select-session)))
  (when (detached-valid-session session)
    (if (and (eq 'active (detached--determine-session-state session))
             (detached--session-attachable session))
        (cl-letf* ((detached-session-mode 'attach)
                   (input
                    (detached-dtach-command session t))
                   ((symbol-function #'eshell-add-to-history) #'ignore))
          (eshell-kill-input)
          ;; Hide the input from the user
          (let ((begin (point))
                (end))
            (insert input)
            (setq end (point))
            (overlay-put (make-overlay begin end) 'invisible t)
            (overlay-put (make-overlay end end) 'before-string "[attached]")
            (insert " "))
          (setq detached--buffer-session session)
          (call-interactively #'eshell-send-input))
      (detached-open-session session))))

;;;; Support functions

;;;###autoload
(defun detached-eshell-external-command (orig-fun &rest args)
  "Advice `eshell-external-command' to optionally use `detached'."
  (let* ((detached-session-action detached-eshell-session-action)
         (command (string-trim-right
                   (mapconcat #'identity
                              (flatten-list args)
                              " ")))
         (session (detached-create-session command))
         (command (detached-dtach-command session)))
    (advice-remove #'eshell-external-command #'detached-eshell-external-command)
    (setq detached--buffer-session session)
    (setq detached-enabled nil)
    (apply orig-fun `(,(seq-first command) ,(seq-rest command)))))

;;;; Minor mode

(defvar detached-eshell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") #'detached-eshell-send-input)
    (define-key map (kbd "<C-return>") #'detached-eshell-attach-session)
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    map)
  "Keymap for `detached-eshell-mode'.")

;;;###autoload
(define-minor-mode detached-eshell-mode
  "Integrate `detached' in `eshell-mode'."
  :lighter " detached-eshell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (make-local-variable 'eshell-preoutput-filter-functions)
  (if detached-eshell-mode
      (progn
        (add-hook 'eshell-preoutput-filter-functions #'detached--detached-env-message-filter)
        (add-hook 'eshell-preoutput-filter-functions #'detached--dtach-eof-message-filter))
    (remove-hook 'eshell-preoutput-filter-functions #'detached--detached-env-message-filter)
    (remove-hook 'eshell-preoutput-filter-functions #'detached--dtach-eof-message-filter)))

(provide 'detached-eshell)

;;; detached-eshell.el ends here
