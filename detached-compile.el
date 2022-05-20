;;; detached-compile.el --- Detached integration for compile -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

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

;; This is a `detached' extension which provides integration for `compile'.

;;; Code:

;;;; Requirements

(require 'compile)
(require 'detached)

(declare-function ansi-color-compilation-filter "ansi-color")

;;;; Variables

(defcustom detached-compile-session-action
  '(:attach detached-compile-attach
            :view detached-compile-session
            :run detached-compile)
  "Actions for a session created with `detached-compile'."
  :group 'detached
  :type 'plist)

;;;; Commands

;;;###autoload
(defun detached-compile (command &optional comint)
  "Run COMMAND through `compile' but in a 'detached' session.
Optionally enable COMINT if prefix-argument is provided."
  (interactive
   (list
    (let ((command (eval compile-command t)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (let* ((detached-enabled t)
         (detached-session-origin (or detached-session-origin 'compile))
         (detached-session-action (or detached-session-action
                                      detached-compile-session-action))
         (detached-session-mode 'create-and-attach))
    (compile command comint)))

;;;###autoload
(defun detached-compile-recompile (&optional edit-command)
  "Re-compile by running `compile' but in a 'detached' session.
Optionally EDIT-COMMAND."
  (interactive "P")
  (let* ((detached-enabled t)
         (detached-session-action detached-compile-session-action)
         (detached-session-origin 'compile)
         (detached-session-mode 'create-and-attach))
    (recompile edit-command)))

;;;;; Functions

;;;###autoload
(defun detached-compile-attach (session)
  "Attach to SESSION with `compile'."
  (when (detached-valid-session session)
    (let* ((detached-enabled t)
           (detached-session-mode 'attach)
           (detached--current-session session))
      (compilation-start (detached--session-command session)))))

;;;###autoload
(defun detached-compile-open (session)
  "Open SESSION with `detached-compile'."
  (when (detached-valid-session session)
    (if (eq 'active (detached--session-state session))
        (detached-compile-attach session)
      (detached-compile-session session))))

;;;###autoload
(defun detached-compile-start (_)
  "Run in `compilation-start-hook' if `detached-enabled'."
  (when detached-enabled
    (setq detached--buffer-session detached--current-session)
    (detached-compile--replace-modesetter)
    (when detached-filter-ansi-sequences
      (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter 0 t))
    (add-hook 'comint-preoutput-filter-functions #'detached--detached-env-message-filter 0 t)
    (add-hook 'comint-preoutput-filter-functions #'detached--dtach-eof-message-filter 0 t)))

;;;;; Support functions

(defun detached-compile--compilation-start (compilation-start &rest args)
  "Create a `detached' session before running COMPILATION-START with ARGS."
  (if detached-enabled
      (pcase-let ((`(,command ,mode ,_ ,highlight-regexp) args)
                  (buffer-name "*detached-compilation*"))
        (if (and (not (eq detached-session-mode 'attach))
                 (not (detached-attachable-command-p command)))
            (detached-start-session command t)
          (cl-letf* ((name-function (lambda (_) buffer-name))
                     (detached--current-session (or detached--current-session
                                                    (detached-create-session command))))
            (apply compilation-start `(,(detached-dtach-command detached--current-session t)
                                       ,(or mode 'detached-compilation-mode)
                                       ,name-function
                                       ,highlight-regexp)))))
    (apply compilation-start args)))

(defun detached-compile--replace-modesetter ()
  "Replace the modsetter inserted by `compilation-start'."
  (save-excursion
    (let ((buffer-read-only nil)
          (regexp (rx (regexp "^dtach ") (or "-c" "-a") (regexp ".*\.socket.*$"))))
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (kill-region (match-beginning 0) (match-end 0))
        (insert (detached--session-command detached--current-session))))))

(defun detached-compile--compilation-detached-filter ()
  "Filter to modify the output in a compilation buffer."
  (let ((begin compilation-filter-start)
        (end (copy-marker (point))))
    (save-excursion
      (goto-char begin)
      (when (re-search-forward "\n?Detached session.*\n?" end t)
        (delete-region (match-beginning 0) (match-end 0))))))

(defun detached-compile--compilation-eof-filter ()
  "Filter to modify the output in a compilation buffer."
  (let ((begin compilation-filter-start)
        (end (copy-marker (point))))
    (save-excursion
      (goto-char begin)
      (when (re-search-forward (format "\n?%s\n" detached--dtach-eof-message) end t)
        (delete-region (match-beginning 0) (match-end 0))))))

;;;;; Major modes

(defvar detached-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    map)
  "Keymap for `detached-compilation-mode'.")

;;;###autoload
(define-derived-mode detached-compilation-mode compilation-mode "Detached Compilation"
  "Major mode for tailing `detached' logs."
  (add-hook 'compilation-filter-hook #'detached-compile--compilation-eof-filter 0 t)
  (add-hook 'compilation-filter-hook #'detached-compile--compilation-detached-filter 0 t))

(advice-add #'compilation-start :around #'detached-compile--compilation-start)

(provide 'detached-compile)

;;; detached-compile.el ends here
