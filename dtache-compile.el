;;; dtache-compile.el --- Dtache integration for compile -*- lexical-binding: t -*-

;; Copyright (C) 2022 Niklas Eklund
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

;; This is a `dtache' extension which provides integration for `compile'.

;;; Code:

;;;; Requirements

(require 'compile)
(require 'dtache)

;;;; Variables

(defcustom dtache-compile-session-action
  '(:attach dtache-compile-attach
            :view dtache-post-compile-session
            :run dtache-compile)
  "Actions for a session created with `dtache-compile'."
  :group 'dtache
  :type 'plist)

;;;; Commands

;;;###autoload
(defun dtache-compile (command &optional comint)
  "Run COMMAND through `compile' but in a 'dtache' session.
Optionally enable COMINT if prefix-argument is provided."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (let* ((dtache-enabled t)
         (dtache-session-action dtache-compile-session-action)
         (dtache-session-origin 'compile)
         (dtache-session-mode 'create-and-attach))
    (compile command comint)))

;;;###autoload
(defun dtache-compile-recompile (&optional edit-command)
  "Re-compile by running `compile' but in a 'dtache' session.
Optionally EDIT-COMMAND."
  (interactive "P")
  (let* ((dtache-enabled t)
         (dtache-session-action dtache-compile-session-action)
         (dtache-session-origin 'compile)
         (dtache-session-mode 'create-and-attach))
    (recompile edit-command)))

;;;;; Functions

(defun dtache-compile-attach (session)
  "Attach to SESSION with `compile'."
  (when (dtache-valid-session session)
    (let* ((dtache-enabled t)
           (dtache-session-mode 'attach)
           (dtache--current-session session))
      (compilation-start (dtache--session-command session)))))

(defun dtache-compile-open (session)
  "Open SESSION with `dtache-compile'."
  (when (dtache-valid-session session)
    (if (eq 'active (dtache--session-state session))
        (dtache-compile-attach session)
      (dtache-post-compile-session session))))

;;;###autoload
(defun dtache-compile-setup ()
  "Setup `dtache-compile'."
  (dtache-setup)
  (advice-add #'compilation-start :around #'dtache-compile--compilation-start)
  (add-hook 'compilation-start-hook #'dtache-compile--start))

;;;;; Support functions

(defun dtache-compile--compilation-start (compilation-start &rest args)
  "Optionally create a `dtache' session before running COMPILATION-START with ARGS."
  (if dtache-enabled
      (pcase-let ((`(,command ,mode ,_ ,highlight-regexp) args)
                  (buffer-name "*dtache-compilation*"))
        (if (and (not (eq dtache-session-mode 'attach))
                 (not (dtache-attachable-command-p command)))
            (dtache-start-session command t)
          (cl-letf* ((name-function (lambda (_) buffer-name))
                     (dtache--current-session (or dtache--current-session
                                                  (dtache-create-session command))))
            (apply compilation-start `(,(dtache-dtach-command dtache--current-session t)
                                       ,(or mode 'dtache-compilation-mode)
                                       ,name-function
                                       ,highlight-regexp)))))
    (apply compilation-start args)))

(defun dtache-compile--start (_)
  "Run in `compilation-start-hook' if `dtache-enabled'."
  (when dtache-enabled
    (setq dtache--buffer-session dtache--current-session)
    (dtache-compile--replace-modesetter)
    (add-hook 'comint-preoutput-filter-functions #'dtache--dtache-env-message-filter 0 t)
    (add-hook 'comint-preoutput-filter-functions #'dtache--dtach-eof-message-filter 0 t)))

(defun dtache-compile--replace-modesetter ()
  "Replace the modsetter inserted by `compilation-start'."
  (save-excursion
    (let ((buffer-read-only nil)
          (regexp (rx (regexp "^dtach ") (or "-c" "-a") (regexp ".*\.socket.*$"))))
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (kill-region (match-beginning 0) (match-end 0))
        (insert (dtache--session-command dtache--current-session))))))

(defun dtache-compile--compilation-dtache-filter ()
  "Filter to modify the output in a compilation buffer."
  (let ((begin compilation-filter-start)
        (end (copy-marker (point))))
    (save-excursion
      (goto-char begin)
      (when (re-search-forward "\n?Dtache session.*\n?" end t)
        (delete-region (match-beginning 0) (match-end 0))))))

(defun dtache-compile--compilation-eof-filter ()
  "Filter to modify the output in a compilation buffer."
  (let ((begin compilation-filter-start)
        (end (copy-marker (point))))
    (save-excursion
      (goto-char begin)
      (when (re-search-forward (format "\n?%s\n" dtache--dtach-eof-message) end t)
        (delete-region (match-beginning 0) (match-end 0))))))

;;;;; Major modes

;;;###autoload
(define-derived-mode dtache-compilation-mode compilation-mode "Dtache Compilation"
  "Major mode for tailing dtache logs."
  (add-hook 'compilation-filter-hook #'dtache-compile--compilation-eof-filter 0 t)
  (add-hook 'compilation-filter-hook #'dtache-compile--compilation-dtache-filter 0 t))

(provide 'dtache-compile)

;;; dtache-compile.el ends here
