;;; dtache-compile.el --- Dtache integration with compile -*- lexical-binding: t -*-

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

;; This package integrates `dtache' with `compile'.

;;; Code:

;;;; Requirements

(require 'compile)
(require 'dtache)

;;;; Variables

(defvar dtache-compile-command nil
  "This variable has value t if `compile' is supposed to run with `dtache'.")
(defvar dtache-compile-session-action '(:attach dtache-compile-attach :view dtache-compile-session))

;;;; Commands

;;;###autoload
(defun dtache-compile ()
  "Run COMMAND through `compile' but in a 'dtache' session.
Optionally enable COMINT if prefix-argument is provided."
  (interactive)
  (let* ((dtache-compile-command t)
         (dtache-session-action dtache-compile-session-action)
         (dtache-session-type 'compile)
         (dtache--dtach-mode 'create))
    (call-interactively #'compile)))

;;;###autoload
(defun dtache-compile-recompile (&optional edit-command)
  "Re-compile by running `compile' but in a 'dtache' session.
Optionally EDIT-COMMAND."
  (interactive)
  (let* ((dtache-compile-command t)
         (dtache-session-action dtache-compile-session-action)
         (dtache-session-type 'compile)
         (dtache--dtach-mode 'create))
    (recompile edit-command)))

;;;;; Functions

(defun dtache-compile-advice (compilation-start &rest args)
  "Optionally create a `dtache' session before running COMPILATION-START with ARGS."
  (if (not dtache-compile-command)
      (apply compilation-start args)
    (pcase-let ((`(,command ,mode ,_ ,highlight-regexp) args)
                (buffer-name "*dtache-compilation*"))
      (if (and (not (eq dtache--dtach-mode 'attach))
               (dtache-redirect-only-p command))
          (dtache-start-session command t)
        (cl-letf* ((name-function (lambda (_) buffer-name))
                   (dtache--current-session (or dtache--current-session
                                                (dtache-create-session command)))
                   (dtache-command (dtache-dtach-command dtache--current-session t)))
          (apply compilation-start `(,dtache-command
                                     ,(or mode 'dtache-compilation-mode)
                                     ,name-function
                                     ,highlight-regexp)))))))

(defun dtache-compile-maybe-start (_proc)
  "Maybe run when compilation starts."
  (when dtache-compile-command
    (setq dtache--buffer-session dtache--current-session)
    (dtache-compile--replace-modesetter)
    (add-hook 'comint-preoutput-filter-functions #'dtache--dtache-env-message-filter 0 t)
    (add-hook 'comint-preoutput-filter-functions #'dtache--dtach-eof-message-filter 0 t)))

(defun dtache-compile-attach (session)
  "Attach to SESSION with `compile'."
  (when (dtache-valid-session session)
    (let* ((dtache-compile-command t)
           (dtache--dtach-mode 'attach)
           (dtache--current-session session))
      (compilation-start nil))))

(defun dtache-compile-open (session)
  "Open SESSION with `dtache-compile'."
  (when (dtache-valid-session session)
    (if (dtache--session-active session)
        (dtache-compile-attach session)
      (dtache-compile-session session))))

;;;###autoload
(defun dtache-compile-setup ()
  "Setup `dtache-compile'."
  (advice-add #'compilation-start :around #'dtache-compile-advice)
  (add-hook 'compilation-start-hook #'dtache-compile-maybe-start))

;;;;; Support functions

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
