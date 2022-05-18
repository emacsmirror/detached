;;; dtache-init.el --- Initialize dtache -*- lexical-binding: t -*-

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

;; This is intended to aid users in configuring `dtache's integration with other packages.

;;; Code:

;;;; Requirements

(require 'subr-x)

(declare-function dtache-attach-session "dtache")
(declare-function dtache-compile-session "dtache")
(declare-function dtache-delete-session "dtache")
(declare-function dtache-insert-session-command "dtache")
(declare-function dtache-kill-session "dtache")
(declare-function dtache-rerun-session "dtache")
(declare-function dtache-tail-session "dtache")
(declare-function dtache-view-session "dtache")
(declare-function dtache-copy-session-command "dtache")
(declare-function dtache-copy-session "dtache")
(declare-function dtache-diff-session "dtache")

(declare-function dtache-shell-mode "dtache")
(declare-function dtache-compile-start "dtache-compile")
(declare-function dtache-dired-do-shell-command "dtache-dired")
(declare-function dtache-eshell-mode "dtache-eshell")
(declare-function dtache-extra-projectile-run-compilation "dtache-extra")
(declare-function dtache-extra-dired-rsync "dtache-extra")
(declare-function dtache-org-babel-sh "dtache-org")
(declare-function dtache-shell-override-history "dtache-shell")
(declare-function dtache-shell-save-history-on-kill "dtache-shell")
(declare-function dtache-vterm-mode "dtache-vterm")

(declare-function org-babel-sh-evaluate "ob-shell")
(declare-function dired-rsync--do-run "dired-rsync")
(declare-function dired-rsync "dired-rsync")
(declare-function projectile "projectile")
(declare-function vterm "vterm")

(defvar embark-general-map)
(defvar embark-keymap-alist)

;;;; Variables

(defcustom dtache-init-block-list nil
  "A list of blocked packages."
  :group 'dtache
  :type 'list)

(defcustom dtache-init-allow-list
  '(compile dired dired-rsync embark eshell org projectile shell vterm)
  "A list of allowed packages."
  :group 'dtache
  :type 'list)

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

;;;; Support functions

(defvar dtache-init--package-integration '((compile . dtache-init-compile)
                                           (dired . dtache-init-dired)
                                           (dired-rsync . dtache-init-dired-rsync)
                                           (embark . dtache-init-embark)
                                           (eshell . dtache-init-eshell)
                                           (org . dtache-init-org)
                                           (projectile . dtache-init-projectile)
                                           (shell . dtache-init-shell)
                                           (vterm . dtache-init-vterm))
  "Alist which contain names of packages and their initialization function.")

;;;; Functions

;;;###autoload
(defun dtache-init ()
  "Initialize `dtache' integration with all packages."

  ;; Required for `dtache-shell-command' which is always provided
  (add-hook 'shell-mode-hook #'dtache-shell-mode)

  (let ((init-functions
         (thread-last dtache-init--package-integration
                      (seq-filter (lambda (it)
                                    (member (car it) dtache-init-allow-list)))
                      (seq-remove (lambda (it)
                                    (member (car it) dtache-init-block-list)))
                      (seq-map #'cdr))))
    (dolist (init-function init-functions)
            (funcall init-function))))

(defun dtache-init-shell ()
  "Initialize integration with `shell'."
  (advice-add #'shell :around #'dtache-shell-override-history)
  (add-hook 'shell-mode-hook #'dtache-shell-save-history-on-kill))

(defun dtache-init-compile ()
  "Initialize integration with `compile'."
  (add-hook 'compilation-start-hook #'dtache-compile-start)
  (add-hook 'compilation-shell-minor-mode-hook #'dtache-shell-mode))

(defun dtache-init-eshell ()
  "Initialize integration with `eshell'."
  (add-hook 'eshell-mode-hook #'dtache-eshell-mode))

(defun dtache-init-org ()
  "Initialize integration with `org'."
  (advice-add #'org-babel-sh-evaluate :around #'dtache-org-babel-sh))

(defun dtache-init-dired ()
  "Initialize integration with `dired'."
  (advice-add 'dired-do-shell-command :around #'dtache-dired-do-shell-command))

(defun dtache-init-dired-rsync ()
  "Initialize integration with `dired-rsync'."
  (when (functionp #'dired-rsync)
    (advice-add #'dired-rsync--do-run :override #'dtache-extra-dired-rsync)))

(defun dtache-init-projectile ()
  "Initialize integration with `projectile'."
  (when (functionp #'projectile)
    (advice-add 'projectile-run-compilation
                :override #'dtache-extra-projectile-run-compilation)))

(defun dtache-init-vterm ()
  "Initialize integration with `vterm'."
  (when (functionp #'vterm)
    (add-hook 'vterm-mode-hook #'dtache-vterm-mode)))

(defun dtache-init-embark ()
  "Initialize integration with `embark'."
  (with-eval-after-load 'embark
    (defvar embark-dtache-map (make-composed-keymap dtache-action-map embark-general-map))
    (add-to-list 'embark-keymap-alist '(dtache . embark-dtache-map))))

(provide 'dtache-init)

;;; dtache-init.el ends here
