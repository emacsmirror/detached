;;; dtache-extra.el --- Dtache integration for external packages -*- lexical-binding: t -*-

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

;; This package provides a collection of functionality to integrate `dtache' with external packages.

;;; Code:

;;;; Requirements

(declare-function dtache-compile "dtache")
(declare-function dtache-start-session "dtache")

(defvar dtache-session-origin)
(defvar dtache-local-session)

;;;; Functions

;;;###autoload
(defun dtache-extra-projectile-run-compilation (cmd &optional use-comint-mode)
  "If CMD is a string execute it with `dtache-compile', optionally USE-COMINT-MODE."
  (if (functionp cmd)
      (funcall cmd)
    (let ((dtache-session-origin 'projectile))
      (dtache-compile cmd use-comint-mode))))

;;;###autoload
(defun dtache-extra-dired-rsync (command _details)
    "Run COMMAND with `dtache'."
    (let ((dtache-local-session t)
          (dtache-session-origin 'rsync))
      (dtache-start-session command t)))

(provide 'dtache-extra)

;;; dtache-extra.el ends here
