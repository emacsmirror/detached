;;; dtache-dired.el --- Dtache integration for dired -*- lexical-binding: t -*-

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

;; This package integrates `dtache' with `dired'.

;;; Code:

;;;; Requirements

(require 'dired)
(require 'dtache)

;;;; Functions

;;;###autoload
(defun dtache-dired-do-shell-command (dired-do-shell-command &rest args)
  "Ensure `dtache' is used before running DIRED-DO-SHELL-COMMAND with ARGS."
  (cl-letf* ((dtache-session-origin 'dired)
             ((symbol-function #'dired-run-shell-command)
              (lambda (command)
                (dtache-start-session command)
                nil)))
    (apply dired-do-shell-command args)))

(provide 'dtache-dired)

;;; dtache-dired.el ends here
