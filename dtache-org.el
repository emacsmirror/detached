;;; dtache-org.el --- Dtache integration for org -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

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

;; This package integrates `dtache' with `org'.  In particular it
;; integrates with `ob-shell' in order to detach babel src blocks.

;;; Code:

;;;; Requirements

(require 'dtache)
(require 'ob-shell)

;;;; Variables

(defcustom dtache-org-session-action
  '(:attach dtache-shell-command-attach-session
            :view dtache-view-dwim
            :run dtache-shell-command)
  "Actions for a session created with `dtache-org'."
  :group 'dtache
  :type 'plist)

;;;; Functions

;;;###autoload
(defun dtache-org-setup ()
  "Setup `dtache-org'."
  (dtache-setup)
  (advice-add #'org-babel-sh-evaluate :around #'dtache-org-babel-sh))

(defun dtache-org-babel-sh (org-babel-sh-evaluate-fun &rest args)
  "Modify ARGS before calling ORG-BABEL-SH-EVALUATE-FUN.

This function modifies the full-body in ARGS and replaces it with a
`dtache' command.  The functionality is enabled by setting a header
property of :dtache t in the org babel src block."
  (pcase-let* ((`(,session ,full-body ,params ,stdin ,cmdline) args))
    (if (alist-get :dtache params)
        (cl-letf* ((dtache-session-origin 'org)
                   (dtache-session-action dtache-org-session-action)
                   (dtache-session-mode 'create)
                   (new-command (replace-regexp-in-string "\n" " && " full-body))
                   (dtach-command
                    (if (string= "none" (alist-get :session params))
                        (dtache-dtach-command new-command t)
                      (format "%s\necho \"[detached]\"" (dtache-dtach-command new-command t))))
                   ((symbol-function #'org-babel-eval)
                    (lambda (_ command)
                      (start-file-process-shell-command "dtache-org" nil command)
                      "[detached]")))
          (apply org-babel-sh-evaluate-fun `(,session ,dtach-command ,params ,stdin ,cmdline)))
      (apply org-babel-sh-evaluate-fun args))))

(provide 'dtache-org)

;;; dtache-org.el ends here
