;;; dtache-shell.el --- Dtache integration in shell -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://www.gitlab.com/niklaseklund/dtache.git
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience processes

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

(defvar dtache-shell-block-list '("^$")
  "A list of regexps to block non-supported input.")
(defvar dtache-shell-silence-dtach-messages t
  "Filter out messages from the `dtach' program.")
(defvar dtache-shell-create-primary-function #'dtache-shell-new-session
  "Primary function for creating a session.")
(defvar dtache-shell-create-secondary-function #'dtache-shell-create-session
  "Secondary function for creating a session.")

;;;; Functions

(defun dtache-shell-filter-dtach-eof (string)
  "Remove eof message from dtach in STRING."
  (if (string-match dtache-eof-message string)
      (replace-regexp-in-string (format "%s\n" dtache-eof-message) "" string)
    string))

(defun dtache-shell-filter-dtach-detached (string)
  "Remove detached message from dtach in STRING."
  (if (string-match dtache-detached-message string)
      (replace-regexp-in-string (format "%s\n" dtache-detached-message) "" string)
    string))

;;;; Commands

;;;###autoload
(defun dtache-shell-send-input (&optional create-session)
  "Send input to `shell'.

Optionally CREATE-SESSION with prefix argument."
  (interactive "P")
  (if create-session
      (funcall dtache-shell-create-primary-function)
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-create (&optional secondary)
  "Create a new session with `dtache-shell-create-primary-function'.

If prefix argument SECONDARY call `dtache-shell-create-secondary-function'."
  (interactive "P")
  (if secondary
      (funcall dtache-shell-create-secondary-function)
    (funcall dtache-shell-create-primary-function)))

;;;###autoload
(defun dtache-shell-create-session ()
  "Create a session and attach to it."
  (interactive)
  (let* ((dtache--dtach-mode "-c")
         (comint-input-sender #'dtache-shell--create-input-sender))
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-new-session ()
  "Create a new session."
  (interactive)
  (let ((dtache--dtach-mode "-n")
        (comint-input-sender #'dtache-shell--create-input-sender))
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-detach ()
  "Detach from session."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (input dtache-detach-character))
    (comint-simple-send proc input)))

;;;###autoload
(defun dtache-shell-attach (session)
  "Attach to SESSION.

`comint-add-to-input-history' is temporarily disabled to avoid
cluttering the comint-history with dtach commands."
  (interactive
   (list (dtache-select-session)))
  (cl-letf ((dtache--current-session session)
            (comint-input-sender #'dtache-shell--attach-input-sender)
            ((symbol-function 'comint-add-to-input-history) (lambda (_) t)))
    (comint-kill-input)
    (comint-send-input)))

;;;; Support functions

(cl-defmethod dtache--attach-to-session (session &context (major-mode shell-mode))
  "Attach to a dtache SESSION when MAJOR-MODE is `shell-mode'."
  (dtache-shell-attach session))

(defun dtache-shell--attach-input-sender (proc _string)
  "Attach to `dtache--session' and send the attach command to PROC."
  (let* ((dtache--dtach-mode "-a")
         (socket
          (concat
           (dtache--session-session-directory dtache--current-session)
           (dtache--session-id dtache--current-session)
           dtache-socket-ext))
         (input
          (concat dtache-program " " dtache--dtach-mode " " socket)))
    (comint-simple-send proc input)))

(defun dtache-shell--create-input-sender (proc string)
  "Create a dtache session based on STRING and send to PROC."
  (with-connection-local-variables
   (if-let* ((supported-input
              (not (seq-find
                    (lambda (blocked)
                      (string-match-p string blocked))
                    dtache-shell-block-list)))
             (command (dtache-dtach-command
                       (dtache--create-session
                        (substring-no-properties string)))))
       (comint-simple-send proc command)
     (comint-simple-send proc string))))

;;;; Minor mode

(define-minor-mode dtache-shell-mode
  "Integrate `dtache' in shell-mode."
  :lighter "dtache-shell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (with-connection-local-variables
   (if dtache-shell-mode
       (progn
         (dtache-db-initialize)
         (dtache-create-session-directory)
         (dtache-cleanup-sessions)
         (when dtache-shell-silence-dtach-messages
           (add-hook 'comint-preoutput-filter-functions #'dtache-shell-filter-dtach-eof 0 t)
           (add-hook 'comint-preoutput-filter-functions #'dtache-shell-filter-dtach-detached 0 t)))
     (when dtache-shell-silence-dtach-messages
       (remove-hook 'comint-preoutput-filter-functions #'dtache-shell-filter-dtach-eof t)
       (remove-hook 'comint-preoutput-filter-functions #'dtache-shell-filter-dtach-detached t)))))

(provide 'dtache-shell)

;;; dtache-shell.el ends here
