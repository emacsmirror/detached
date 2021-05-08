;;; dtache-shell.el --- Shell integration of dtache -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://www.gitlab.com/niklaseklund/dtache.git
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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

;; This package provides integration of dtache into shell-mode buffers.

;;;; Usage

;; `dtache-shell-enable': Enable dtache in shell buffers

;;; Code:
;;;; Requirements

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'dtache)
(require 'comint)
(require 's)
(require 'shell)

;;;; Functions

(defun dtache-shell-enable ()
  "Enable `dtache-shell'."
  (add-hook 'shell-mode-hook #'dtache-shell-maybe-activate)
  (advice-add 'shell :around #'dtache-shell--disable-histfile))

(defun dtache-shell-disable ()
  "Disable `dtache-shell'."
  (remove-hook 'shell-mode-hook #'dtache-shell-maybe-activate)
  (advice-remove 'shell #'dtache-shell--disable-histfile))

(defun dtache-shell-maybe-activate ()
  "Only local sessions are supported."
  (unless (file-remote-p default-directory)
    (dtache-shell-mode)))

;;;; Commands

;;;###autoload
(defun dtache-shell-create-session (&optional disable-block)
  "Create a new dtache session.
Use prefix argument DISABLE-BLOCK to force the launch of a session."
  (interactive "P")
  (let ((comint-input-sender #'dtache-shell-input-sender)
        (dtache-block-list (if disable-block '() dtache-block-list)))
    (comint-send-input)))

;;;###autoload
(defun dtache-shell-detach ()
  "Detach from an attached session."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (input "\C-\\"))
    (if (dtache-shell--attached-p)
        (comint-simple-send proc input)
      (message "Not attached to a session"))))

(defun dtache-shell-input-sender (proc string)
  "Create a dtache command based on STRING and send to PROC.

The function doesn't create dtache sessions when STRING is matching
any regexp found in `dtache-block-list'."
  (if-let* ((no-child-process (not (process-running-child-p (get-process (buffer-name)))))
            (allowed (not (--find (s-matches-p it string) dtache-block-list)))
            (session (dtache-create-session (substring-no-properties string)))
            (command (dtache-session-command session)))
      (comint-simple-send proc command)
    (comint-simple-send proc string)))

;;;; Support functions

(defun dtache-shell--attached-p ()
  "Return t if `shell' is attached to a session."
  (let ((pid (process-running-child-p (get-process (buffer-name)))))
    (when pid
      (let-alist (process-attributes pid)
	    (s-equals-p "dtach" .comm)))))

(defun dtache-shell--filter-dtach-eof (string)
  "Remove eof message from dtach in STRING."
  (if (string-match dtache-eof-message string)
      (s-replace (format "%s\n" (s-replace "\\" "" dtache-eof-message)) "" string)
    string))

(defun dtache-shell--disable-histfile (orig-fun &rest args)
  "Disable HISTFILE before calling ORIG-FUN with ARGS."
  (cl-letf (((getenv "HISTFILE") ""))
    (apply orig-fun args)))

(defun dtache-shell--save-history ()
  "Save `shell' history."
  (comint-write-input-ring))

;;;; Minor mode

(define-minor-mode dtache-shell-mode
  "Integrate `dtache' in shell-mode."
  :lighter "dtache-shell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if dtache-shell-mode
      (progn
        (dtache-cleanup-sessions)
        (add-hook 'comint-preoutput-filter-functions #'dtache-shell--filter-dtach-eof 0 t)
        (add-hook 'kill-buffer-hook #'dtache-shell--save-history 0 t))
    (remove-hook 'comint-preoutput-filter-functions #'dtache-shell--filter-dtach-eof t)
    (remove-hook 'kill-buffer-hook #'dtache-shell--save-history t)))

(provide 'dtache-shell)

;;; dtache-shell.el ends here
