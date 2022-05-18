;;; detached-vterm.el --- Detached integration with vterm -*- lexical-binding: t -*-

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

;; This package integrates `detached' with `vterm'

;;; Code:

;;;; Requirements

(require 'detached)

(declare-function vterm-send-C-a "vterm")
(declare-function vterm-send-C-k "vterm")
(declare-function vterm-send-C-e "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-end-of-line "vterm")

(defvar vterm--process)

;;;; Commands

;;;###autoload
(defun detached-vterm-send-input (&optional detach)
  "Create a `detached' session.

Optionally DETACH from it."
  (interactive)
  (vterm-send-C-a)
  (let* ((input (buffer-substring-no-properties (point) (vterm-end-of-line)))
         (detached-session-origin 'vterm)
         (detached-session-action
          '(:attach detached-shell-command-attach-session
                    :view detached-view-dwim
                    :run detached-shell-command))
         (detached-session-mode
          (if detach 'create 'create-and-attach)))
    (vterm-send-C-k)
    (process-send-string vterm--process (detached-dtach-command input t))
    (vterm-send-C-e)
    (vterm-send-return)))

;;;###autoload
(defun detached-vterm-attach (session)
  "Attach to an active `detached' SESSION."
  (interactive
   (list
    (let* ((host-name (car (detached--host)))
           (sessions
            (thread-last (detached-get-sessions)
                         (seq-filter (lambda (it)
                                       (string= (car (detached--session-host it)) host-name)))
                         (seq-filter (lambda (it) (eq 'active (detached--determine-session-state it)))))))
      (detached-completing-read sessions))))
  (let ((detached-session-mode 'attach))
    (process-send-string vterm--process (detached-dtach-command session t))
    (vterm-send-return)))

;;;###autoload
(defun detached-vterm-detach ()
  "Detach from a `detached' session."
  (interactive)
  (process-send-string vterm--process detached--dtach-detach-character))

;;;; Minor mode

(defvar detached-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") #'detached-vterm-send-input)
    (define-key map (kbd "<C-return>") #'detached-vterm-attach)
    (define-key map (kbd detached-detach-key) #'detached-vterm-detach)
    map)
  "Keymap for `detached-vterm-mode'.")

;;;###autoload
(define-minor-mode detached-vterm-mode
  "Integrate `detached' in `vterm'."
  :lighter " detached-vterm"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(provide 'detached-vterm)

;;; detached-vterm.el ends here
