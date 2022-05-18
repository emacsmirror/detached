;;; dtache-vterm.el --- Dtache integration with vterm -*- lexical-binding: t -*-

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

;; This package integrates `dtache' with `vterm'

;;; Code:

;;;; Requirements

(require 'dtache)

(declare-function vterm-send-C-a "vterm")
(declare-function vterm-send-C-k "vterm")
(declare-function vterm-send-C-e "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-end-of-line "vterm")

(defvar vterm--process)

;;;; Commands

;;;###autoload
(defun dtache-vterm-send-input (&optional detach)
  "Create a `dtache' session.

Optionally DETACH from it."
  (interactive)
  (vterm-send-C-a)
  (let* ((input (buffer-substring-no-properties (point) (vterm-end-of-line)))
         (dtache-session-origin 'vterm)
         (dtache-session-action
          '(:attach dtache-shell-command-attach-session
                    :view dtache-view-dwim
                    :run dtache-shell-command))
         (dtache-session-mode
          (if detach 'create 'create-and-attach)))
    (vterm-send-C-k)
    (process-send-string vterm--process (dtache-dtach-command input t))
    (vterm-send-C-e)
    (vterm-send-return)))

;;;###autoload
(defun dtache-vterm-attach (session)
  "Attach to an active `dtache' SESSION."
  (interactive
   (list
    (let* ((host-name (car (dtache--host)))
           (sessions
            (thread-last (dtache-get-sessions)
                         (seq-filter (lambda (it)
                                       (string= (car (dtache--session-host it)) host-name)))
                         (seq-filter (lambda (it) (eq 'active (dtache--determine-session-state it)))))))
      (dtache-completing-read sessions))))
  (let ((dtache-session-mode 'attach))
    (process-send-string vterm--process (dtache-dtach-command session t))
    (vterm-send-return)))

;;;###autoload
(defun dtache-vterm-detach ()
  "Detach from a `dtache' session."
  (interactive)
  (process-send-string vterm--process dtache--dtach-detach-character))

;;;; Minor mode

(defvar dtache-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") #'dtache-vterm-send-input)
    (define-key map (kbd "<C-return>") #'dtache-vterm-attach)
    (define-key map (kbd dtache-detach-key) #'dtache-vterm-detach)
    map)
  "Keymap for `dtache-vterm-mode'.")

;;;###autoload
(define-minor-mode dtache-vterm-mode
  "Integrate `dtache' in `vterm'."
  :lighter " dtache-vterm"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(provide 'dtache-vterm)

;;; dtache-vterm.el ends here
