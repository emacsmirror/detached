;;; detached-list.el --- Manage detached sessions -*- lexical-binding: t -*-

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

;; This is an interface to manage `detached' sessions.

;;; Code:

;;;; Requirements

(require 'detached)
(require 'tabulated-list)

;;;; Variables

(defcustom detached-list-config
  `((:name "Command" :function detached--session-command :length 60)
    (:name "State" :function detached-list--state-str :length 10 :face detached-state-face)
    (:name "Status" :function detached-list--status-str :length 10 :face detached-failure-face)
    (:name "Host" :function detached--host-str :length 15 :face detached-host-face)
    (:name "Directory" :function detached--working-dir-str :length 40 :face detached-working-dir-face)
    (:name "Metadata" :function detached--metadata-str :length 30 :face detached-metadata-face)
    (:name "Duration" :function detached--duration-str :length 20 :face detached-duration-face)
    (:name "Created" :function detached--creation-str :length 20 :face detached-creation-face))
  "Configuration for `detached' list mode."
  :type '(repeat symbol)
  :group 'detached)

;;;; Private

(defvar-local detached-list--marked-sessions nil
  "A list of marked session ids.")
(defvar-local detached-list--filters nil
  "A list of filters to apply when displaying the sessions.")

;;;; Functions

(defun detached-list-imenu-index ()
  "Create an `imenu' index for `detached-list'."
  (let ((index))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((session (tabulated-list-get-id)))
        (push `(,(detached--session-command session) . ,(point))
              index))
      (forward-line 1))
    (seq-reverse index)))

(defun detached-list-eldoc (_callback)
  "A member of `eldoc-documentation-functions', for signatures."
  (let ((session (tabulated-list-get-id)))
    (when (detached-session-p session)
      (detached--session-command session))))

;;;; Commands

(defun detached-list-detach-from-session (session)
  "Detach from SESSION at point."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let* ((buffer (detached-list--attached-p session)))
    (unless (get-buffer-window buffer)
      (pop-to-buffer buffer))
    (with-selected-window (get-buffer-window buffer)
        (detached-detach-session))))

(defun detached-list-jump-to-directory (session)
  "Jump to SESSION at point's directory."
  (interactive
   (list (tabulated-list-get-id)))
  (detached-open-session-directory session))

(defun detached-list-copy-session-command (session)
  "Copy SESSION at point's command."
  (interactive
   (list (tabulated-list-get-id)))
  (detached-copy-session-command session))

(defun detached-list-copy-session-output (session)
  "Copy SESSION at point's output."
  (interactive
   (list (tabulated-list-get-id)))
  (detached-copy-session session))

(defun detached-list-kill-session ()
  "Send a TERM signal to sessions at point, or all marked sessions.

Optionally DELETE the session if prefix-argument is provided."
  (interactive)
  (when (y-or-n-p (if detached-list--marked-sessions
                      "Kill all marked sessions? "
                    "Kill session at point? "))
    (seq-do
     (lambda (session)
       (detached-list--unmark-session session)
       (detached-kill-session session current-prefix-arg))
     (detached-list--get-marked-or-current-sessions))
    (detached-list-revert)))

(defun detached-list-rerun-session (session &optional suppress-output)
  "Rerun SESSION at point.

Optionally SUPPRESS-OUTPUT."
  (interactive
   (list (tabulated-list-get-id)
         current-prefix-arg))
  (detached-rerun-session session suppress-output)
  (detached-list-revert))

(defun detached-list-diff-marked-sessions ()
  "Diff two sessions."
  (interactive)
  (if (= (length detached-list--marked-sessions) 2)
      (apply #'detached-diff-session detached-list--marked-sessions)
      (message "Mark two sessions")))

(defun detached-list-open-session ()
  "View session."
  (interactive)
  (detached-open-session
   (tabulated-list-get-id)))

(defun detached-list-narrow-host ()
  "Narrow to sessions from a selected host."
  (interactive)
  (when-let ((hostnames
              (thread-last (detached-list--get-filtered-sessions)
                           (seq-map #'detached--session-host)
                           (seq-map #'car)
                           (seq-uniq)))
             (hostname
              (completing-read
               "Select host: "
               hostnames))
             (multiple-hostnames (> (length hostnames) 1)))
    (detached-list-narrow-sessions
     `(,(concat "Host: " hostname) .
       ,(lambda (session)
          (string-match hostname
                        (car (detached--session-host session))))))))

(defun detached-list-narrow-regexp (regexp)
  "Narrow to sessions which command match REGEXP."
  (interactive
   (list (read-regexp
          "Filter session commands containing (regexp): ")))
  (when regexp
    (detached-list-narrow-sessions
     `(,(concat "Regexp: " regexp) .
       ,(lambda (session)
          (string-match regexp
                        (detached--session-command session)))))))

(defun detached-list-narrow-local ()
  "Narrow to local SESSIONS."
  (interactive)
  (detached-list-narrow-sessions
   `("Local" .
     ,(lambda (session)
        (detached--local-session-p session)))))

(defun detached-list-narrow-remote ()
  "Narrow to remote SESSIONS."
  (interactive)
  (detached-list-narrow-sessions
   `("Remote" .
     ,(lambda (session)
        (detached--remote-session-p session)))))

(defun detached-list-narrow-active ()
  "Narrow to active SESSIONS."
  (interactive)
  (detached-list-narrow-sessions
   `("Active" .
     ,(lambda (session)
        (detached--active-session-p session)))))

(defun detached-list-narrow-inactive ()
  "Narrow to inactive SESSIONS."
  (interactive)
  (detached-list-narrow-sessions
   `("Inactive" .
     ,(lambda (session)
        (null (detached--active-session-p session))))))

(defun detached-list-narrow-success ()
  "Narrow to successful SESSIONS."
  (interactive)
  (detached-list-narrow-sessions
   `("Success" .
     ,(lambda (session)
        (eq 'success (car (detached--session-status session)))))))

(defun detached-list-narrow-failure ()
  "Narrow to failed SESSIONS."
  (interactive)
  (detached-list-narrow-sessions
   `("Failure" .
     ,(lambda (session)
        (eq 'failure (car (detached--session-status session)))))))

(defun detached-list-mark-regexp (regexp)
  "Mark sessions which command match REGEXP.

If prefix-argument is provided unmark instead of mark."
  (interactive
   (list (read-regexp
          (concat (if current-prefix-arg "Unmark" "Mark")
                  " session commands containing (regexp): "))))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((session (tabulated-list-get-id)))
        (when (string-match regexp (detached--session-command session))
          (if current-prefix-arg
              (detached-list--unmark-session session)
            (detached-list--mark-session session))))
      (forward-line))))

(defun detached-list-delete-session ()
  "Delete session at point, or all marked sessions."
  (interactive)
  (when (y-or-n-p (if detached-list--marked-sessions
                      "Delete all marked sessions? "
                    "Delete session at point? "))
    (seq-do
     (lambda (session)
       (detached-list--unmark-session session)
       (detached-delete-session session))
     (detached-list--get-marked-or-current-sessions))
    (detached-list-revert)))

(defun detached-list-mark-session ()
  "Mark session at point and advance to next session."
  (interactive)
  (let* ((session (tabulated-list-get-id)))
    (detached-list--mark-session session)
    (forward-line)))

(defun detached-list-unmark-session ()
  "Unmark session at point and advance to next session."
  (interactive)
  (let* ((session (tabulated-list-get-id)))
    (detached-list--unmark-session session)
    (forward-line)))

(defun detached-list-unmark-sessions ()
  "Unmark all sessions."
  (interactive)
  (setq detached-list--marked-sessions nil)
  (detached-list-revert))

(defun detached-list-toggle-mark-session ()
  "Toggle mark on session at point."
  (interactive)
  (let* ((session (tabulated-list-get-id)))
    (if (detached-list--marked-session-p session)
        (detached-list--unmark-session session)
      (detached-list--mark-session session))))

(defun detached-list-toggle-sessions ()
  "Toggle mark on all sessions."
  (interactive)
  (let* ((sessions (seq-map #'car tabulated-list-entries))
         (unmarked-sessions
          (seq-remove
           (lambda (session)
             (seq-find
              (lambda (marked-session)
                (eq (detached--session-id marked-session)
                    (detached--session-id session)))
              detached-list--marked-sessions))
           sessions)))
    (setq detached-list--marked-sessions unmarked-sessions)
    (detached-list-revert)))

(defun detached-list-revert ()
  "Update content in buffer."
  (interactive)
  (tabulated-list-revert)
  (detached-list--restore-marks))

;;;###autoload
(defun detached-list-sessions ()
  "Open list of `detached'."
  (interactive)
  (let ((buffer (get-buffer-create "*detached-list*")))
    (pop-to-buffer-same-window buffer)
    (detached-list-mode)
    (setq tabulated-list-entries
          (seq-map #'detached-list--get-entry
                   (detached-list--get-filtered-sessions)))
    (tabulated-list-print t)))

(defun detached-list-narrow-sessions (filter)
  "Narrow session(s) based on FILTER."
  (let* ((current-filters `(,filter ,@detached-list--filters))
         (buffer (get-buffer-create
                  (format "*detached-list [%s]*"
                          (string-join
                           (thread-last current-filters
                                        (seq-reverse)
                                        (seq-map #'car))
                           " AND ")))))
    (pop-to-buffer-same-window buffer)
    (detached-list-mode)
    (setq detached-list--filters current-filters)
    (setq tabulated-list-entries
          (seq-map #'detached-list--get-entry
                   (detached-list--get-filtered-sessions)))
    (tabulated-list-print t)))

;;;; Support functions

(defun detached-list--revert-sessions ()
  "Recompute `tabulated-list-entries'."
  (setq tabulated-list-entries
        (seq-map #'detached-list--get-entry
                 (detached-list--get-filtered-sessions))))

(defun detached-list--get-entry (session)
  "Return list entry based on SESSION."
  `(,session
    ,(cl-loop for config in detached-list-config
              vconcat `(,
                        (let ((str (funcall (plist-get config ':function) session)))
                          (if-let ((face (plist-get config :face)))
                              (propertize str 'face face)
                            str))))))

(defun detached-list--get-format ()
  "Return the format for `detached-list'."
  (cl-loop for config in detached-list-config
           vconcat `((,(plist-get config ':name)
                      ,(plist-get config ':length)
                      ,(plist-get config ':sort)))))

(defun detached-list--marked-session-p (session)
  "Return t if SESSION is marked."
  (seq-find (lambda (it)
              (eq (detached--session-id it)
                  (detached--session-id session)))
            detached-list--marked-sessions))

(defun detached-list--attached-p (session)
  "Return t if Emacs is attached to SESSION."
  (let ((id (detached--session-id session)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (when-let ((buffer-session detached--buffer-session)
                    (buffer-session-id (detached--session-id buffer-session)))
           (eq buffer-session-id id))))
     (buffer-list))))

(defun detached-list--unmark-session (session)
  "Unmark SESSION."
  (when (detached-list--marked-session-p session)
    (tabulated-list-put-tag " ")
    (setq detached-list--marked-sessions
          (seq-remove (lambda (it)
                        (eq (detached--session-id it)
                            (detached--session-id session)))
                      detached-list--marked-sessions))))

(defun detached-list--mark-session (session)
  "Mark SESSION."
  (unless (detached-list--marked-session-p session)
    (tabulated-list-put-tag (detached-list--mark-identifier))
    (setq detached-list--marked-sessions
          (push session detached-list--marked-sessions))))

(defun detached-list--restore-marks ()
  "Restore mark(s) in `detached-list-mode' buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((session (tabulated-list-get-id)))
        (when (detached-list--marked-session-p session)
          (tabulated-list-put-tag (detached-list--mark-identifier))))
      (forward-line))))

(defun detached-list--mark-identifier ()
  "Return identifier for marked sessions."
  (let ((str "*"))
    (propertize str 'face 'detached-mark-face)))

(defun detached-list--status-str (session)
  "Return a string representation of SESSION's status."
  (let ((status (detached-session-status session)))
    (symbol-name status)))

(defun detached-list--state-str (session)
  "Return a string representation of SESSION's state."
  (symbol-name (detached--session-state session)))

(defun detached-list--get-marked-or-current-sessions ()
  "Return a list of relevant sessions."
  (or detached-list--marked-sessions
      `(,(tabulated-list-get-id))))

(defun detached-list--get-filtered-sessions ()
  "Return a list of filtered sessions."
  (thread-last (detached-get-sessions)
               (seq-filter (lambda (session)
                             (seq-every-p
                              (lambda (it) it)
                              (seq-map (lambda (filter)
                                        (funcall (cdr filter) session))
                                      detached-list--filters))))))

;;;; Major mode

(defvar detached-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'detached-list-delete-session)
    (define-key map (kbd "g") #'detached-list-revert)
    (define-key map (kbd "j") #'detached-list-jump-to-directory)
    (define-key map (kbd "k") #'detached-list-kill-session)
    (define-key map (kbd "m") #'detached-list-mark-session)
    (define-key map (kbd "n a") #'detached-list-narrow-active)
    (define-key map (kbd "n h") #'detached-list-narrow-host)
    (define-key map (kbd "n f") #'detached-list-narrow-failure)
    (define-key map (kbd "n i") #'detached-list-narrow-inactive)
    (define-key map (kbd "n l") #'detached-list-narrow-local)
    (define-key map (kbd "n r") #'detached-list-narrow-remote)
    (define-key map (kbd "n s") #'detached-list-narrow-success)
    (define-key map (kbd "n %") #'detached-list-narrow-regexp)
    (define-key map (kbd "r") #'detached-list-rerun-session)
    (define-key map (kbd "s") #'imenu)
    (define-key map (kbd "t") #'detached-list-toggle-mark-session)
    (define-key map (kbd "T") #'detached-list-toggle-sessions)
    (define-key map (kbd "u") #'detached-list-unmark-session)
    (define-key map (kbd "U") #'detached-list-unmark-sessions)
    (define-key map (kbd "w") #'detached-list-copy-session-command)
    (define-key map (kbd "W") #'detached-list-copy-session-output)
    (define-key map (kbd "x") #'detached-list-detach-from-session)
    (define-key map (kbd "%") #'detached-list-mark-regexp)
    (define-key map (kbd "=") #'detached-list-diff-marked-sessions)
    (define-key map (kbd "!") #'detached-shell-command)
    (define-key map (kbd "<return>") #'detached-list-open-session)
    map)
  "Keymap used in `detached-list-mode'.")

(define-derived-mode detached-list-mode tabulated-list-mode "Detached List"
  "Mode for `detached' list."
  (setq tabulated-list-format (detached-list--get-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq imenu-create-index-function #'detached-list-imenu-index)
  (add-hook 'eldoc-documentation-functions #'detached-list-eldoc nil t)
  (add-hook 'tabulated-list-revert-hook #'detached-list--revert-sessions nil t)
  (tabulated-list-init-header))

(provide 'detached-list)

;;; detached-list.el ends here
