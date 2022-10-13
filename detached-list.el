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
(require 'hl-line)
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
  :type '(repeat (plist :options ((:name symbol)
                                  (:function symbol)
                                  (:length symbol)
                                  (:face symbol))))
  :group 'detached)

(defcustom detached-list-display-buffer-action
  '(display-buffer-same-window
    (inhibit-same-window . nil))
  "The action used to display the detached list buffer."
  :group 'detached
  :type 'sexp)

(defcustom detached-list-open-session-display-buffer-action
  '(display-buffer-same-window
    (inhibit-same-window . nil))
  "The action used to display a detached session."
  :group 'detached
  :type 'sexp)

(defcustom detached-list-filters nil
  "An alist of custom filters that can be applied.

The filters are built using the different narrow functions that
detached list implements."
  :group 'detached
  :type '(alist :key-type string))

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
      (let ((strs `(,(detached--session-command session)
                    ,(when-let  ((annotation (detached--session-annotation session)))
                       (propertize annotation 'face 'detached-annotation-face)))))
        (string-join (seq-remove #'null strs) "\n")))))

;;;; Commands

(defun detached-list-initialize-session-directory (&optional all)
  "Initialize a session-directory.

Optionally initialize ALL session-directories."
  (interactive "P")
  (if-let* ((uninitialized-directories
               (thread-last (detached-get-sessions)
                            (seq-filter #'detached--uninitialized-session-p)
                            (seq-map #'detached--session-directory)
                            (seq-uniq))))
    (if all
        (seq-do #'detached-list--initialize-directory uninitialized-directories)
      (when-let ((directory (completing-read "Initialize directory: " uninitialized-directories)))
        (detached-list--initialize-directory directory)))
    (message "All session directories have been initialized")))

(defun detached-list-edit-annotation (session)
  "Edit SESSION's annotation."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let* ((initial-value (or
                     (detached--session-annotation session)
                     ""))
              (annotation (read-string "Annotation: " initial-value)))
    (setf (detached--session-annotation session) annotation)
    (detached--db-update-entry session)))

(defun detached-list-quit ()
  "Quit command."
  (interactive)
  (if (= (length (window-list)) 1)
      (bury-buffer)
    (delete-window)))

(defun detached-list-remove-narrow-criterion ()
  "Remove narrow criterion."
  (interactive)
  (if detached-list--filters
      (detached-list-narrow-sessions
       (cdr detached-list--filters))
    (message "No criterion to remove")))

(defun detached-list-widen ()
  "Remove all narrowing restrictions."
  (interactive)
  (when detached-list--filters
    (detached-list-narrow-sessions nil)))

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
  (when (eq 'create-and-attach (detached--session-initial-mode session))
    (when-let ((single-window (> (length (window-list)) 1))
               (buffer (current-buffer)))
      (delete-window (get-buffer-window))
      (bury-buffer buffer)))
  (detached-rerun-session session suppress-output))

(defun detached-list-diff-marked-sessions ()
  "Diff two sessions."
  (interactive)
  (if (= (length detached-list--marked-sessions) 2)
      (apply #'detached-diff-session detached-list--marked-sessions)
    (message "Mark two sessions")))

(defun detached-list-open-session ()
  "View session."
  (interactive)
  (let ((session (tabulated-list-get-id))
        (detached-open-session-display-buffer-action
         detached-list-open-session-display-buffer-action))
    (when-let ((single-window (> (length (window-list)) 1))
               (buffer (current-buffer)))
      (delete-window (get-buffer-window))
      (bury-buffer buffer))
    (detached-open-session session)))

(defun detached-list-narrow-host (hostname)
  "Narrow to sessions from a selected HOSTNAME."
  (interactive
   (list
    (when-let* ((hostnames
                 (thread-last (detached-list--get-filtered-sessions)
                              (seq-map #'detached--session-host)
                              (seq-map #'car)
                              (seq-uniq))))
      (completing-read
       "Select host: "
       hostnames))))
  (when hostname
    (detached-list-narrow-sessions
     `((,(concat "Host: " hostname) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (string-match hostname
                                       (car (detached--session-host it))))
                       sessions)))
       ,@detached-list--filters))))

(defun detached-list-narrow-output-regexp (regexp)
  "Narrow to sessions which output contain REGEXP."
  (interactive
   (list (read-regexp
          "Filter session outputs containing (regexp): ")))
  (when regexp
    (detached-list-narrow-sessions
     `((,(concat "Output: " regexp) .
        ,(lambda (sessions)
           (let* ((sessions-and-directories
                   (thread-last sessions
                                (seq-group-by #'detached--session-directory)
                                (seq-filter (lambda (it)
                                              ;; Filter out only accessible directories
                                              (or (not (file-remote-p (car it)))
                                                  (file-remote-p (car it) nil t))))))
                  (session-ids
                   (thread-last sessions-and-directories
                                (seq-map
                                 (lambda (it)
                                   (pcase-let* ((`(,session-directory . ,sessions) it)
                                                (default-directory session-directory)
                                                (includes
                                                 (seq-map (lambda (session)
                                                            (format "--include=%s"
                                                                    (file-name-nondirectory
                                                                     (detached--session-file
                                                                      session
                                                                      'log))))
                                                          sessions))
                                                (grep-command
                                                 (string-join `(,detached-grep-program
                                                                "--files-with-matches"
                                                                ,@includes
                                                                "--no-messages"
                                                                "--ignore-case"
                                                                "--recursive"
                                                                ,(format "\"%s\"" regexp))
                                                              " ")))
                                     (split-string
                                      (with-connection-local-variables
                                       (with-temp-buffer
                                         (process-file-shell-command grep-command nil t)
                                         (buffer-string)))
                                      "\n" t))))
                                (flatten-tree)
                                (seq-remove #'null)
                                (seq-map #'file-name-sans-extension))))
             (seq-filter (lambda (it)
                           (member (symbol-name (detached--session-id it)) session-ids))
                         sessions))))
       ,@detached-list--filters))))

(defun detached-list-narrow-regexp (regexp)
  "Narrow to sessions which command match REGEXP."
  (interactive
   (list (read-regexp
          "Filter session commands containing (regexp): ")))
  (when regexp
    (detached-list-narrow-sessions
     `((,(concat "Regexp: " regexp) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (string-match regexp
                                       (detached--session-command it)))
                       sessions)))
       ,@detached-list--filters))))

(defun detached-list-narrow-annotation-regexp (regexp)
  "Narrow to sessions which annotation match REGEXP."
  (interactive
   (list (read-regexp
          "Filter session annotations containing (regexp): ")))
  (when regexp
    (detached-list-narrow-sessions
     `((,(concat "Annotation: " regexp) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (when-let ((annotation (detached--session-annotation it)))
                           (string-match regexp annotation)))
                       sessions)))
       ,@detached-list--filters))))

(defun detached-list-narrow-local ()
  "Narrow to local sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(("Local" .
      ,(lambda (sessions)
         (seq-filter #'detached--local-session-p sessions)))
     ,@detached-list--filters)))

(defun detached-list-narrow-remote ()
  "Narrow to remote sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(("Remote" .
      ,(lambda (sessions)
         (seq-filter #'detached--remote-session-p sessions)))
     ,@detached-list--filters)))

(defun detached-list-select-filter ()
  "Select a `detached-list-filter' to apply."
  (interactive)
  (when-let* ((filter-name (completing-read "Select filter: " detached-list-filters))
              (filter (alist-get filter-name detached-list-filters nil nil #'string=)))
    (setq detached-list--filters nil)
    (seq-do (lambda (it) (apply it)) filter)))

(defun detached-list-narrow-origin (origin)
  "Narrow to sessions with a specific ORIGIN."
  (interactive
   (list
    (when-let ((origins
                (thread-last (detached-list--get-filtered-sessions)
                             (seq-map #'detached--session-origin)
                             (seq-uniq)
                             (seq-remove #'null)
                             (seq-map #'symbol-name))))
      (completing-read
       "Select origin: "
       origins))))
  (when origin
    (detached-list-narrow-sessions
     `((,(concat "Origin: " origin) .
        ,(lambda (sessions)
           (seq-filter
            (lambda (it)
              (string-match origin
                            (symbol-name (detached--session-origin it))))
              sessions)))
       ,@detached-list--filters))))

(defun detached-list-narrow-active ()
  "Narrow to active sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(("Active" .
      ,(lambda (sessions)
         (seq-filter #'detached--active-session-p sessions)))
     ,@detached-list--filters)))

(defun detached-list-narrow-inactive ()
  "Narrow to inactive sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(("Inactive" .
      ,(lambda (sessions)
         (seq-remove #'detached--active-session-p sessions)))
     ,@detached-list--filters)))

(defun detached-list-narrow-success ()
  "Narrow to successful sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(("Success" .
      ,(lambda (sessions)
         (seq-filter (lambda (it)
                       (eq 'success (car (detached--session-status it))))
                     sessions)))
     ,@detached-list--filters)))

(defun detached-list-narrow-failure ()
  "Narrow to failed sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(("Failure" .
      ,(lambda (sessions)
         (seq-filter (lambda (it)
                       (eq 'failure (car (detached--session-status it))))
                     sessions)))
     ,@detached-list--filters)))

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
    (let ((detached--update-database nil))
      (seq-do
       (lambda (session)
         (detached-list--unmark-session session)
         (detached-delete-session session))
       (detached-list--get-marked-or-current-sessions)))
    (detached--db-update-sessions)
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
  (detached-list--restore-marks)
  (when hl-line-mode (hl-line-highlight)))

;;;###autoload
(defun detached-list-sessions ()
  "Open list of `detached'."
  (interactive)
  (let* ((buffer (get-buffer-create "*detached-list*"))
         (window
          (or
           (get-buffer-window buffer)
           (display-buffer buffer detached-list-display-buffer-action))))
    (select-window window)
    (unless (eq major-mode 'detached-list-mode)
      (detached-list-mode)
      (setq tabulated-list-entries
            (seq-map #'detached-list--get-entry
                     (detached-list--get-filtered-sessions)))
      (tabulated-list-print t))))

(defun detached-list-narrow-sessions (filters)
  "Narrow session(s) based on FILTERS."
  (setq detached-list--filters filters)
  (setq tabulated-list-entries
        (seq-map #'detached-list--get-entry
                 (detached-list--get-filtered-sessions)))
  (tabulated-list-print t))

;;;; Support functions

(defun detached-list--initialize-directory (directory)
  "Initialize sessions in DIRECTORY."
  (thread-last (detached-get-sessions)
               (seq-filter (lambda (it) (string= directory (detached--session-directory it))))
               (seq-do #'detached--initialize-session)))

(defun detached-list--db-update ()
  "Function to run when the database is updated."
  (when-let ((detached-list-buffer (detached-list--get-list-mode-buffer)))
    (with-current-buffer detached-list-buffer
      (detached-list-revert))))

(defun detached-list--get-list-mode-buffer ()
  "Return buffer with `detached-list-mode'."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (eq major-mode 'detached-list-mode)))
            (buffer-list)))

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
  (let ((sessions (detached-get-sessions)))
    (seq-do (lambda (filter)
              (setq sessions (funcall (cdr filter) sessions)))
            detached-list--filters)
    sessions))

(cl-defmethod detached--get-session ((_mode (derived-mode detached-list-mode)))
  "Return session when in `detached-list-mode'."
  (tabulated-list-get-id))

;;;; Major mode

(defvar detached-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'detached-list-edit-annotation)
    (define-key map (kbd "d") #'detached-list-delete-session)
    (define-key map (kbd "f") #'detached-list-select-filter)
    (define-key map (kbd "g") #'detached-list-revert)
    (define-key map (kbd "i") #'detached-list-initialize-session-directory)
    (define-key map (kbd "j") #'imenu)
    (define-key map (kbd "k") #'detached-list-kill-session)
    (define-key map (kbd "m") #'detached-list-mark-session)
    (define-key map (kbd "n a") #'detached-list-narrow-active)
    (define-key map (kbd "n h") #'detached-list-narrow-host)
    (define-key map (kbd "n f") #'detached-list-narrow-failure)
    (define-key map (kbd "n i") #'detached-list-narrow-inactive)
    (define-key map (kbd "n l") #'detached-list-narrow-local)
    (define-key map (kbd "n o") #'detached-list-narrow-origin)
    (define-key map (kbd "n r") #'detached-list-narrow-remote)
    (define-key map (kbd "n s") #'detached-list-narrow-success)
    (define-key map (kbd "n /") #'detached-list-narrow-output-regexp)
    (define-key map (kbd "n % a") #'detached-list-narrow-annotation-regexp)
    (define-key map (kbd "n % c") #'detached-list-narrow-regexp)
    (define-key map (kbd "q") #'detached-list-quit)
    (define-key map (kbd "r") #'detached-list-rerun-session)
    (define-key map (kbd "t") #'detached-list-toggle-mark-session)
    (define-key map (kbd "T") #'detached-list-toggle-sessions)
    (define-key map (kbd "u") #'detached-list-unmark-session)
    (define-key map (kbd "U") #'detached-list-unmark-sessions)
    (define-key map (kbd "w") #'detached-list-copy-session-command)
    (define-key map (kbd "W") #'detached-list-copy-session-output)
    (define-key map (kbd "x") #'detached-list-detach-from-session)
    (define-key map (kbd "%") #'detached-list-mark-regexp)
    (define-key map (kbd "=") #'detached-list-diff-marked-sessions)
    (define-key map (kbd "-") #'detached-list-widen)
    (define-key map (kbd "!") #'detached-shell-command)
    (define-key map (kbd ".") #'detached-describe-session)
    (define-key map (kbd "<backspace>") #'detached-list-remove-narrow-criterion)
    (define-key map (kbd "<return>") #'detached-list-open-session)
    map)
  "Keymap used in `detached-list-mode'.")

(defun detached-list--mode-line-indicator ()
  "Return the mode line indicator based on narrow criteria."
  (if detached-list--filters
      (string-join
       (thread-last detached-list--filters
                    (seq-reverse)
                    (seq-map #'car))
       " > ")
    ""))

(define-derived-mode detached-list-mode tabulated-list-mode "Detached List"
  "Mode for `detached' list."
  (setq tabulated-list-format (detached-list--get-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq imenu-create-index-function #'detached-list-imenu-index)
  (setq-local eldoc-echo-area-use-multiline-p t)
  (setq-local eldoc-idle-delay 0)
  (hl-line-mode)
  (add-hook 'eldoc-documentation-functions #'detached-list-eldoc nil t)
  (add-hook 'tabulated-list-revert-hook #'detached-list--revert-sessions nil t)
  (setq-local mode-line-position '((:eval (detached-list--mode-line-indicator))))
  (tabulated-list-init-header))

(provide 'detached-list)

;;; detached-list.el ends here
