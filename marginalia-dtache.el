;;; marginalia-dtache.el --- Dtache Marginalia integration -*- lexical-binding: t -*-

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

;; This package provides annotated `dtache' sessions with `marginalia'.

;;; Code:

;;;; Requirements

(require 'dtache)
(require 'marginalia)

;;;; Variables

(defvar marginalia-dtache-git-branch-length 30)
(defvar marginalia-dtache-duration-length 10)
(defvar marginalia-dtache-size-length 8)
(defvar marginalia-dtache-date-length 12)

;;;; Faces

(defgroup marginalia-dtache-faces nil
  "Faces used by `marginalia-mode'."
  :group 'marginalia
  :group 'faces)

(defface marginalia-dtache-git
  '((t :inherit marginalia-char))
  "Face used to highlight git information in `marginalia-mode'.")

(defface marginalia-dtache-error
  '((t :inherit error))
  "Face used to highlight error in `marginalia-mode'.")

(defface marginalia-dtache-active
  '((t :inherit marginalia-file-owner))
  "Face used to highlight active in `marginalia-mode'.")

(defface marginalia-dtache-duration
  '((t :inherit marginalia-date))
  "Face used to highlight duration in `marginalia-mode'.")

(defface marginalia-dtache-size
  '((t :inherit marginalia-size))
  "Face used to highlight size in `marginalia-mode'.")

(defface marginalia-dtache-creation
  '((t :inherit marginalia-date))
  "Face used to highlight date in `marginalia-mode'.")

;;;; Functions

(defun marginalia-dtache-annotate (candidate)
  "Annotate dtache CANDIDATE."
  (let* ((session (dtache-decode-session candidate)))
    (marginalia--fields
     ((marginalia-dtache--active session) :width 3 :face 'marginalia-dtache-active)
     ((marginalia-dtache--stderr-p session) :width 3 :face 'marginalia-dtache-error)
     ((marginalia-dtache--git-branch session) :truncate marginalia-dtache-git-branch-length :face 'marginalia-dtache-git)
     ((marginalia-dtache--duration session) :truncate marginalia-dtache-duration-length :face 'marginalia-dtache-duration)
     ((marginalia-dtache--size session) :truncate marginalia-dtache-size-length :face 'marginalia-dtache-size)
     ((marginalia-dtache--creation session) :truncate marginalia-dtache-date-length :face 'marginalia-dtache-date))))

;;;; Support functions

(defun marginalia-dtache--duration (session)
  "Return SESSION's duration time."
  (let* ((time (round (dtache--session-duration session)))
         (hours (/ time 3600))
         (minutes (/ (mod time 3600) 60))
         (seconds (mod time 60)))
    (cond ((> time (* 60 60)) (format "%sh %sm %ss" hours minutes seconds))
          ((> time 60) (format "%sm %ss" minutes seconds))
          (t (format "%ss" seconds)))))

(defun marginalia-dtache--creation (session)
  "Return SESSION's creation time."
  (format-time-string
   "%b %d %H:%M"
   (dtache--session-creation-time session)))

(defun marginalia-dtache--size (session)
  "Return the size of SESSION's log."
  (file-size-human-readable
   (dtache--session-log-size session)))

(defun marginalia-dtache--git-branch (session)
  "Return the git branch for SESSION."
  (plist-get (dtache--session-metadata session) :git))

(defun marginalia-dtache--active (session)
  "Return string if SESSION is active."
  (if (dtache--session-active session)
      "*"
    ""))

(defun marginalia-dtache--stderr-p (session)
  "Return string if SESSION has errors."
  (if (dtache--session-stderr-p session)
      "!"
    ""))

(provide 'marginalia-dtache)

;;; marginalia-dtache.el ends here
