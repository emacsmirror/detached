;;; dtache-marginalia.el --- Marginalia for dtache -*- lexical-binding: t -*-

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

;; This package provides a marginalia annotator for dtache.

;;; Code:

;;;; Requirements

(require 'dtache)
(require 'marginalia)

;;;; Variables

(defvar dtache-marginalia-git-branch-length 30)
(defvar dtache-marginalia-duration-length 10)
(defvar dtache-marginalia-size-length 8)
(defvar dtache-marginalia-date-length 12)

;;;; Faces

(defface dtache-marginalia-git
  '((t :inherit marginalia-char))
  "Face used to highlight git information in `marginalia-mode'.")

(defface dtache-marginalia-error
  '((t :inherit error))
  "Face used to highlight error in `marginalia-mode'.")

(defface dtache-marginalia-active
  '((t :inherit marginalia-file-owner))
  "Face used to highlight active in `marginalia-mode'.")

(defface dtache-marginalia-duration
  '((t :inherit marginalia-date))
  "Face used to highlight duration in `marginalia-mode'.")

(defface dtache-marginalia-size
  '((t :inherit marginalia-size))
  "Face used to highlight size in `marginalia-mode'.")

(defface dtache-marginalia-creation
  '((t :inherit marginalia-date))
  "Face used to highlight date in `marginalia-mode'.")

;;;; Functions

(defun dtache-marginalia-annotate (candidate)
  "Annotate dtache CANDIDATE."
  (let* ((session (dtache-session-decode candidate)))
    (marginalia--fields
     ((dtache-marginalia--active session) :width 3 :face 'dtache-marginalia-active)
     ((dtache-marginalia--stderr-p session) :width 3 :face 'dtache-marginalia-error)
     ((dtache-marginalia--git-branch session) :truncate dtache-marginalia-git-branch-length :face 'dtache-marginalia-git)
     ((dtache-marginalia--duration session) :truncate dtache-marginalia-duration-length :face 'dtache-marginalia-duration)
     ((dtache-marginalia--size session) :truncate dtache-marginalia-size-length :face 'dtache-marginalia-size)
     ((dtache-marginalia--creation session) :truncate dtache-marginalia-date-length :face 'dtache-marginalia-date))))

;;;; Support functions

(defun dtache-marginalia--duration (session)
  "Return SESSION's duration time."
  (let* ((time (round (dtache--session-duration session)))
         (hours (/ time 3600))
         (minutes (/ (mod time 3600) 60))
         (seconds (mod time 60)))
    (cond ((> time (* 60 60)) (format "%sh %sm %ss" hours minutes seconds))
          ((> time 60) (format "%sm %ss" minutes seconds))
          (t (format "%ss" seconds)))))

(defun dtache-marginalia--creation (session)
  "Return SESSION's creation time."
  (format-time-string
   "%b %d %H:%M"
   (dtache--session-creation-time session)))

(defun dtache-marginalia--size (session)
  "Return the size of SESSION's log."
  (file-size-human-readable
   (dtache--session-log-size session)))

(defun dtache-marginalia--git-branch (session)
  "Return the git branch for SESSION."
  (dtache--session-git session))

(defun dtache-marginalia--active (session)
  "Return string if SESSION is active."
  (if (dtache--session-active session)
      "*"
    ""))

(defun dtache-marginalia--stderr-p (session)
  "Return string if SESSION has errors."
  (if (dtache--session-stderr-p session)
      "!"
    ""))

(provide 'dtache-marginalia)

;;; dtache-marginalia.el ends here
