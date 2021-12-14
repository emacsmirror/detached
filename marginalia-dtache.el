;;; marginalia-dtache.el --- Dtache Marginalia integration -*- lexical-binding: t -*-

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

;; This package provides annotated `dtache' sessions through
;; `marginalia' which enhances the `dtache-open-session'.

;;; Code:

;;;; Requirements

(require 'dtache)
(require 'marginalia)

;;;; Variables

(defvar marginalia-dtache-metadata-length 30)
(defvar marginalia-dtache-duration-length 10)
(defvar marginalia-dtache-working-dir-length 50)
(defvar marginalia-dtache-size-length 8)
(defvar marginalia-dtache-date-length 12)
(defvar marginalia-dtache-host-length 10)

;;;; Faces

(defgroup marginalia-dtache-faces nil
  "Faces used by `marginalia-mode'."
  :group 'marginalia
  :group 'faces)

(defface marginalia-dtache-metadata
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

(defface marginalia-dtache-working-dir
  '((t :inherit marginalia-symbol))
  "Face used to highlight working directory in `marginalia-mode'.")

(defface marginalia-dtache-host
  '((t :inherit marginalia-function))
  "Face used to highlight host in `marginalia-mode'.")

;;;; Functions

(defun marginalia-dtache-annotate (candidate)
  "Annotate dtache CANDIDATE."
  (let* ((session
          (get-text-property 0 'dtache--data candidate)))
    (marginalia--fields
     ((dtache--active-str session) :width 3 :face 'marginalia-dtache-active)
     ((dtache--status-str session) :width 3 :face 'marginalia-dtache-error)
     ((dtache--session-host session) :truncate marginalia-dtache-host-length :face 'marginalia-dtache-host)
     ((dtache--working-dir-str session) :truncate marginalia-dtache-working-dir-length :face 'marginalia-dtache-working-dir)
     ((dtache--metadata-str session) :truncate marginalia-dtache-metadata-length :face 'marginalia-dtache-metadata)
     ((dtache--duration-str session) :truncate marginalia-dtache-duration-length :face 'marginalia-dtache-duration)
     ((dtache--size-str session) :truncate marginalia-dtache-size-length :face 'marginalia-dtache-size)
     ((dtache--creation-str session) :truncate marginalia-dtache-date-length :face 'marginalia-dtache-creation))))

(provide 'marginalia-dtache)

;;; marginalia-dtache.el ends here
