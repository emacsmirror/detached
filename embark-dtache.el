;;; embark-dtache.el --- Dtache Embark integration -*- lexical-binding: t -*-

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

;; This package provides `embark' actions to operate on `dtache' sessions.

;;; Code:

;;;; Requirements

(require 'dtache)
(require 'embark)

;;;; Keymap

(embark-define-keymap embark-dtache-map
  "Keymap for Embark dtache actions."
  ("a" dtache-attach-to-session)
  ("l" dtache-open-log)
  ("t" dtache-tail-log)
  ("e" dtache-open-stderr)
  ("o" dtache-open-stdout)
  ("i" dtache-insert-session-command)
  ("w" dtache-copy-session-command)
  ("W" dtache-copy-session-log)
  ("c" dtache-compile-session)
  ("d" dtache-remove-session)
  ("k" dtache-kill-session))

(add-to-list 'embark-keymap-alist '(dtache . embark-dtache-map))

(provide 'embark-dtache)

;;; embark-dtache.el ends here
