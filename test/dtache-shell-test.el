;;; dtache-shell-test.el --- Tests for dtache-shell.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Url: https://gitlab.com/niklaseklund/dtache
;; Package-requires: ((emacs "26.1"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for dtache-shell

;;; Code:

(require 'ert)
(require 'dtache-shell)

;;;; Tests

(ert-deftest dtache-shell-test-filter-eof ()
  (let ((str "
[EOF - dtach terminating]
user@machine "))
    (should (string= "\nuser@machine " (dtache-shell--filter-dtach-eof str)))))

(provide 'dtache-shell-test)

;;; dtache-shell-test.el ends here
