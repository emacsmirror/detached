;;; dtache-test.el --- Tests for dtache.el -*- lexical-binding: t; -*-

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

;; Tests for dtache

;;; Code:

(require 'ert)
(require 'dtache)

;;; Tests

(ert-deftest dtache-test-session-short-id ()
  (let ((session (dtache--session-create :id "abcdefg12345678")))
    (should (string= "12345678" (dtache--session-short-id session)))))

(ert-deftest dtache-test-session-truncate-command ()
  (let ((dtache-max-command-length 7))
    (should (string= "12...78"
                     (dtache--session-truncate-command
                      (dtache--session-create :command "12345678")))))
  (let ((dtache-max-command-length 6))
    (should (string= "1...8"
                     (dtache--session-truncate-command
                      (dtache--session-create :command "12345678"))))))

(ert-deftest dtache-test-session-encode ()
  (let ((session
         (dtache--session-create :command "abcdefghijk"
                                 :id "-------12345678"))
        (dtache-max-command-length 8))
    (should (string= "ab...jk  12345678" (dtache-session-encode session)))))

(provide 'dtache-test)

;;; dtache-test.el ends here
