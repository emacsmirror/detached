;;; dtache-marginalia-test.el --- Tests for dtache-marginalia.el -*- lexical-binding: t; -*-

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

;; Tests for dtache-marginalia

;;; Code:

(require 'ert)
(require 'dtache-marginalia)

(ert-deftest dtache-marginalia-test-duration ()
  (should (string= "1s" (dtache-marginalia--duration (dtache--session-create :duration 1))))
  (should (string= "1m 1s" (dtache-marginalia--duration (dtache--session-create :duration 61))))
  (should (string= "1h 1m 1s" (dtache-marginalia--duration (dtache--session-create :duration 3661)))))

(ert-deftest dtache-marginalia-test-creation ()
  ;; Make sure to set the TIMEZONE before executing the test to avoid
  ;; differences between machines
  (cl-letf (((getenv "TZ") "UTC0"))
    (should (string= "May 08 08:49" (dtache-marginalia--creation (dtache--session-create :creation-time 1620463748.7636228))))))

(ert-deftest dtache-marginalia-test-size ()
  (should (string= "100" (dtache-marginalia--size (dtache--session-create :log-size 100))))
  (should (string= "1k" (dtache-marginalia--size (dtache--session-create :log-size 1024)))))

(ert-deftest dtache-marginalia-git ()
  (should (string= "foo" (dtache-marginalia--git-branch (dtache--session-create :git "foo"))))
  (should (not (dtache-marginalia--git-branch (dtache--session-create)))))

(ert-deftest dtache-marginalia-active ()
  (should (string= "*" (dtache-marginalia--active (dtache--session-create :active t))))
  (should (string= "" (dtache-marginalia--active (dtache--session-create :active nil)))))

(ert-deftest dtache-marginalia-stderr-p ()
  (should (string= "!" (dtache-marginalia--stderr-p (dtache--session-create :stderr-p t))))
  (should (string= "" (dtache-marginalia--stderr-p (dtache--session-create :stderr-p nil)))))

(provide 'dtache-marginalia-test)

;;; dtache-marginalia-test.el ends here
