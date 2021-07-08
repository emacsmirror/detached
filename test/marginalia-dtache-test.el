;;; marginalia-dtache-test.el --- Tests for marginalia-dtache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Url: https://gitlab.com/niklaseklund/dtache
;; Package-Requires: ((emacs "27.1"))
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

;; Tests for `marginalia-dtache'.

;;; Code:

(require 'ert)
(require 'marginalia-dtache)

(ert-deftest marginalia-dtache-test-duration ()
  (should (string= "1s" (marginalia-dtache--duration (dtache--session-create :duration 1))))
  (should (string= "1m 1s" (marginalia-dtache--duration (dtache--session-create :duration 61))))
  (should (string= "1h 1m 1s" (marginalia-dtache--duration (dtache--session-create :duration 3661)))))

(ert-deftest marginalia-dtache-test-creation ()
  ;; Make sure to set the TIMEZONE before executing the test to avoid
  ;; differences between machines
  (cl-letf (((getenv "TZ") "UTC0"))
    (should (string= "May 08 08:49" (marginalia-dtache--creation (dtache--session-create :creation-time 1620463748.7636228))))))

(ert-deftest marginalia-dtache-test-size ()
  (should (string= "100" (marginalia-dtache--size (dtache--session-create :log-size 100))))
  (should (string= "1k" (marginalia-dtache--size (dtache--session-create :log-size 1024)))))

(ert-deftest marginalia-dtache-git ()
  (should (string= "foo" (marginalia-dtache--git-branch (dtache--session-create :metadata '(:git-branch "foo")))))
  (should (not (marginalia-dtache--git-branch (dtache--session-create)))))

(ert-deftest marginalia-dtache-active ()
  (should (string= "*" (marginalia-dtache--active (dtache--session-create :active t))))
  (should (string= "" (marginalia-dtache--active (dtache--session-create :active nil)))))

(ert-deftest marginalia-dtache-degraded ()
  (should (string= "!" (marginalia-dtache--degraded (dtache--session-create :degraded t))))
  (should (string= "" (marginalia-dtache--degraded (dtache--session-create :degraded nil)))))

(provide 'marginalia-dtache-test)

;;; marginalia-dtache-test.el ends here
