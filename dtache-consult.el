;;; dtache-consult.el --- Dtache interface using Consult multi sources -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

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

;; This package integrates `dtache' with `consult'[1].  The package
;; provides a command `dtache-consult-session' which provides multiple session sources.
;;
;; [1] https://github.com/minad/consult

;;; Code:

;;;; Requirements

(require 'dtache)

(declare-function consult--multi "consult")

;;;; Variables

(defcustom dtache-consult-hidden-predicates nil
  "Predicates for sessions that should be hidden."
  :type '(repeat function)
  :group 'dtache)

(defcustom dtache-consult-sources
  '(dtache-consult--source-session
    dtache-consult--source-active-session
    dtache-consult--source-inactive-session
    dtache-consult--source-success-session
    dtache-consult--source-failure-session
    dtache-consult--source-local-session
    dtache-consult--source-remote-session
    dtache-consult--source-current-session)
  "Sources used by `dtache-consult-session'.

See `consult-multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'dtache)

(defvar dtache-consult--source-session
  `(:category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-remove
                (lambda (x)
                  (seq-find (lambda (predicate)
                              (apply predicate `(,(cdr x))))
                         dtache-consult-hidden-predicates))
                (dtache-session-candidates (dtache-get-sessions))))))
  "All `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-hidden-session
  `(:narrow (?\s . "Hidden")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (seq-find (lambda (predicate)
                              (apply predicate `(,(cdr x))))
                         dtache-consult-hidden-predicates))
                (dtache-session-candidates (dtache-get-sessions))))))
  "Active `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-active-session
  `(:narrow (?a . "Active")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (eq 'active (dtache--session-state (cdr x))))
                (dtache-session-candidates (dtache-get-sessions))))))
  "Active `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-inactive-session
  `(:narrow (?i . "Inactive")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (eq 'inactive (dtache--session-state (cdr x))))
                (dtache-session-candidates (dtache-get-sessions))))))
  "Inactive `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-failure-session
  `(:narrow (?f . "Failure")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (eq 'failure (car (dtache--session-status (cdr x)))))
                (dtache-session-candidates (dtache-get-sessions))))))
  "Failed `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-success-session
  `(:narrow (?s . "Success")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (eq 'success (car (dtache--session-status (cdr x)))))
                (dtache-session-candidates (dtache-get-sessions))))))
  "Successful `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-local-session
  `(:narrow (?l . "Local Host")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (eq 'local (cdr (dtache--session-host (cdr x)))))
                (dtache-session-candidates (dtache-get-sessions)))))
    "Local host `dtache' sessions as a source for `consult'."))

(defvar dtache-consult--source-remote-session
  `(:narrow (?r . "Remote Host")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (mapcar #'car
               (seq-filter
                (lambda (x)
                  (eq 'remote (cdr (dtache--session-host (cdr x)))))
                (dtache-session-candidates (dtache-get-sessions))))))
  "Remote host `dtache' sessions as a source for `consult'.")

(defvar dtache-consult--source-current-session
  `(:narrow (?c . "Current Host")
    :hidden t
    :category dtache
    :annotate dtache-session-annotation
    :action (lambda (x) (dtache-open-session (dtache--decode-session x)))
    :items
    ,(lambda ()
       (let ((host-name (car (dtache--host))))
         (mapcar #'car (seq-filter
                        (lambda (x)
                          (string= (car (dtache--session-host (cdr x))) host-name))
                        (dtache-session-candidates (dtache-get-sessions)))))))
  "Current host `dtache' sessions as a source for `consult'.")

;;;; Commands

;;;###autoload
(defun dtache-consult-session ()
  "Enhanced `dtache-open-session' command."
  (interactive)
  (unless (require 'consult nil 'noerror)
    (error "Install Consult to use dtache-consult"))
  (consult--multi dtache-consult-sources
                  :prompt "Select session: "
                  :require-match t
                  :sort nil))

(provide 'dtache-consult)

;;; dtache-consult.el ends here
