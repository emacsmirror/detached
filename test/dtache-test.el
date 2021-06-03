;;; dtache-test.el --- Tests for dtache.el -*- lexical-binding: t; -*-

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

;; Tests for `dtache'.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'dtache)

;;;; Support

(defmacro dtache-test--with-temp-database (&rest body)
  "Initialize a dtache database and evaluate BODY."
  `(let* ((temp-directory (make-temp-file "dtache" t))
          (dtache-db-directory (expand-file-name "db" temp-directory))
          (dtache-session-directory (expand-file-name "sessions" temp-directory))
          (dtache-db))
     (unwind-protect
         (progn
           (dtache-db-initialize)
           (dtache-create-session-directory)
           ,@body)
       (delete-directory temp-directory t))))

(cl-defun dtache-test--create-session (&key command host)
  "Create session with COMMAND running on HOST."
  (cl-letf* (((symbol-function #'dtache--host) (lambda () host))
             ((symbol-function #'dtache-metadata) (lambda () nil))
             (session (dtache--create-session command)))
    (dtache-test--change-session-state session 'activate)
    session))

(defun dtache-test--change-session-state (session state)
  "Set STATE of SESSION."
  (pcase state
    ('activate
     (dolist (type `(socket log stderr))
       (with-temp-file (dtache-session-file session type))))
    ('deactivate
     (delete-file (dtache-session-file session 'socket)))
    ('kill
     (delete-file (dtache-session-file session 'socket))
     (delete-file (dtache-session-file session 'log))
     (delete-file (dtache-session-file session 'stderr)))))

;;;; Tests

(ert-deftest dtache-test-dtach-command ()
  (let* ((dtache-shell "zsh")
         (dtache-program "/usr/bin/dtach")
         (actual
          (dtache-dtach-command
           (dtache--session-create :id "12345" :session-directory "/tmp/dtache/")))
         (expected "^/usr/bin/dtach -c /tmp/dtache/12345.socket -z zsh -c .*"))
    (should (string-match-p expected actual))))

(ert-deftest dtache-test-metadata ()
  ;; No annotators
  (let ((dtache-metadata-annotators '()))
    (should (not (dtache-metadata))))

  ;; Two annotatos
  (let ((dtache-metadata-annotators
         '((:git-branch . (lambda () "foo"))
           (:username . (lambda () "bar"))))
        (expected '(:git-branch "foo" :username "bar")))
    (should (equal (dtache-metadata) expected))))

(ert-deftest dtache-test-session-file ()
  ;; Local files
  (cl-letf* (((symbol-function #'expand-file-name) (lambda (file directory) (concat directory file)))
             (session (dtache--session-create :id "12345" :session-directory "/home/user/tmp/")))
    (should (string= "/home/user/tmp/12345.log" (dtache-session-file session 'log)))
    (should (string= "/home/user/tmp/12345.stderr" (dtache-session-file session 'stderr)))
    (should (string= "/home/user/tmp/12345.stdout" (dtache-session-file session 'stdout)))
    (should (string= "/home/user/tmp/12345.socket" (dtache-session-file session 'socket))))

  ;; Remote files
  (cl-letf* (((symbol-function #'expand-file-name) (lambda (file directory) (concat directory file)))
             ((symbol-function #'file-remote-p) (lambda (_directory) "/ssh:foo:"))
             (session (dtache--session-create :id "12345" :session-directory "/home/user/tmp/")))
    (should (string= "/ssh:foo:/home/user/tmp/12345.log" (dtache-session-file session 'log)))
    (should (string= "/ssh:foo:/home/user/tmp/12345.stderr" (dtache-session-file session 'stderr)))
    (should (string= "/ssh:foo:/home/user/tmp/12345.stdout" (dtache-session-file session 'stdout)))
    (should (string= "/ssh:foo:/home/user/tmp/12345.socket" (dtache-session-file session 'socket)))))

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
    (should (string= "ab...jk  12345678" (dtache-encode-session session)))))

(ert-deftest dtache-test-host ()
  (should (string= "localhost" (dtache--host)))
  (let ((default-directory "/ssh:remotehost:/home/user/git"))
    (should (string= "remotehost" (dtache--host)))))

(ert-deftest dtache-test-session-active-p ()
  (dtache-test--with-temp-database
   (let ((session (dtache-test--create-session :command "foo" :host "localhost")))
     (should (dtache--session-active-p session))
     (dtache-test--change-session-state session 'deactivate)
     (should (not (dtache--session-active-p session))))))

(ert-deftest dtache-test-session-dead-p ()
  (dtache-test--with-temp-database
   (let ((session (dtache-test--create-session :command "foo" :host "localhost")))
     (should (not (dtache--session-dead-p session)))
     (dtache-test--change-session-state session 'deactivate)
     (should (not (dtache--session-dead-p session)))
     (dtache-test--change-session-state session 'kill)
     (should (dtache--session-dead-p session)))))

(ert-deftest dtache-test-session-decode ()
  (dtache-test--with-temp-database
   (dtache-test--create-session :command "foo" :host "localhost")
   (dtache-session-candidates)
   (should
    (equal (elt (dtache--db-select-host-sessions "localhost") 0)
           (dtache-decode-session
            (car (elt dtache--session-candidates 0)))))))

(ert-deftest dtache-test-session-candidates ()
  (dtache-test--with-temp-database
   (dtache-test--create-session :command "foo" :host "localhost")
   (dtache-test--create-session :command "bar" :host "localhost")
   (should
    (seq-set-equal-p
     (thread-last (dtache-session-candidates)
       (seq-map #'cdr))
     (seq-reverse
      (dtache--db-select-host-sessions "localhost"))))))

(ert-deftest dtache-test-update-sessions ()
  (dtache-test--with-temp-database
   (cl-letf* ((session1 (dtache-test--create-session :command "foo" :host "localhost"))
              (session2 (dtache-test--create-session :command "bar" :host "localhost"))
              (session3 (dtache-test--create-session :command "baz" :host "remotehost"))
              (host "localhost")
              ((symbol-function #'dtache--host) (lambda () host)))
     ;; Add three sessions two matching host which will be
     ;; updated. One of them is dead and should be removed
     (dtache-test--change-session-state session2 'kill)
     (dtache-test--change-session-state session3 'deactivate)
     (dtache-update-sessions)
     (let ((db-sessions (dtache--db-select-host-sessions host)))
       (should (= (length db-sessions) 1))
       (should (string= (dtache--session-id (elt db-sessions 0)) (dtache--session-id session1)))
       (should (not (equal (elt db-sessions 0) session1)))))))

(ert-deftest dtache-test-cleanup-sessions ()
  (dtache-test--with-temp-database
   (cl-letf* ((session1 (dtache-test--create-session :command "foo" :host "remotehost"))
              (session2 (dtache-test--create-session :command "bar" :host "localhost"))
              (session3 (dtache-test--create-session :command "baz" :host "localhost"))
              (host "localhost")
              ((symbol-function #'dtache--host) (lambda () host)))
     ;; One active, one dead, one active
     (dtache-test--change-session-state session1 'deactivate)
     (dtache-test--change-session-state session2 'kill)
     (dtache-cleanup-sessions)
     (should (seq-set-equal-p
              (dtache--db-select-host-sessions host)
              `(,session3))))))

;;;;; Database

(ert-deftest dtache-test-db-initialize ()
  (dtache-test--with-temp-database
   (should (emacsql-live-p dtache-db))))

(ert-deftest dtache-test-db-insert-session ()
  (dtache-test--with-temp-database
   (let* ((session (dtache-test--create-session :command "foo" :host "localhost"))
          (id (dtache--session-id session)))
     (should (equal (dtache--db-select-session id) session)))))

(ert-deftest dtache-test-db-remove-session ()
  (dtache-test--with-temp-database
   (let* ((host "localhost")
          (session1 (dtache-test--create-session :command "foo" :host host))
          (session2 (dtache-test--create-session :command "bar" :host host)))
     (should (seq-set-equal-p `(,session1 ,session2) (dtache--db-select-host-sessions host)))
     (dtache--db-remove-session session1)
     (should (seq-set-equal-p `(,session2) (dtache--db-select-host-sessions host))))))

(ert-deftest dtache-test-db-update-session ()
  (dtache-test--with-temp-database
   (let* ((session (dtache-test--create-session :command "foo" :host "localhost"))
          (id (dtache--session-id session)))
     (setf (dtache--session-active session) nil)
     (should (not (equal session (dtache--db-select-session id))))
     (dtache--db-update-session session)
     (should (equal session (dtache--db-select-session id))))))

(ert-deftest dtache-test-db-select-host-sessions ()
  (dtache-test--with-temp-database
   (let* ((session1 (dtache-test--create-session :command "foo" :host "localhost"))
          (session2 (dtache-test--create-session :command "bar" :host "remotehost"))
          (session3 (dtache-test--create-session :command "baz" :host "localhost")))
     (should (seq-set-equal-p `(,session2) (dtache--db-select-host-sessions "remotehost")))
     (should (seq-set-equal-p `(,session1 ,session3) (dtache--db-select-host-sessions "localhost"))))))

(ert-deftest dtache-test-db-select-active-sessions ()
  (dtache-test--with-temp-database
   (let* ((session1 (dtache-test--create-session :command "foo" :host "localhost"))
          (session2 (dtache-test--create-session :command "bar" :host "remotehost"))
          (session3 (dtache-test--create-session :command "baz" :host "localhost")))
     (dtache-test--change-session-state session1 'deactivate)
     (dtache-update-sessions)
     (let ((sessions (dtache--db-select-active-sessions "localhost")))
       (should (= (length sessions) 1))
       (should (string= (dtache--session-id (elt sessions 0)) (dtache--session-id session3)))))))

(provide 'dtache-test)

;;; dtache-test.el ends here
