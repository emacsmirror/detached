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
          (dtache-db-directory (expand-file-name "dtache.db" temp-directory))
          (dtache-session-directory (expand-file-name "sessions" temp-directory)))
     (unwind-protect
         (progn
           (dtache-initialize)
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
  (cl-letf* (((symbol-function #'dtache--output-command) (lambda (_) "command"))
             (dtache-shell-program "zsh")
             (dtache-dtach-program "/usr/bin/dtach")
             (dtache--dtach-mode 'create)
             (actual
              (dtache-dtach-command
               (dtache--session-create :id "12345" :session-directory "/tmp/dtache/")))
             (expected `(, "-c" "/tmp/dtache/12345.socket" "-z" "zsh" "-c" "command")))
    (should (equal expected actual))))

(ert-deftest dtache-test-metadata ()
  ;; No annotators
  (let ((dtache-metadata-annotators-alist '()))
    (should (not (dtache-metadata))))

  ;; Two annotators
  (let ((dtache-metadata-annotators-alist
         '((git-branch . (lambda () "foo"))
           (username . (lambda () "bar"))))
        (expected '((username . "bar")
                    (git-branch . "foo"))))
    (should (equal (dtache-metadata) expected))))

(ert-deftest dtache-test-session-file ()
  ;; Local files
  (cl-letf* (((symbol-function #'expand-file-name) (lambda (file directory) (concat directory file)))
             ((symbol-function #'file-remote-p) (lambda (_directory) nil))
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

(ert-deftest dtache-test-cleanup-host-sessions ()
  (dtache-test--with-temp-database
   (cl-letf* ((session1 (dtache-test--create-session :command "foo" :host "remotehost"))
              (session2 (dtache-test--create-session :command "bar" :host "localhost"))
              (session3 (dtache-test--create-session :command "baz" :host "localhost"))
              (host "localhost")
              ((symbol-function #'dtache--host) (lambda () host)))
     ;; One inactive, one missing, one active
     (dtache-test--change-session-state session1 'deactivate)
     (dtache-test--change-session-state session2 'kill)
     (dtache-cleanup-host-sessions host)
     (should (seq-set-equal-p
              (dtache--db-select-sessions)
              `(,session1 ,session3))))))

;;;;; Database

(ert-deftest dtache-test-db-insert-session ()
  (dtache-test--with-temp-database
   (let* ((session (dtache-test--create-session :command "foo" :host "localhost")))
     (should (equal (dtache--db-select-sessions) `(,session))))))

(ert-deftest dtache-test-db-remove-session ()
  (dtache-test--with-temp-database
   (let* ((host "localhost")
          (session1 (dtache-test--create-session :command "foo" :host host))
          (session2 (dtache-test--create-session :command "bar" :host host)))
     (should (seq-set-equal-p `(,session1 ,session2) (dtache--db-select-sessions)))
     (dtache--db-remove-session session1)
     (should (seq-set-equal-p `(,session2) (dtache--db-select-sessions))))))

(ert-deftest dtache-test-db-update-session ()
  (dtache-test--with-temp-database
   (let* ((session (dtache-test--create-session :command "foo" :host "localhost"))
          (id (dtache--session-id session)))
     (setf (dtache--session-active session) nil)
     (should (not (equal session (car (dtache--db-select-sessions)))))
     (dtache--db-update-session session)
     (should (equal session (car (dtache--db-select-sessions)))))))

(ert-deftest dtache-test-output-command ()
  ;; Degraded
  (let* ((dtache-notify-send nil)
         (actual
          (dtache--output-command
           (dtache--session-create :id "12345" :session-directory "/tmp/dtache/" :command "ls" :degraded t)))
         (expected "{ ls; } &> /tmp/dtache/12345.log"))
    (should (string= actual expected)))

  ;; Normal
  (let* ((dtache-notify-send nil)
         (actual
          (dtache--output-command
           (dtache--session-create :id "12345" :session-directory "/tmp/dtache/" :command "ls")))
         (expected "{ { ls; } > >(tee /tmp/dtache/12345.stdout ); } 2> >(tee /tmp/dtache/12345.stderr )  | tee /tmp/dtache/12345.log"))
    (should (string= actual expected))))

(ert-deftest dtache-test-degraded-p ()
  (let ((dtache-degraded-list '("ls")))
    (should (not (dtache-degraded-p "cd")))
    (should (dtache-degraded-p "ls -la"))))

(ert-deftest dtache-test-session-pid ()
  (cl-letf* (((symbol-function #'process-file) (lambda (_program _infile _buffer _display &rest _args)
                                                 (insert "\"USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND\nuser    6699  0.0  0.0   4752  2304 ?        Ss   13:06   0:00 dtach -n /tmp/foo.socket\nuser    6698  0.0  0.0   4752  2304 ?        Ss   13:07   0:00 dtach -c /tmp/bar.socket\n")))

             (session1 (dtache--session-create :id "foo" :session-directory "/tmp/"))
             (session2 (dtache--session-create :id "bar" :session-directory "/tmp/"))
             (session3 (dtache--session-create :id "baz" :session-directory "/tmp/"))
             (dtache--socket-ext ".socket"))
    (should (string= "6699" (dtache--session-pid session1)))
    (should (string= "6698" (dtache--session-pid session2)))
    (should (not (dtache--session-pid session3)))))

;;;;; String representations

(ert-deftest dtache-test-duration-str ()
  (should (string= "1s" (dtache--duration-str (dtache--session-create :duration 1))))
  (should (string= "1m 1s" (dtache--duration-str (dtache--session-create :duration 61))))
  (should (string= "1h 1m 1s" (dtache--duration-str (dtache--session-create :duration 3661)))))

(ert-deftest dtache-test-creation-str ()
  ;; Make sure to set the TIMEZONE before executing the test to avoid
  ;; differences between machines
  (cl-letf (((getenv "TZ") "UTC0"))
    (should (string= "May 08 08:49" (dtache--creation-str (dtache--session-create :creation-time 1620463748.7636228))))))

(ert-deftest dtache-test-size-str ()
  (should (string= "100" (dtache--size-str (dtache--session-create :log-size 100))))
  (should (string= "1k" (dtache--size-str (dtache--session-create :log-size 1024)))))

(ert-deftest dtache-test-degraded-str ()
  (should (string= "!" (dtache--degraded-str (dtache--session-create :degraded t))))
  (should (string= "" (dtache--degraded-str (dtache--session-create :degraded nil)))))

(ert-deftest dtache-test-active-str ()
  (should (string= "*" (dtache--active-str (dtache--session-create :active t))))
  (should (string= "" (dtache--active-str (dtache--session-create :active nil)))))

(provide 'dtache-test)

;;; dtache-test.el ends here
