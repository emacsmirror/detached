;;; dtache-test.el --- Tests for dtache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

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
          (dtache-session-directory (expand-file-name "sessions" temp-directory))
          (dtache--sessions)
          (dtache--sessions-initialized)
          (dtache--remote-session-timer))
     (unwind-protect
         (progn
           (dtache-setup)
           ,@body)
       (delete-directory temp-directory t))))

(cl-defun dtache-test--create-session (&key command host)
  "Create session with COMMAND running on HOST."
  (cl-letf* (((symbol-function #'dtache--host) (lambda () host))
             ((symbol-function #'dtache-metadata) (lambda () nil))
             ((symbol-function #'dtache--watch-session-directory) #'ignore)
             (session (dtache-create-session command)))
    (dtache-test--change-session-state session 'activate)
    session))

(defun dtache-test--change-session-state (session state)
  "Set STATE of SESSION."
  (pcase state
    ('activate
     (dolist (type `(socket log))
       (with-temp-file (dtache--session-file session type))))
    ('deactivate
     (delete-file (dtache--session-file session 'socket)))
    ('kill
     (delete-file (dtache--session-file session 'socket))
     (delete-file (dtache--session-file session 'log)))))

;;;; Tests

(ert-deftest dtache-test-dtach-command ()
  (dtache-test--with-temp-database
   (cl-letf* ((dtache-dtach-program "dtach")
              (dtache-env "dtache-env")
              (dtache-shell-program "bash")
              (session (dtache-create-session "ls -la"))
              ((symbol-function #'dtache-create-session)
               (lambda (_)
                 session)))
     (let* ((dtache-session-mode 'create-and-attach)
            (expected `("-c" ,(dtache--session-file session 'socket t)
                        "-z" ,dtache-shell-program
                        "-c"
                        ,(format "{ dtache-env ls\\ -la; } 2>&1 | tee %s"
                                 (dtache--session-file session 'log t))))
            (expected-concat (format "%s -c %s -z %s -c %s"
                                     dtache-dtach-program
                                     (dtache--session-file session 'socket t)
                                     dtache-shell-program
                                     (shell-quote-argument
                                      (format "{ dtache-env ls\\ -la; } 2>&1 | tee %s"
                                              (dtache--session-file session 'log t))))))
       (should (equal expected (dtache-dtach-command session)))
       (should (equal expected-concat (dtache-dtach-command session t))))
     (let* ((dtache-session-mode 'attach)
            (expected `("-a" ,(dtache--session-file session 'socket t)))
            (expected-concat (format "%s -a %s"
                                     dtache-dtach-program
                                     (dtache--session-file session 'socket t))))
       (should (equal expected (dtache-dtach-command session)))
       (should (equal expected-concat (dtache-dtach-command session t)))))))

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
             ((symbol-function #'file-remote-p) (lambda (_directory _localname) "/home/user/tmp"))
             (session (dtache--session-create :id 's12345 :directory "/home/user/tmp/")))
    (should (string= "/home/user/tmp/s12345.log" (dtache--session-file session 'log)))
    (should (string= "/home/user/tmp/s12345.socket" (dtache--session-file session 'socket))))

  ;; Remote files
  (cl-letf* (((symbol-function #'expand-file-name) (lambda (file directory) (concat directory file)))
             ((symbol-function #'file-remote-p) (lambda (_directory _localname) "/ssh:foo:/home/user/tmp/"))
             (session (dtache--session-create :id 's12345 :directory "/ssh:foo:/home/user/tmp/")))
    (should (string= "/ssh:foo:/home/user/tmp/s12345.log" (dtache--session-file session 'log)))
    (should (string= "/ssh:foo:/home/user/tmp/s12345.socket" (dtache--session-file session 'socket)))))

(ert-deftest dtache-test-session-truncate-command ()
  (let ((dtache-max-command-length 7))
    (dtache--session-truncate-command
     (dtache--session-create :command "12345678"))
    (should (string= "123...678"
                     (dtache--session-truncate-command
                      (dtache--session-create :command "12345678")))))
  (let ((dtache-max-command-length 2))
    (dtache--session-truncate-command
                      (dtache--session-create :command "12345678"))
    (should (string= "1...8"
                     (dtache--session-truncate-command
                      (dtache--session-create :command "12345678"))))))

(ert-deftest dtache-test-host ()
  (cl-letf (((symbol-function #'system-name) (lambda () "localhost")))
    (should (equal '(:type local :name "localhost") (dtache--host))))
  (let ((default-directory "/ssh:remotehost:/home/user/git"))
    (should (equal '(:type remote :name "remotehost") (dtache--host)))))

(ert-deftest dtache-test-session-active-p ()
  (dtache-test--with-temp-database
   (let ((session (dtache-test--create-session :command "foo" :host '(:type local :name "bar"))))
     (should (eq 'active (dtache--determine-session-state session)))
     (dtache-test--change-session-state session 'deactivate)
     (should (eq 'inactive (dtache--determine-session-state session))))))

(ert-deftest dtache-test-session-dead-p ()
  (dtache-test--with-temp-database
   (let ((session (dtache-test--create-session :command "foo" :host '(:type local :name "bar"))))
     (should (not (dtache--session-missing-p session)))
     (dtache-test--change-session-state session 'deactivate)
     (should (not (dtache--session-missing-p session)))
     (dtache-test--change-session-state session 'kill)
     (should (dtache--session-missing-p session)))))

(ert-deftest dtache-test-cleanup-host-sessions ()
  (dtache-test--with-temp-database
   (cl-letf* ((session1 (dtache-test--create-session :command "foo" :host '(:type remote :name "remotehost")))
              (session2 (dtache-test--create-session :command "bar" :host '(:type local :name "localhost")))
              (session3 (dtache-test--create-session :command "baz" :host '(:type local :name "localhost")))
              (host '(:type local :name "localhost"))
              ((symbol-function #'dtache--host) (lambda () host)))
     ;; One inactive, one missing, one active
     (dtache-test--change-session-state session1 'deactivate)
     (dtache-test--change-session-state session2 'kill)
     (dtache--cleanup-host-sessions host)
     (dtache--db-get-sessions)
     (should (seq-set-equal-p
              (dtache--db-get-sessions)
              `(,session1 ,session3))))))

(ert-deftest dtache-test-dtach-arg ()
  (let ((dtache-session-mode 'create))
    (should (string= "-n" (dtache--dtach-arg))))
  (let ((dtache-session-mode 'create-and-attach))
    (should (string= "-c" (dtache--dtach-arg))))
  (let ((dtache-session-mode 'attach))
    (should (string= "-a" (dtache--dtach-arg))))
  (let ((dtache-session-mode nil))
    (should-error (dtache--dtach-arg))))

;;;;; Database

(ert-deftest dtache-test-db-insert-session ()
  (dtache-test--with-temp-database
   (let* ((session (dtache-test--create-session :command "foo" :host '(:type local :name "localhost"))))
     (should (equal (dtache--db-get-sessions) `(,session))))))

(ert-deftest dtache-test-db-remove-session ()
  (dtache-test--with-temp-database
   (let* ((host '(:type local :name "host"))
          (session1 (dtache-test--create-session :command "foo" :host '(:type local :name "host")))
          (session2 (dtache-test--create-session :command "bar" :host '(:type local :name "host"))))
     (should (seq-set-equal-p `(,session1 ,session2) (dtache--db-get-sessions)))
     (dtache--db-remove-entry session1)
     (should (seq-set-equal-p `(,session2) (dtache--db-get-sessions))))))

(ert-deftest dtache-test-db-update-session ()
  (dtache-test--with-temp-database
   (let* ((session (dtache-test--create-session :command "foo" :host '(:type local :name "host")))
          (id (dtache--session-id session))
          (copy))
     (setq copy (copy-dtache-session session))
     (setf (dtache--session-state copy) nil)
     (should (not (equal copy (dtache--db-get-session id))))
     (dtache--db-update-entry copy t)
     (should (equal copy (car (dtache--db-get-sessions)))))))

(ert-deftest dtache-test-dtache-command ()
  (let ((attachable-session (dtache--session-create :directory "/tmp/dtache/"
                                                :working-directory "/home/user/"
                                                :command "ls -la"
                                                :attachable t
                                                :id 'foo123))
        (nonattachable-session (dtache--session-create :directory "/tmp/dtache/"
                                                :working-directory "/home/user/"
                                                :command "ls -la"
                                                :attachable nil
                                                :id 'foo123)))
    ;; With dtache-env
    (let ((dtache-env "dtache-env"))
      (should (string= "{ dtache-env ls\\ -la; } 2>&1 | tee /tmp/dtache/foo123.log"
                       (dtache--dtache-command attachable-session)))
      (should (string= "{ dtache-env ls\\ -la; } &> /tmp/dtache/foo123.log"
                       (dtache--dtache-command nonattachable-session))))

    ;; Without dtache-env
    (let ((dtache-env nil)
          (dtache-shell-program "bash"))
      (should (string= "{ bash -c ls\\ -la; } 2>&1 | tee /tmp/dtache/foo123.log"
                       (dtache--dtache-command attachable-session)))
      (should (string= "{ bash -c ls\\ -la; } &> /tmp/dtache/foo123.log"
                       (dtache--dtache-command nonattachable-session))))))

(ert-deftest dtache-test-attachable-command-p ()
  (let ((dtache-nonattachable-commands '("ls")))
    (should (dtache-attachable-command-p "cd"))
    (should (not (dtache-attachable-command-p "ls -la")))))

;;;;; String representations

(ert-deftest dtache-test-duration-str ()
  (should (string= "1s" (dtache--duration-str (dtache--session-create :time '(:duration 1)))))
  (should (string= "1m 1s" (dtache--duration-str (dtache--session-create :time '(:duration 61)))))
  (should (string= "1h 1m 1s" (dtache--duration-str (dtache--session-create :time '(:duration 3661))))))

(ert-deftest dtache-test-creation-str ()
  ;; Make sure to set the TIMEZONE before executing the test to avoid
  ;; differences between machines
  (cl-letf* (((getenv "TZ") "UTC0")
             (session (dtache--session-create :time `(:start 1620463748.7636228))))
    (should (string= "May 08 08:49" (dtache--creation-str session)))))

(ert-deftest dtache-test-size-str ()
  (should (string= "100" (dtache--size-str (dtache--session-create :log-size 100))))
  (should (string= "1k" (dtache--size-str (dtache--session-create :log-size 1024)))))

(ert-deftest dtache-test-status-str ()
  (should (string= "!" (dtache--status-str (dtache--session-create :status 'failure))))
  (should (string= " " (dtache--status-str (dtache--session-create :status 'success))))
  (should (string= " " (dtache--status-str (dtache--session-create :status 'unknown)))))

(ert-deftest dtache-test-state-str ()
  (should (string= "*" (dtache--state-str (dtache--session-create :state 'active))))
  (should (string= " " (dtache--state-str (dtache--session-create :state 'inactive)))))

(ert-deftest dtache-test-working-dir-str ()
  (should
   (string= "/home/user/repo"
            (dtache--working-dir-str
             (dtache--session-create :working-directory "/ssh:remote:/home/user/repo"))))
  (should
   (string= "~/repo"
            (dtache--working-dir-str
             (dtache--session-create :working-directory "~/repo")))))

;;;;; Output filters

(ert-deftest dtache-test-dtach-eof-message-filter ()
  (let ((str "
[EOF - dtach terminating]
user@machine "))
    (should (string= "user@machine " (dtache--dtach-eof-message-filter str)))))

(ert-deftest dtache-test-dtach-detached-message-filter ()
  (let ((str "
[detached]
user@machine "))
    (should (string= "user@machine " (dtache--dtach-detached-message-filter str)))))

(ert-deftest dtache-test-dtache-env-message-filter ()
  (let ((str "output\n\nDtache session exited abnormally with code 127"))
    (should (string= "output\n" (dtache--dtache-env-message-filter str))))
  (let ((str "output\n\nDtache session finished"))
    (should (string= "output\n" (dtache--dtache-env-message-filter str)))))

(provide 'dtache-test)

;;; dtache-test.el ends here
