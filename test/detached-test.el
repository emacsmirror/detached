;;; detached-test.el --- Tests for detached.el -*- lexical-binding: t; -*-

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

;; Tests for `detached'.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'detached)

;;;; Support

(defmacro detached-test--with-temp-database (&rest body)
  "Initialize a detached database and evaluate BODY."
  `(let* ((temp-directory (make-temp-file "detached" t))
          (detached-db-directory (expand-file-name "detached.db" temp-directory))
          (detached-session-directory (expand-file-name "sessions" temp-directory))
          (detached--sessions)
          (detached--sessions-initialized)
          (detached--remote-session-timer))
     (unwind-protect
         (progn
           (detached-initialize-sessions)
           ,@body)
       (delete-directory temp-directory t))))

(cl-defun detached-test--create-session (&key command host)
  "Create session with COMMAND running on HOST."
  (cl-letf* (((symbol-function #'detached--host) (lambda () host))
             ((symbol-function #'detached-metadata) (lambda () nil))
             ((symbol-function #'detached--watch-session-directory) #'ignore)
             (session (detached-create-session command)))
    (detached-test--change-session-state session 'activate)
    session))

(defun detached-test--change-session-state (session state)
  "Set STATE of SESSION."
  (pcase state
    ('activate
     (dolist (type `(socket log))
       (with-temp-file (detached--session-file session type))))
    ('deactivate
     (delete-file (detached--session-file session 'socket)))
    ('kill
     (delete-file (detached--session-file session 'socket))
     (delete-file (detached--session-file session 'log)))))

;;;; Tests

(ert-deftest detached-test-dtach-command ()
  (detached-test--with-temp-database
   (cl-letf* ((detached-dtach-program "dtach")
              (detached-env "detached-env")
              (detached-shell-program "bash")
              (session (detached-create-session "ls -la"))
              (detached-show-output-on-attach t)
              (detached-show-output-command "/bin/cat")
              ((symbol-function #'detached-create-session)
               (lambda (_)
                 session)))
     (let* ((detached-session-mode 'create-and-attach)
            (expected `(,detached-dtach-program
                        "-c" ,(detached--session-file session 'socket t)
                        "-z" ,detached-shell-program
                        "-c"
                        ,(format "{ detached-env terminal-data ls\\ -la; } 2>&1 | tee %s"
                                 (detached--session-file session 'log t))))
            (expected-concat (format "%s -c %s -z %s -c %s"
                                     detached-dtach-program
                                     (detached--session-file session 'socket t)
                                     detached-shell-program
                                     (shell-quote-argument
                                      (format "{ detached-env terminal-data ls\\ -la; } 2>&1 | tee %s"
                                              (detached--session-file session 'log t))))))
       (should (equal expected (detached-dtach-command session)))
       (should (equal expected-concat (detached-dtach-command session t))))
     (let* ((detached-session-mode 'attach)
            (expected `(,detached-show-output-command
                        ,(format "%s;" (detached--session-file session 'log t))
                        ,detached-dtach-program "-a" ,(detached--session-file session 'socket t) "-r" "none"))
            (expected-concat (format "%s %s; %s -a %s -r none"
                                     detached-show-output-command
                                     (detached--session-file session 'log t)
                                     detached-dtach-program
                                     (detached--session-file session 'socket t))))
       (should (equal expected (detached-dtach-command session)))
       (should (equal expected-concat (detached-dtach-command session t)))))))

(ert-deftest detached-test-metadata ()
  ;; No annotators
  (let ((detached-metadata-annotators-alist '()))
    (should (not (detached-metadata))))

  ;; Two annotators
  (let ((detached-metadata-annotators-alist
         '((git-branch . (lambda () "foo"))
           (username . (lambda () "bar"))))
        (expected '((username . "bar")
                    (git-branch . "foo"))))
    (should (equal (detached-metadata) expected))))

(ert-deftest detached-test-session-file ()
  ;; Local files
  (cl-letf* (((symbol-function #'expand-file-name) (lambda (file directory) (concat directory file)))
             ((symbol-function #'file-remote-p) (lambda (_directory _localname) "/home/user/tmp"))
             (session (detached--session-create :id 's12345 :directory "/home/user/tmp/")))
    (should (string= "/home/user/tmp/s12345.log" (detached--session-file session 'log)))
    (should (string= "/home/user/tmp/s12345.socket" (detached--session-file session 'socket))))

  ;; Remote files
  (cl-letf* (((symbol-function #'expand-file-name) (lambda (file directory) (concat directory file)))
             ((symbol-function #'file-remote-p) (lambda (_directory _localname) "/ssh:foo:/home/user/tmp/"))
             (session (detached--session-create :id 's12345 :directory "/ssh:foo:/home/user/tmp/")))
    (should (string= "/ssh:foo:/home/user/tmp/s12345.log" (detached--session-file session 'log)))
    (should (string= "/ssh:foo:/home/user/tmp/s12345.socket" (detached--session-file session 'socket)))))

(ert-deftest detached-test-host ()
  (cl-letf (((symbol-function #'system-name) (lambda () "localhost")))
    (should (equal '("localhost" . local) (detached--host))))
  (let ((default-directory "/ssh:remotehost:/home/user/git"))
    (should (equal '("remotehost" . remote) (detached--host)))))

(ert-deftest detached-test-session-active-p ()
  (detached-test--with-temp-database
   (let ((session (detached-test--create-session :command "foo" :host '("bar" . local))))
     (should (eq 'active (detached--determine-session-state session)))
     (detached-test--change-session-state session 'deactivate)
     (should (eq 'inactive (detached--determine-session-state session))))))

(ert-deftest detached-test-session-dead-p ()
  (detached-test--with-temp-database
   (let ((session (detached-test--create-session :command "foo" :host '("bar" . local))))
     (should (not (detached--session-missing-p session)))
     (detached-test--change-session-state session 'deactivate)
     (should (not (detached--session-missing-p session)))
     (detached-test--change-session-state session 'kill)
     (should (detached--session-missing-p session)))))

(ert-deftest detached-test-cleanup-host-sessions ()
  (detached-test--with-temp-database
   (cl-letf* ((session1 (detached-test--create-session :command "foo" :host '("remotehost" . remote)))
              (session2 (detached-test--create-session :command "bar" :host '("localhost" . local)))
              (session3 (detached-test--create-session :command "baz" :host '("localhost" . local)))
              (host '("localhost" . local))
              ((symbol-function #'detached--host) (lambda () host)))
     ;; One inactive, one missing, one active
     (detached-test--change-session-state session1 'deactivate)
     (detached-test--change-session-state session2 'kill)
     (detached--cleanup-host-sessions host)
     (detached--db-get-sessions)
     (should (seq-set-equal-p
              (detached--db-get-sessions)
              `(,session1 ,session3))))))

(ert-deftest detached-test-dtach-arg ()
  (let ((detached-session-mode 'create))
    (should (string= "-n" (detached--dtach-arg))))
  (let ((detached-session-mode 'create-and-attach))
    (should (string= "-c" (detached--dtach-arg))))
  (let ((detached-session-mode 'attach))
    (should (string= "-a" (detached--dtach-arg))))
  (let ((detached-session-mode nil))
    (should-error (detached--dtach-arg))))

;;;;; Database

(ert-deftest detached-test-db-insert-session ()
  (detached-test--with-temp-database
   (let* ((session (detached-test--create-session :command "foo" :host '("localhost" . local))))
     (should (equal (detached--db-get-sessions) `(,session))))))

(ert-deftest detached-test-db-remove-session ()
  (detached-test--with-temp-database
   (let* ((host '(:type local :name "host"))
          (session1 (detached-test--create-session :command "foo" :host '("host" . local)))
          (session2 (detached-test--create-session :command "bar" :host '("host" . local))))
     (should (seq-set-equal-p `(,session1 ,session2) (detached--db-get-sessions)))
     (detached--db-remove-entry session1)
     (should (seq-set-equal-p `(,session2) (detached--db-get-sessions))))))

(ert-deftest detached-test-db-update-session ()
  (detached-test--with-temp-database
   (let* ((session (detached-test--create-session :command "foo" :host '("host" . local)))
          (id (detached--session-id session))
          (copy))
     (setq copy (copy-detached-session session))
     (setf (detached--session-state copy) nil)
     (should (not (equal copy (detached--db-get-session id))))
     (detached--db-update-entry copy t)
     (should (equal copy (car (detached--db-get-sessions)))))))

(ert-deftest detached-test-detached-command ()
  (let ((attachable-session (detached--session-create :directory "/tmp/detached/"
                                                :working-directory "/home/user/"
                                                :command "ls -la"
                                                :attachable t
                                                :env-mode 'terminal-data
                                                :id 'foo123))
        (nonattachable-session (detached--session-create :directory "/tmp/detached/"
                                                :working-directory "/home/user/"
                                                :command "ls -la"
                                                :attachable nil
                                                :env-mode 'plain-text
                                                :id 'foo123)))
    ;; With detached-env
    (let ((detached-env "detached-env"))
      (should (string= "{ detached-env terminal-data ls\\ -la; } 2>&1 | tee /tmp/detached/foo123.log"
                       (detached--detached-command attachable-session)))
      (should (string= "{ detached-env plain-text ls\\ -la; } &> /tmp/detached/foo123.log"
                       (detached--detached-command nonattachable-session))))

    ;; Without detached-env
    (let ((detached-env nil)
          (detached-shell-program "bash"))
      (should (string= "{ bash -c ls\\ -la; } 2>&1 | tee /tmp/detached/foo123.log"
                       (detached--detached-command attachable-session)))
      (should (string= "{ bash -c ls\\ -la; } &> /tmp/detached/foo123.log"
                       (detached--detached-command nonattachable-session))))))

(ert-deftest detached-test-attachable-command-p ()
  (let ((detached-nonattachable-commands '("ls")))
    (should (detached-attachable-command-p "cd"))
    (should (not (detached-attachable-command-p "ls -la")))))

;;;;; String representations

(ert-deftest detached-test-duration-str ()
  (should (string= "1s" (detached--duration-str (detached--session-create :time '(:duration 1)))))
  (should (string= "1m 1s" (detached--duration-str (detached--session-create :time '(:duration 61)))))
  (should (string= "1h 1m 1s" (detached--duration-str (detached--session-create :time '(:duration 3661))))))

(ert-deftest detached-test-creation-str ()
  ;; Make sure to set the TIMEZONE before executing the test to avoid
  ;; differences between machines
  (cl-letf* (((getenv "TZ") "UTC0")
             (session (detached--session-create :time `(:start 1620463748.7636228))))
    (should (string= "May 08 08:49" (detached--creation-str session)))))

(ert-deftest detached-test-size-str ()
  (should (string= "100" (detached--size-str (detached--session-create :size 100 :state 'inactive))))
  (should (string= "1k" (detached--size-str (detached--session-create :size 1024 :state 'inactive)))))

(ert-deftest detached-test-status-str ()
  (should (string= "!" (detached--status-str (detached--session-create :status '(failure . 127)))))
  (should (string= "" (detached--status-str (detached--session-create :status '(success . 0)))))
  (should (string= "" (detached--status-str (detached--session-create :status '(unknown . 0))))))

(ert-deftest detached-test-state-str ()
  (should (string= "*" (detached--state-str (detached--session-create :state 'active))))
  (should (string= "" (detached--state-str (detached--session-create :state 'inactive)))))

(ert-deftest detached-test-working-dir-str ()
  (should
   (string= "/home/user/repo"
            (detached--working-dir-str
             (detached--session-create :working-directory "/ssh:remote:/home/user/repo"))))
  (should
   (string= "~/repo"
            (detached--working-dir-str
             (detached--session-create :working-directory "~/repo")))))

;;;;; Output filters

(ert-deftest detached-test-dtach-eof-message-filter ()
  (let ((str "
[EOF - dtach terminating]
user@machine "))
    (should (string= "user@machine " (detached--dtach-eof-message-filter str)))))

(ert-deftest detached-test-dtach-detached-message-filter ()
  (let ((str "
[detached]
user@machine "))
    (should (string= "user@machine " (detached--dtach-detached-message-filter str)))))

(ert-deftest detached-test-detached-env-message-filter ()
  (let ((str "output\n\nDetached session exited abnormally with code 127"))
    (should (string= "output\n" (detached--detached-env-message-filter str))))
  (let ((str "output\n\nDetached session finished"))
    (should (string= "output\n" (detached--detached-env-message-filter str)))))

(provide 'detached-test)

;;; detached-test.el ends here
