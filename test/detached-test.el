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
		  (detached-db-directory (expand-file-name "detached-sessions.db" temp-directory))
		  (detached-session-directory (expand-file-name "sessions" temp-directory))
		  (detached--sessions)
		  (detached--sessions-initialized))
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
			 ((symbol-function #'emacs-pid) (lambda () 1))
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

;;;;; Session interface

(ert-deftest detached-test-session-started-p ()
  (detached-test--with-temp-database
   (cl-letf* ((session (detached-create-session "foo")))
     (should (not (detached-session-started-p session)))
     (setf (detached--session-state session) 'active)
     (should (detached-session-started-p session)))))

(ert-deftest detached-test-session-status ()
  (let ((failed-session (detached--session-create :status `(failure . 128))))
    (should (detached-session-failed-p failed-session))
    (should (= 128 (detached-session-exit-code failed-session)))))

(ert-deftest detached-test-session-host ()
  ;; Remotehost session
  (let ((remote-session (detached--session-create :host `("foo" . remotehost))))
    (should (detached-session-remotehost-p remote-session))
    (should (not (detached-session-localhost-p remote-session)))
    (should (string= "foo" (detached-session-host-name remote-session))))
  ;; Localhost session
  (let ((local-session (detached--session-create :host `("bar" . localhost))))
    (should (not (detached-session-remotehost-p local-session)))
    (should (detached-session-localhost-p local-session))
    (should (string= "bar" (detached-session-host-name local-session)))))

;;;;; Other

;; (ert-deftest detached-test-session-start-command ()
;;   (detached-test--with-temp-database
;;    (cl-letf* ((detached-tail-program "tail")
;;               (detached-dtach-program "dtach")
;;               (detached-shell-program "bash")
;;               (detached-show-session-context t)
;;               (detached-session-context-lines 20))

;;      ;; ;; Create and attach
;;      ;; (let* ((session (detached-create-session "ls -la"))
;;      ;;        (log (detached--session-file session 'log t))
;;      ;;        (expected-list `(,detached-tail-program
;;      ;;                         "-F"
;;      ;;                         "-n" ,(number-to-string detached-session-context-lines)
;;      ;;                         ,log)))
;;      ;;   (setf (detached--session-initial-mode session) 'attached)
;;      ;;   (should (equal expected-list (detached-session-start-command session
;;      ;;                                                                :type 'list))))

;;      ;; Create and attach to degraded session
;;      (let* ((detached-degraded-commands '("ls"))
;;             (session (detached-create-session "ls -la"))
;;             (log (detached--session-file session 'log t))
;;             (expected-list `(,detached-tail-program
;;                              "-F"
;;                              "-n" ,(number-to-string detached-session-context-lines)
;;                              ,log)))
;;        (setf (detached--session-initial-mode session) 'attached)
;;        (should (equal expected-list (detached-session-start-command session
;;                                                                     :type 'list))))

;;      ;; Create

;;      )))

(ert-deftest detached-test-dtach-command ()
  (detached-test--with-temp-database
   (cl-letf* ((detached-dtach-program "dtach")
			  (detached-shell-program "bash")
			  (session (detached-create-session "ls -la"))
			  (detached-show-session-context t)
			  (detached-session-context-lines 20)
			  (detached-tail-program "tail")
			  ((symbol-function #'detached-create-session)
			   (lambda (_)
				 session))
			  ((symbol-function #'detached--detached-command)
			   (lambda (_)
				 (format "{ detached-command }"))))
	 (let* ((detached-session-mode 'attached)
			(expected `(,detached-dtach-program
						"-c" ,(detached--session-file session 'socket t)
						"-z" ,detached-shell-program
						"-c"
						"{ detached-command }"))
			(expected-concat (format "%s -c %s -z %s -c %s"
									 detached-dtach-program
									 (detached--session-file session 'socket t)
									 detached-shell-program
									 "\\{\\ detached-command\\ \\}")))
	   (should (equal expected (detached--dtach-command session)))
	   (should (equal expected-concat (detached--dtach-command session t))))
	 (let* ((detached-session-mode 'attach)
			(log (detached--session-file session 'log t))
			(expected `(,detached-tail-program
                        "-n"
                        ,(number-to-string detached-session-context-lines)
						,(format "%s;" log)
						,detached-dtach-program "-a" ,(detached--session-file session 'socket t) "-r" "none"))
			(expected-concat (format "%s %s; %s -a %s -r none"
                                     (format "%s -n %s" detached-tail-program detached-session-context-lines)
									 log
									 detached-dtach-program
									 (detached--session-file session 'socket t))))
	   (should (equal expected (detached--dtach-command session)))
	   (should (equal expected-concat (detached--dtach-command session t)))))))

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
	(should (equal '("localhost" . localhost) (detached--host))))
  (let ((default-directory "/ssh:remotehost:/home/user/git"))
	(should (equal '("remotehost" . remotehost) (detached--host)))))

(ert-deftest detached-test-session-active-p ()
  (detached-test--with-temp-database
   (let ((session (detached-test--create-session :command "foo" :host '("bar" . localhost))))
	 (should (eq 'active (detached--determine-session-state session)))
	 (detached-test--change-session-state session 'deactivate)
	 (should (eq 'inactive (detached--determine-session-state session))))))

(ert-deftest detached-test-session-dead-p ()
  (detached-test--with-temp-database
   (let ((session (detached-test--create-session :command "foo" :host '("bar" . localhost))))
	 (should (not (detached--session-missing-p session)))
	 (detached-test--change-session-state session 'deactivate)
	 (should (not (detached--session-missing-p session)))
	 (detached-test--change-session-state session 'kill)
	 (should (detached--session-missing-p session)))))

(ert-deftest detached-test-cleanup-host-sessions ()
  (detached-test--with-temp-database
   (cl-letf* ((session1 (detached-test--create-session :command "foo" :host '("remotehost" . remotehost)))
			  (session2 (detached-test--create-session :command "bar" :host '("localhost" . localhost)))
			  (session3 (detached-test--create-session :command "baz" :host '("localhost" . localhost)))
			  (host '("localhost" . localhost))
			  ((symbol-function #'detached--host) (lambda () host)))
	 ;; One inactive, one missing, one active
	 (detached-test--change-session-state session1 'deactivate)
	 (detached-test--change-session-state session2 'kill)
	 (detached--cleanup-host-sessions "localhost")
	 (detached--db-get-sessions)
	 (should (seq-set-equal-p
			  (detached--db-get-sessions)
			  `(,session1 ,session3))))))

(ert-deftest detached-test-dtach-arg ()
  (let ((detached-session-mode 'detached))
	(should (string= "-n" (detached--dtach-arg))))
  (let ((detached-session-mode 'attached))
	(should (string= "-c" (detached--dtach-arg))))
  (let ((detached-session-mode 'attach))
	(should (string= "-a" (detached--dtach-arg))))
  (let ((detached-session-mode nil))
	(should-error (detached--dtach-arg))))

;;;;; Database

(ert-deftest detached-test-db-insert-session ()
  (detached-test--with-temp-database
   (let* ((session (detached-test--create-session :command "foo" :host '("localhost" . localhost))))
	 (should (equal (detached--db-get-sessions) `(,session))))))

(ert-deftest detached-test-db-remove-session ()
  (detached-test--with-temp-database
   (let* ((session1 (detached-test--create-session :command "foo" :host '("host" . localhost)))
		  (session2 (detached-test--create-session :command "bar" :host '("host" . localhost))))
	 (should (seq-set-equal-p `(,session1 ,session2) (detached--db-get-sessions)))
	 (detached--db-remove-entry session1)
	 (should (seq-set-equal-p `(,session2) (detached--db-get-sessions))))))

(ert-deftest detached-test-db-update-session ()
  (detached-test--with-temp-database
   (let* ((session (detached-test--create-session :command "foo" :host '("host" . localhost)))
          (id (detached-session-id session))
          (copy))
     (setq copy (copy-detached-session session))
     (setf (detached--session-state copy) nil)
     (should (not (equal copy (detached--db-get-session id))))
     (detached--db-update-entry copy)
     (should (equal copy (car (detached--db-get-sessions)))))))

(ert-deftest detached-test-detached-command ()
  (let ((detached-shell-program "bash")
		(detached-script-program "script")
		(detached-tee-program "tee")
		(detached-terminal-data-command "script --quiet --flush --return --command \"%s\" /dev/null")
		(terminal-data-session
		 (detached--session-create :directory "/tmp/detached/"
								   :working-directory "/home/user/"
								   :command "ls -la"
								   :degraded nil
								   :text-mode 'terminal-data
								   :id 'foo123))
		(degraded-plain-text-session
		 (detached--session-create :directory "/tmp/detached/"
								   :working-directory "/home/user/"
								   :command "ls -la"
								   :degraded t
								   :text-mode 'plain-text
								   :id 'foo123)))
	(should (string= "{ bash -c if\\ TERM\\=eterm-color\\ script\\ --quiet\\ --flush\\ --return\\ --command\\ \\\"ls\\ -la\\\"\\ /dev/null\\;\\ then\\ true\\;\\ else\\ echo\\ \\\"\\[detached-exit-code\\:\\ \\$\\?\\]\\\"\\;\\ fi; } 2>&1 | tee /tmp/detached/foo123.log"
					 (detached--detached-command terminal-data-session)))
	(should (string= "{ bash -c if\\ ls\\ -la\\;\\ then\\ true\\;\\ else\\ echo\\ \\\"\\[detached-exit-code\\:\\ \\$\\?\\]\\\"\\;\\ fi; } &> /tmp/detached/foo123.log"
					 (detached--detached-command degraded-plain-text-session)))))

(ert-deftest detached-test-degraded-command-p ()
  (let ((detached-degraded-commands '("ls")))
	(should (not (detached-degraded-command-p "cd")))
	(should (detached-degraded-command-p "ls -la"))))

(ert-deftest detached-test-get-session-directory ()
  (let ((default-directory "/ssh:remotehost:/home/user/git")
		(detached-session-directory "/tmp/detached"))
	;; Remote session directory
	(should (string= "/ssh:remotehost:/tmp/detached" (detached--get-session-directory)))
	(let ((detached-local-session t))
	  ;; Enforced local session directory with `detached-local-session'
	  (should (string= "/tmp/detached" (detached--get-session-directory)))))
  (let ((default-directory "/home/user/git")
		(detached-session-directory "/tmp/detached"))
	;; Local session directory
	(should (string= "/tmp/detached" (detached--get-session-directory)))))

;;;;; String representations

(ert-deftest detached-test-duration-str ()
  (should (string= "1s" (detached--duration-str
						 (detached--session-create :time '(:duration 1) :state 'inactive))))
  (should (string= "1m 1s" (detached--duration-str
							(detached--session-create :time '(:duration 61) :state 'inactive))))
  (should (string= "1h 1m 1s" (detached--duration-str
							   (detached--session-create :time '(:duration 3661) :state 'inactive)))))

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
  ;; Accessible sessions
  (cl-letf (((symbol-function #'detached--session-accessible-p) (lambda (_) t)))
	(should (string= "*" (detached--state-str
						  (detached--session-create :state 'active))))
	(should (string= "" (detached--state-str
						 (detached--session-create :state 'inactive))))
	(should (string= "?" (detached--state-str
						  (detached--session-create :state 'unknown)))))

  ;; Inaccessible sessions
  (cl-letf (((symbol-function #'detached--session-accessible-p) (lambda (_) nil)))
	(should (string= "?" (detached--state-str
						  (detached--session-create :state 'active))))
	(should (string= "" (detached--state-str
						 (detached--session-create :state 'inactive))))
	(should (string= "?" (detached--state-str
						  (detached--session-create :state 'unknown))))))

(ert-deftest detached-test-working-dir-str ()
  (should
   (string= "/home/user/repo"
			(detached--working-dir-str
			 (detached--session-create :working-directory "/ssh:remote:/home/user/repo"))))
  (should
   (string= "~/repo"
			(detached--working-dir-str
			 (detached--session-create :working-directory "~/repo")))))

(ert-deftest detached-test-verify-db-compatbility ()
  ;; Database version is older than minimum version
  (cl-letf (((symbol-function #'detached--db-session-version) (lambda () (format "0.9.1.1")))
			(detached-minimum-session-version "0.9.1.2"))
	(should (not (detached--verify-db-compatibility))))
  ;; Database version is equal to minimum version
  (cl-letf (((symbol-function #'detached--db-session-version) (lambda () (format "0.9.1.1")))
			(detached-minimum-session-version "0.9.1.1"))
	(should (detached--verify-db-compatibility)))
  ;; Database version is newer than minimum version
  (cl-letf (((symbol-function #'detached--db-session-version) (lambda () (format "0.9.1.2")))
			(detached-minimum-session-version "0.9.1.1"))
	(should (detached--verify-db-compatibility))))

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

(ert-deftest detached-test-env-message-filter ()
  (let ((str "output\n[detached-exit-code: 127]\n"))
	(should (string= "output" (detached--env-message-filter str)))))

(provide 'detached-test)

;;; detached-test.el ends here
