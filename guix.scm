;;; guix.scm -- Guix package definition

(use-modules
 (guix packages)
 (guix git-download)
 (guix gexp)
 (guix build-system gnu)
 ((guix licenses) #:prefix license:)
 (guix build-system emacs)
 (gnu packages emacs-xyz)
 (gnu packages screen)
 (ice-9 popen)
 (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define %git-commit
  (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f2" OPEN_READ)))

(define-public emacs-dtache
  (let ((branch "remote")
        (commit "220f93dfa710474b4f9c9db0349a6082374f80c0")
        (revision "0"))
    (package
     (name "emacs-dtache")
     (version (git-version "0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/niklaseklund/dtache")
             (commit commit)))
       (sha256
        (base32
         "0yvkygdqghrp8xn7nfgfq3x5y913r6kasqndxy3fr2dqpxxb941a"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (native-inputs
      `(("emacs-ert-runner" ,emacs-ert-runner)))
     (inputs `(("dtach" ,dtach)))
     (arguments
      `(#:tests? #t
        #:test-command '("ert-runner")))
     (home-page "https://gitlab.com/niklaseklund/dtache")
     (synopsis "Dtach Emacs")
     (description "Dtache allows a program to be seamlessly executed
in an environment that is isolated from Emacs.")
     (license license:gpl3+))))

(package
  (inherit emacs-dtache)
  (name "emacs-dtache-git")
  (version (git-version (package-version emacs-dtache) "HEAD" %git-commit))
  (source (local-file %source-dir
                      #:recursive? #t)))
