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

(define-public emacs-detached
  (package
    (name "emacs-detached")
    (version "0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~niklaseklund/detached.el")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "160h60vrpxslw6y290ndc065cc75dab58aq7kjqash94vkifnii2"))))
    (arguments
     (list
      #:tests? #t
      #:test-command #~(list "ert-runner")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'install-detached-env
            (lambda _
              (install-file "detached-env" (string-append #$output "/bin"))))
          (add-after 'unpack 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (make-file-writable "detached.el")
              (emacs-substitute-variables "detached.el"
                ("detached-env"
                 (string-append #$output "/bin/detached-env"))
                ("detached-dtach-program"
                 (search-input-file inputs "/bin/dtach"))
                ("detached-shell-program"
                 (search-input-file inputs "/bin/bash"))))))))
    (build-system emacs-build-system)
    (native-inputs (list emacs-ert-runner))
    (inputs (list dtach))
    (home-page "https://git.sr.ht/~niklaseklund/detached.el")
    (synopsis "A package to launch, and manage, detached processes")
    (description
     "The detached package allows users to run processes
detached from Emacs.  It provides integration with multiple built-in modes, as
well as providing an interface to attach and interact with the processes.")
    (license license:gpl3+)))

(package
  (inherit emacs-detached)
  (name "emacs-detached-git")
  (version (git-version (package-version emacs-detached) "HEAD" %git-commit))
  (source (local-file %source-dir
                      #:recursive? #t)))
