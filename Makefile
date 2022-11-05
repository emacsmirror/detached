byte-compile:
	emacs --batch --load=detached.el --eval='(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))' ./*.el

autoloads:
	emacs --batch --eval='(progn (setq make-backup-files nil) (make-directory-autoloads default-directory "detached-autoloads.el"))'

tests:
	emacs --batch --load=detached.el --load=test/detached-test.el --funcall=ert-run-tests-batch-and-exit

docs:
	emacs --batch --eval='(progn (setq make-backup-files nil) (find-file "doc/detached.org") (org-texinfo-export-to-info))'

clean:
	rm *.elc
	rm doc/*.texi

all: byte-compile autoloads tests docs clean
