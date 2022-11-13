byte-compile:
	emacs --batch --load=detached.el --eval='(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))' ./*.el

style:
	emacs --batch --eval="(let ((make-backup-files nil) (indent-tabs-mode nil) (tab-width 4) (files (directory-files-recursively default-directory \"detached.*\.el\"))) (seq-do (lambda (it) (find-file it) (whitespace-cleanup) (indent-region (point-min) (point-max)) (save-buffer)) files))"

autoloads:
	emacs --batch --eval='(progn (setq make-backup-files nil) (make-directory-autoloads default-directory "detached-autoloads.el"))'

tests:
	emacs --batch --load=detached.el --load=test/detached-test.el --funcall=ert-run-tests-batch-and-exit

docs:
	emacs --batch --eval='(progn (setq make-backup-files nil) (find-file "doc/detached.org") (org-texinfo-export-to-info))'

clean:
	rm -f *.elc
	rm -f doc/*.texi
	rm -f detached-autoloads.el

all: byte-compile autoloads tests docs clean
