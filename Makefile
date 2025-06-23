.PHONY: test

test:
	emacs -Q --batch \
	      -L . \
	      -l ert \
	      -l tests/test-ob-cobol.el \
	      -f ert-run-tests-batch-and-exit
