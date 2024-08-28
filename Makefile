.PHONY: test

test:
	@rm -f .test-org-id-locations
	emacs -Q --batch \
	      -L . \
	      -l ert \
	      -l test-ob-cobol.el \
	      -f ert-run-tests-batch-and-exit
