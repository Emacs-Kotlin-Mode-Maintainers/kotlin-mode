all: test

test:
	cask exec emacs -batch -l ert -l test/kotlin-mode-test.el -f ert-run-tests-batch-and-exit

.PHONY: all test
