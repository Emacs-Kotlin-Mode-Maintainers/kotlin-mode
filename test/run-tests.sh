#!/bin/bash

cd ../
emacs -batch -l ert -l test/kotlin-mode-test.el -f ert-run-tests-batch-and-exit
cd test
