#!/bin/sh

# Run tests.  Used in Makefile.

if ! [ -d ".cask/$(./scripts/invoke_cask.sh eval '(princ emacs-version)')/elpa/ert-runner-"* ]
then
    ./scripts/invoke_cask.sh install || exit 1
fi

./scripts/invoke_cask.sh emacs --version || exit 1
./scripts/invoke_cask.sh emacs --batch -q \
  --eval "(add-to-list 'load-path \"$(readlink -f .)\")" \
  --eval "(add-to-list 'load-path \"$(readlink -f .)/test\")" \
  -f batch-byte-compile \
  *.el test/*.el || exit 1
./scripts/invoke_cask.sh exec ert-runner -L . -L test
