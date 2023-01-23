#!/bin/sh

# Run linter.  Used in Makefile.

if ! [ -d ".cask/$(./scripts/invoke_cask.sh eval '(princ emacs-version)')/elpa/elisp-lint-"* ]
then
    ./scripts/invoke_cask.sh install || exit 1
fi

./scripts/invoke_cask.sh emacs --version || exit 1
rm -f *.elc test/*.elc || exit 1
rm -f *-autoloads.el || exit 1
./scripts/invoke_cask.sh emacs --batch -Q \
  -l elisp-lint.el \
  --eval '(setq elisp-lint--debug t)' \
  -f elisp-lint-files-batch \
  *.el || exit 1
rm -f *.elc test/*.elc || exit 1
rm -f *-autoloads.el || exit 1
