CASK ?= cask
EMACS ?= emacs
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)

SRC = $(wildcard *.el)
PACKAGE = dist/kotlin-mode-$(VERSION).tar

.PHONY: help all deps package install test clean

help:
## Shows this message.
# Process this Makefile with following filters
#
# - Remove empty line.
# - Remove line starting with whitespace, dot, or uppercase letters.
# - Remove line containing ## no-doc.
# - Remove after colon if the line is not a comment line.
# - Replace /^## / to "  ".
# - Remove other comment lines.
# - Insert newline before rules.
	@sed -e '/^\s*$$/d; /^[	.A-Z]/d; /## no-doc/d; s/^\([^#][^:]*\):.*/\1/; s/^## /  /; /^#/d; s/^[^ ]/\n&/' Makefile

all: package
## Builds the package.

deps:
## Installs the dependencies.
	$(CASK) install

$(PACKAGE): $(SRC) deps ## no-doc
	rm -rf dist
	$(CASK) package

package: $(PACKAGE)
## Builds the package.

install: package
## Installs the package.
	$(CASK) exec $(EMACS) --batch \
	  -l package \
	  -f package-initialize \
	  -f package-refresh-contents \
	  --eval '(package-install-file "$(PACKAGE)")'

clean:
## Cleans the dist directory and *.elc.
	rm -rf dist *.elc

test:
## Tests the package.
	$(CASK) exec $(EMACS) --batch -q \
	  --eval "(add-to-list 'load-path \""$(shell readlink -f .)"\")" \
	  --eval "(add-to-list 'load-path \""$(shell readlink -f .)"/test\")" \
	  -f batch-byte-compile \
	  *.el
	cask exec ert-runner -L . -L test

lint:
## Run linters.
	rm -f *.elc
	rm -f kotlin-mode-autoloads.el
	$(CASK) exec $(EMACS) --batch -Q \
	  -l elisp-lint.el \
	  --eval '(setq elisp-lint--debug t)' \
	  -f elisp-lint-files-batch \
	  *.el
	rm -f *.elc
	rm -f kotlin-mode-autoloads.el
