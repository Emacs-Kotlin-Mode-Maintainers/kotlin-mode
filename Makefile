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
	CASK="${CASK}" EMACS="${EMACS}" scripts/run_test.sh

test_in_docker:
## Tests the package in Docker.
	CASK="${CASK}" EMACS="${EMACS}" scripts/run_test_in_docker.sh

lint:
## Run linters.
	CASK="${CASK}" EMACS="${EMACS}" scripts/run_linter.sh

lint_in_docker:
## Run linters in Docker.
	CASK="${CASK}" EMACS="${EMACS}" scripts/run_linter_in_docker.sh
