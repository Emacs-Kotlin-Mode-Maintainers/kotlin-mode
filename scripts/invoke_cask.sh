#!/bin/sh

# A little wrapper for Cask.  Used in Makefile.
# If ${CASK} is executable, use it.
# Otherwise, download the Cask to .cask/cask if not exists, then execute it.

if command -v "${CASK}" > /dev/null
then
    "${CASK}" "$@"
elif command -v .cask/cask/bin/cask > /dev/null
then
    .cask/cask/bin/cask "$@"
else
    mkdir -p .cask || exit 1
    git clone --depth 1 https://github.com/cask/cask.git .cask/cask || exit 1
    chmod a+x .cask/cask/bin/cask || exit 1
    .cask/cask/bin/cask "$@"
fi
