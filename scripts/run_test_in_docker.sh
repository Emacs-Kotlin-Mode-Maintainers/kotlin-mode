#!/bin/sh

# Run tests in Docker.  Used in Makefile.

# Cask does't support Emacs 24.
for version in 28 27 26 25 # 24
do
    rm -f *.elc test/*.elc
    rm -f *-autoloads.el
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --user="$(id -u):$(id -g)" \
        --workdir="/src" \
        --env=ELDEV_DIR=/src/.eldev \
        --env=HOME=/tmp \
        silex/emacs:${version} \
        bash -c "/src/scripts/run_test.sh" \
        || exit 1
done

echo "done"
