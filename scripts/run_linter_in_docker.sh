#!/bin/sh

# Run linter in Docker.  Used in Makefile.

# Cask does't support Emacs 24.
# Indentation rules changed since 29.
for version in 29 # 28 27 26 25 # 24
do
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --user="$(id -u):$(id -g)" \
        --workdir="/src" \
        --env=ELDEV_DIR=/src/.eldev \
        --env=HOME=/tmp \
        silex/emacs:${version}-ci-cask \
        bash -c "/src/scripts/run_linter.sh" \
        || exit 1
done

echo "done"
