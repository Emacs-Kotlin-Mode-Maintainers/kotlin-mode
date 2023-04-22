#!/bin/sh

# Run linter in Docker.  Used in Makefile.

# Cask does't support Emacs 24.
for version in 28 27 26 25 # 24
do
    ARGS=$( [ "$version" -lt 26 ] && echo "--no-checkdoc" )
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --user="$(id -u):$(id -g)" \
        --workdir="/src" \
        --env=ELDEV_DIR=/src/.eldev \
        --env=HOME=/tmp \
        silex/emacs:${version}-ci \
        bash -c "/src/scripts/run_linter.sh $ARGS" \
        || exit 1
done

echo "done"
