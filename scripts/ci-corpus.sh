#!/bin/sh
set -eu

version=$(sed -n 's/^version *= *//p' vendor/jasmin.lock)
git clone --quiet --depth 1 --branch "v$version" https://github.com/jasmin-lang/jasmin.git "$1"
echo "corpus: Jasmin $version in $1"
