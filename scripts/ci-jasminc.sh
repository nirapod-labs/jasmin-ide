#!/bin/sh
set -eu

root=$1
version=$(sed -n 's/^version *= *//p' vendor/jasmin.lock)

opam init --root "$root" --bare --no-setup --disable-sandboxing -y default https://opam.ocaml.org
opam switch create --root "$root" -y jasminc ocaml-base-compiler.4.14.4
opam install --root "$root" --switch jasminc -y "jasmin.$version"
"$root/jasminc/bin/jasminc" -version
