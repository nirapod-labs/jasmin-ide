#!/bin/sh
# One row of the multi-file corpus checks, driven through `jasmin-lsp check`.
#
# usage: corpus.sh <jasmin-lsp> <corpus-root> <row>
#
#   <jasmin-lsp>   the built server; it takes `check` as its first word
#   <corpus-root>  a libjade checkout whose files match the expected output
#                  of the tests here (docs/context-tests.md names it)
#   <row>          entry | alone | context | scoped | required
#
# Every check runs inside the directory that holds the file, so the checker
# prints a path relative to it and the output does not depend on where the
# corpus sits on the host. The corpus is read and never written: the rows
# that need a file of their own write it, and a copy of the corpus files
# they use, into work/ in the directory this script runs in.
#
# A row reports what the check printed and the status it exited with; the
# script itself fails only when the corpus does not hold the files.

set -u

if [ "$#" -ne 3 ]; then
  printf 'usage: corpus.sh <jasmin-lsp> <corpus-root> <row>\n' >&2
  exit 2
fi

server=$1
root=$2
row=$3

if [ -z "$root" ] || [ ! -d "$root" ]; then
  printf 'no corpus root: name a libjade checkout, with JASMIN_CONTEXT_CORPUS\n' >&2
  printf 'in the environment or as the second argument; got: %s\n' "$root" >&2
  exit 2
fi

# Both paths are used after a cd, so each is resolved to an absolute one
# once, here: the server, which the tests name relative to their own
# directory, and the corpus root, which they name in the environment.
server=$(cd -- "$(dirname -- "$server")" && pwd)/$(basename -- "$server")
root=$(cd -- "$root" && pwd)

# The libjade directory the rows are written against: the AVX2 ML-KEM 768
# unit. Its entry, kem.jazz, requires eighteen .jinc files, directly or
# through the others, and two of them are the files the rows below use:
# params.jinc declares the parameters, and consts.jinc uses them.
unit=oldsrc-should-delete/crypto_kem/mlkem/mlkem768/amd64/avx2
params=$root/$unit/params.jinc
consts=$root/$unit/consts.jinc

work=$PWD/work

digest() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | cut -d' ' -f1
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | cut -d' ' -f1
  else
    printf 'no sha256sum and no shasum\n' >&2
    exit 2
  fi
}

missing=
for file in "$root/$unit/kem.jazz" "$params" "$consts"; do
  [ -f "$file" ] || missing="$missing $file"
done
if [ -n "$missing" ]; then
  printf 'the corpus does not hold the files these rows use; missing:%s\n' "$missing" >&2
  exit 2
fi

# check <directory> <file>... runs the checker there and prints what it
# wrote plus its exit status. The status is reported, not returned: a file
# that fails to check is an observation, not a broken test.
check() {
  directory=$1
  shift
  output=$(cd -- "$directory" && "$server" check "$@" 2>&1)
  status=$?
  if [ -n "$output" ]; then
    printf '%s\n' "$output"
  fi
  printf 'status %s\n' "$status"
}

case $row in
entry)
  # The corpus entry itself, with its whole closure read from disk.
  check "$root/$unit" kem.jazz
  ;;
alone)
  # consts.jinc as the document, beside the params.jinc it uses and with
  # none of those parameters in scope.
  check "$root/$unit" consts.jinc
  ;;
context)
  # The same consts.jinc named as the document in the directory that holds
  # its entry: kem.jazz requires kem.jinc, which requires indcpa.jinc,
  # which requires poly.jinc, which requires consts.jinc after params.jinc.
  # The engine reads no requiring entry yet, so the file is checked as one
  # of its own, and the entry beside it is checked too, to show that the
  # context it needs does exist in the corpus.
  printf 'consts.jinc sha256 %s\n' "$(digest "$consts")"
  check "$root/$unit" consts.jinc
  check "$root/$unit" kem.jazz
  ;;
scoped)
  # The same consts.jinc, unchanged, behind an entry that declares the
  # parameters first: the context the rule builds, supplied by hand until
  # the engine supplies it.
  rm -rf "$work/scoped"
  mkdir -p "$work/scoped"
  cp "$params" "$work/scoped/params.jinc"
  cp "$consts" "$work/scoped/consts.jinc"
  cat >"$work/scoped/entry.jazz" <<'EOF'
require "params.jinc"
require "consts.jinc"
EOF
  printf 'consts.jinc sha256 %s\n' "$(digest "$work/scoped/consts.jinc")"
  check "$work/scoped" entry.jazz
  ;;
required)
  # The same consts.jinc behind an entry that does not declare them, so the
  # file holds the error the `alone` row records. The count of diagnostics
  # the check places in consts.jinc is printed after the entry's own lines.
  rm -rf "$work/required"
  mkdir -p "$work/required"
  cp "$consts" "$work/required/consts.jinc"
  cat >"$work/required/entry.jazz" <<'EOF'
require "consts.jinc"
EOF
  output=$(cd -- "$work/required" && "$server" check entry.jazz 2>&1)
  status=$?
  if [ -n "$output" ]; then
    printf '%s\n' "$output"
  fi
  printf 'status %s\n' "$status"
  in_file=$(printf '%s\n' "$output" | grep -c '^consts\.jinc:')
  printf 'diagnostics in consts.jinc: %s\n' "$in_file"
  ;;
*)
  printf 'unknown row: %s\n' "$row" >&2
  exit 2
  ;;
esac
