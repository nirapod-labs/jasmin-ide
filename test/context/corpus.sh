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
  # consts.jinc with no entry beside it: a file no entry requires is checked
  # on its own (design 8.4), which is what this row observes. The corpus file
  # is copied unchanged; required-file.t pins its bytes by digest, and this
  # row prints no digest of its own.
  rm -rf "$work/alone"
  mkdir -p "$work/alone"
  cp "$consts" "$work/alone/consts.jinc"
  check "$work/alone" consts.jinc
  ;;
context)
  # The same consts.jinc named as the document in the directory that holds
  # its entry: kem.jazz requires kem.jinc, which requires indcpa.jinc,
  # which requires poly.jinc, which requires consts.jinc after params.jinc.
  # The checker reads that chain from the entry up to the require that names
  # the file (design 8.4), so the file types in the environment params.jinc
  # leaves behind and is clean. The entry beside it is checked too, to show
  # that the context it gives is one the whole entry types in.
  printf 'consts.jinc sha256 %s\n' "$(digest "$consts")"
  check "$root/$unit" consts.jinc
  check "$root/$unit" kem.jazz
  ;;
scoped)
  # The same consts.jinc, unchanged, behind an entry that declares the
  # parameters first: the context the rule builds, supplied by hand until
  # the engine supplies it. The entry uses jdmontx16, the last array
  # consts.jinc declares, so its verdict is about the file and not about
  # the clearance of the two requires: a require whose items fail leaves
  # the environment it found, and then no declaration of the file is in
  # scope. The second check is that control, over a copy of the file whose
  # first MLKEM_Q at 73:18, the token the `alone` row reports, is
  # MLKEM_NOPE.
  rm -rf "$work/scoped"
  mkdir -p "$work/scoped/control"
  cp "$params" "$work/scoped/params.jinc"
  cp "$params" "$work/scoped/control/params.jinc"
  cp "$consts" "$work/scoped/consts.jinc"
  cat >"$work/scoped/entry.jazz" <<'EOF'
require "params.jinc"
require "consts.jinc"

fn probe() -> reg u16 {
  reg u16 x;
  x = jdmontx16[0];
  return x;
}
EOF
  cp "$work/scoped/entry.jazz" "$work/scoped/control/entry.jazz"
  printf 'consts.jinc sha256 %s\n' "$(digest "$work/scoped/consts.jinc")"
  check "$work/scoped" entry.jazz
  sed '73s/MLKEM_Q/MLKEM_NOPE/' "$consts" >"$work/scoped/control/consts.jinc"
  printf 'control: consts.jinc:73:18 is MLKEM_NOPE, not MLKEM_Q\n'
  printf 'consts.jinc sha256 %s\n' "$(digest "$work/scoped/control/consts.jinc")"
  check "$work/scoped/control" entry.jazz
  ;;
required)
  # The same consts.jinc behind an entry that does not declare them, so the
  # file holds the error the `alone` row records. The digest pins the bytes
  # as the other rows pin theirs. The last line counts the diagnostic lines
  # the check placed in consts.jinc, `consts.jinc:<line>:...`, and not the
  # per-file summary `consts.jinc: <n> error`, which names the file too.
  rm -rf "$work/required"
  mkdir -p "$work/required"
  cp "$consts" "$work/required/consts.jinc"
  cat >"$work/required/entry.jazz" <<'EOF'
require "consts.jinc"
EOF
  printf 'consts.jinc sha256 %s\n' "$(digest "$work/required/consts.jinc")"
  output=$(cd -- "$work/required" && "$server" check entry.jazz 2>&1)
  status=$?
  if [ -n "$output" ]; then
    printf '%s\n' "$output"
  fi
  printf 'status %s\n' "$status"
  in_file=$(printf '%s\n' "$output" | grep -c '^consts\.jinc:[0-9]')
  printf 'diagnostics in consts.jinc: %s\n' "$in_file"
  ;;
*)
  printf 'unknown row: %s\n' "$row" >&2
  exit 2
  ;;
esac
