#!/bin/sh
# Assemble the vendored Jasmin library from a Jasmin source release.
#
#   vendor/fetch.sh [<url-or-path> <sha256>]
#
# With no arguments the source URL, the release version and the digest are
# read from vendor/jasmin.lock. The archive may be given as an http(s) URL, a
# file:// URL or a path on this machine; the digest is checked either way.
#
# The archive must hold a single top-level directory, jasmin-compiler-v<ver>/
# (the name upstream publishes), with the release layout of the compiler:
# src/, uint63/, LICENSE and MANIFEST. Only the parts the library needs are
# copied out, into vendor/jasmin/, which is not committed.
#
# The script is idempotent: with vendor/jasmin/ already assembled from the
# requested digest it does not download, unpack or copy anything.

set -eu

here=$(unset CDPATH
  cd -- "$(dirname -- "$0")" && pwd)
dest=$here/jasmin
stage=$here/.jasmin.new
lock=$here/jasmin.lock

say() { printf '%s\n' "vendor/fetch.sh: $*" >&2; }
die() { say "error: $*"; exit 1; }

usage() {
  say "usage: vendor/fetch.sh [<url-or-path> <sha256>]"
  exit 2
}

# The digest of a file, from whichever of the two usual tools is installed.
sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | cut -d' ' -f1
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | cut -d' ' -f1
  else
    die "neither sha256sum nor shasum is installed"
  fi
}

# The first "key = value" line of the lock file for the given key.
lock_value() {
  sed -n "s/^[[:space:]]*$1[[:space:]]*=[[:space:]]*//p" "$lock" |
    sed -n '1s/[[:space:]]*$//p'
}

