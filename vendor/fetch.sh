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

# Copy the archive named by $1 to $2, from the network or this machine.
obtain() {
  case $1 in
    http://* | https://*)
      if command -v curl >/dev/null 2>&1; then
        curl -fsSL --retry 3 -o "$2" "$1"
      elif command -v wget >/dev/null 2>&1; then
        wget -q -O "$2" "$1"
      else
        die "no curl or wget is installed, so $1 cannot be downloaded"
      fi
      ;;
    file://*)
      cp "${1#file://}" "$2"
      ;;
    *)
      [ -f "$1" ] || die "no such file: $1"
      cp "$1" "$2"
      ;;
  esac
}

case $# in
  0)
    [ -f "$lock" ] || die "no arguments and no $lock to read them from"
    url=$(lock_value upstream)
    version=$(lock_value version)
    sha=$(lock_value sha256)
    [ -n "$url" ] || die "$lock has no upstream line"
    [ -n "$sha" ] || die "$lock has no sha256 line"
    ;;
  2)
    url=$1
    sha=$2
    version=
    ;;
  *)
    usage
    ;;
esac

case $sha in
  *[!0-9a-f]* | '') die "not a SHA-256 digest: $sha" ;;
esac
[ "${#sha}" -eq 64 ] || die "not a SHA-256 digest: $sha"

if [ -d "$dest" ] && [ -f "$dest/.source-sha256" ] &&
  [ "$(cat "$dest/.source-sha256")" = "$sha" ]; then
  say "vendor/jasmin/ is already assembled from $sha, nothing to do"
else
  work=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-fetch.XXXXXX")
  cleanup() { rm -rf "$work" "$stage"; }
  trap cleanup EXIT HUP INT TERM

  # 1. Take the bytes and check them before they are unpacked.
  archive=$work/source
  obtain "$url" "$archive"
  actual=$(sha256_of "$archive")
  [ "$actual" = "$sha" ] || die "digest mismatch: wanted $sha, got $actual"
  say "digest ok: $sha"

  # 2. Unpack into a private directory and insist on the upstream layout.
  mkdir "$work/unpacked"
  tar -xf "$archive" -C "$work/unpacked"

  entries=0
  root=
  for e in "$work/unpacked"/* "$work/unpacked"/.[!.]*; do
    if [ -e "$e" ] || [ -L "$e" ]; then
      entries=$((entries + 1))
      root=$e
    fi
  done
  [ "$entries" -eq 1 ] || die "the archive must hold one top-level directory, found $entries"
  [ -d "$root" ] || die "the archive does not hold a directory at its top level"

  base=$(basename "$root")
  case $base in
    jasmin-compiler-v[0-9]* | jasmin-compiler) ;;
    *) die "unexpected top-level directory: $base" ;;
  esac
  if [ -n "$version" ]; then
    [ "$base" = "jasmin-compiler-v$version" ] ||
      die "the archive is $base, the lock records version $version"
  fi

  for d in src uint63; do
    [ -d "$root/compiler/$d" ] || die "the archive has no compiler/$d"
  done
  for f in LICENSE MANIFEST; do
    [ -f "$root/compiler/$f" ] || die "the archive has no compiler/$f"
  done
  [ -f "$root/compiler/src/dune" ] || die "the archive has no compiler/src/dune"

  # 3. Assemble the library tree.
  rm -rf "$stage"
  mkdir -p "$stage"
  cp -R "$root/compiler/src" "$stage/src"
  cp -R "$root/compiler/uint63" "$stage/uint63"
  cp "$root/compiler/LICENSE" "$stage/LICENSE"
  cp "$root/compiler/MANIFEST" "$stage/MANIFEST"

  # 4. Drop the public names. A library that has one must belong to a
  #    package of this workspace, and the vendor tree has no package of its
  #    own, so the libraries stay private and are reached by their internal
  #    names: `jasmin` and `uint63`. The implementations of the virtual
  #    library name it the same way. See vendor/README.md.
  find "$stage" -type f -name dune -print >"$work/dune-files"
  public_before=$(grep -rI -E '\(public_name[[:space:]]+jasmin\.' "$stage" | wc -l | tr -d ' ')
  [ "$public_before" -gt 0 ] || die "no public_name jasmin.* line in the tree"
  while IFS= read -r f; do
    awk '/^[[:space:]]*\(public_name[[:space:]]+jasmin\.[^)]*\)[[:space:]]*$/ {next}
         {gsub(/\(implements jasmin\.uint63\)/, "(implements uint63)"); print}' \
      "$f" >"$f.new"
    mv "$f.new" "$f"
  done <"$work/dune-files"
  public_after=$(grep -rI -E '\(public_name[[:space:]]+jasmin\.' "$stage" | wc -l | tr -d ' ')
  [ "$public_after" -eq 0 ] || die "a public_name jasmin.* line survived the strip"
  leftovers=$(grep -rI -E '\(implements[[:space:]]+jasmin\.' "$stage" | wc -l | tr -d ' ')
  [ "$leftovers" -eq 0 ] || die "an implements line still names a public library"
  grep -q -E '^[[:space:]]*\(name[[:space:]]+jasmin\)' "$stage/src/dune" ||
    die "src/dune no longer declares the jasmin library"
  say "stripped $public_before public names from $base/compiler/{src,uint63}"

  # 5. Put the tree in place and stamp it, so a repeat run is a no-op.
  rm -rf "$dest"
  mv "$stage" "$dest"
  printf '%s\n' "$sha" >"$dest/.source-sha256"
  say "assembled vendor/jasmin/ from $base"
fi
