#!/bin/sh

# Write the module the build compiles in beside the library.
#
# Three answers and no others:
#
#   - the vendored tree is the release vendor/jasmin.lock pins: its stamp,
#     vendor/jasmin/.source-sha256, is the lock's digest, and the answer is
#     that release's version;
#   - the vendored tree was assembled from another tarball, which is what
#     the edge channel does (`vendor/fetch.sh <tarball> <sha256>`): its stamp
#     is a different digest, and the answer is the first twelve hex digits of
#     that digest, because the lock's version would name a release the tree
#     is not;
#   - there is no vendored tree: the library came from the switch.
#
# A tree with no stamp is taken as the lock's release, which is what every
# tree vendor/fetch.sh assembled before it wrote stamps was.
#
# The script names no absolute path and prints no path at all: the module
# holds a release number, a digest prefix or nothing, so a checkout's
# location cannot reach the binary that reports it.
#
# Its input is read from the build directory, where dune has copied the
# dependencies the stanza declares: ../vendor/jasmin/ and ../vendor/jasmin.lock.

set -eu

lock_value() {
  sed -n "s/^[[:space:]]*$1[[:space:]]*=[[:space:]]*//p" \
    ../vendor/jasmin.lock 2>/dev/null | sed -n '1s/[[:space:]]*$//p'
}

release=''
tarball=''
if [ -d ../vendor/jasmin ]; then
  release=$(lock_value version)
  stamp=''
  if [ -f ../vendor/jasmin/.source-sha256 ]; then
    stamp=$(sed -n '1s/[[:space:]]*$//p' ../vendor/jasmin/.source-sha256)
  fi
  if [ -n "$stamp" ] && [ "$stamp" != "$(lock_value sha256)" ]; then
    release=''
    tarball=$(printf '%s' "$stamp" | cut -c1-12)
  fi
fi

case $release in
'') printf 'let vendored_release : string option = None\n' ;;
*) printf 'let vendored_release = Some "%s"\n' "$release" ;;
esac
case $tarball in
'') printf 'let vendored_tarball : string option = None\n' ;;
*) printf 'let vendored_tarball = Some "%s"\n' "$tarball" ;;
esac
