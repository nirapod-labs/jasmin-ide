#!/bin/sh

# Write the module the build compiles in beside the library.
#
# Two answers and no others. The vendored tree is the one the repository
# fetches into vendor/jasmin/; when it is there, its release is the version
# recorded in vendor/jasmin.lock, and when it is not, the library came from
# the switch and the answer is None. The script names no absolute path and
# prints no path at all: the module holds a release number or nothing, so a
# checkout's location cannot reach the binary that reports it.
#
# Its input is read from the build directory, where dune has copied the two
# dependencies the stanza declares: ../vendor/jasmin/ and ../vendor/jasmin.lock.

set -eu

if [ -d ../vendor/jasmin ]; then
  release=$(sed -n 's/^[[:space:]]*version[[:space:]]*=[[:space:]]*//p' \
    ../vendor/jasmin.lock 2>/dev/null | sed -n '1s/[[:space:]]*$//p')
else
  release=''
fi

case $release in
'') printf 'let vendored_release : string option = None\n' ;;
*) printf 'let vendored_release = Some "%s"\n' "$release" ;;
esac
