`--version` prints one line and exits 0: the server's own version, the Jasmin
version it links, and which source that Jasmin was built from. The protocol
is not started, so nothing is written to stderr either.

The line's shape is pinned exactly, with no generic wildcard, so a missing
part fails here rather than in a reader's eye. The route is the one part that
differs between a build from the vendored tree and a build from the installed
library, so the line does not name it here: the route is checked against the
record the build made of which one it used (`controller/build_route.ml`). It
is then checked against where each part comes from: the server version against
`controller/version.ml`, the Jasmin release against the lock file, and the
route against that record. A version that was bumped in one place and not the
other fails too, and a `--version` that reported a hard-coded string would not
pass.

  $ ../../../lsp-server/native/main.exe --version > out.txt 2> err.txt; echo "status $?"
  status 0
  $ sed -E 's/Jasmin Compiler [^,]+, built from .*\)$/Jasmin Compiler VERSION, built from ROUTE)/' out.txt
  jasmin-lsp 0.1.0 (Jasmin Compiler VERSION, built from ROUTE)
  $ wc -l < out.txt | tr -d ' '
  1
  $ wc -c < err.txt | tr -d ' '
  0
  $ grep -cE '^jasmin-lsp [0-9]+\.[0-9]+\.[0-9]+ \(Jasmin Compiler [0-9]{4}\.[0-9]{2}\.[0-9]+[^,]*, built from (vendored release [0-9]{4}\.[0-9]{2}\.[0-9]+|vendored tarball sha256:[0-9a-f]{12}|installed library)\)$' out.txt
  1
  $ sed -n 's/^let version = "\(.*\)"$/\1/p' ../../../controller/version.ml
  0.1.0
  $ grep -c "^jasmin-lsp $(sed -n 's/^let version = "\(.*\)"$/\1/p' ../../../controller/version.ml) " out.txt
  1
The route is checked against the record the build made of it. That record
holds the release number for a vendored release, the digest prefix for
another vendored tarball (the edge channel), and nothing for the installed
library, and the line must name that route and no other.

  $ route=$(sed -n 's/^let vendored_release = Some "\(.*\)"$/vendored release \1/p' ../../../controller/build_route.ml)
  $ [ -n "$route" ] || route=$(sed -n 's/^let vendored_tarball = Some "\(.*\)"$/vendored tarball sha256:\1/p' ../../../controller/build_route.ml)
  $ route=${route:-installed library}
  $ grep -c ", built from $route)$" out.txt
  1

A vendored release must be the one the lock file pins, so a bump of the lock
that the build did not pick up fails here. Another route names no release to
compare.

  $ case $route in "vendored release "*) grep -c "(Jasmin Compiler $(sed -n 's/^version = //p' ../../../vendor/jasmin.lock)," out.txt ;; *) echo 1 ;; esac
  1

`-v` is the same line: it is the short form of the same option, not a
second one.

  $ ../../../lsp-server/native/main.exe -v > out.txt 2> err.txt; echo "status $?"
  status 0
  $ ../../../lsp-server/native/main.exe --version > long.txt
  $ cmp -s out.txt long.txt && echo "same line"
  same line

The line is written to stdout even where nothing reads it, which is the case
that matters: an earlier build read no option at all, so `--version` entered
the protocol loop, waited on standard input for a client that was not there,
printed `input closed` on stderr and exited 1. Standard input is closed here
for that reason.

  $ ../../../lsp-server/native/main.exe --version < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 0
  $ wc -c < err.txt | tr -d ' '
  0
