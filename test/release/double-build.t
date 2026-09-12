The comparison is bookkeeping around two builds: two clean trees at one
commit, every artefact the release publishes, the digests compared and the
verdict recorded per file. A stand-in for dune writes the file the script
copies, so nothing is compiled and the cases below are the script's own
contract.

A cram directory is reused between runs, so the test starts by clearing what
it builds below.

  $ rm -rf tree-a tree-b out first.txt home run.out

  $ cat > fake-dune <<'EOF'
  > #!/bin/sh
  > # A stand-in for dune: it reads the options this script passes, refuses
  > # nothing, and writes the executable the caller copies out. FAKE_FAIL
  > # makes the build fail; FAKE_EMBED_ROOT writes the tree's own path into
  > # the file, the way a binary that embeds its build path would; FAKE_NOTHING
  > # reports success and writes no file at all.
  > set -eu
  > root=
  > build=
  > while [ $# -gt 0 ]; do
  >   case $1 in
  >   build) shift ;;
  >   --root) root=$2; shift 2 ;;
  >   --build-dir) build=$2; shift 2 ;;
  >   --profile | --workspace | -j) shift 2 ;;
  >   *) shift ;;
  >   esac
  > done
  > [ -n "$build" ] || { echo "fake-dune: no --build-dir" >&2; exit 2; }
  > [ "${FAKE_FAIL:-}" = yes ] && { echo "fake-dune: the build failed" >&2; exit 1; }
  > [ "${FAKE_NOTHING:-}" = yes ] && exit
  > mkdir -p "$build/default/lsp-server/native"
  > if [ "${FAKE_EMBED_ROOT:-}" = yes ]; then
  >   printf 'built in %s\n' "$root"
  > else
  >   printf 'the same bytes both times\n'
  > fi >"$build/default/lsp-server/native/main.exe"
  > EOF
  $ chmod +x fake-dune

  $ mkdir -p home
  $ HOME=$PWD/home; export HOME
  $ GIT_CONFIG_NOSYSTEM=1; export GIT_CONFIG_NOSYSTEM
  $ GIT_AUTHOR_NAME=test; export GIT_AUTHOR_NAME
  $ GIT_AUTHOR_EMAIL=test@example.invalid; export GIT_AUTHOR_EMAIL
  $ GIT_COMMITTER_NAME=test; export GIT_COMMITTER_NAME
  $ GIT_COMMITTER_EMAIL=test@example.invalid; export GIT_COMMITTER_EMAIL

The two trees must be at one commit before the script will compare them, so
the commits below are given one date: two repositories committed a second
apart are two commits, and this test would then fail for a reason that is not
the script's.

  $ GIT_AUTHOR_DATE='2001-09-09T01:46:40 +0000'; export GIT_AUTHOR_DATE
  $ GIT_COMMITTER_DATE='2001-09-09T01:46:40 +0000'; export GIT_COMMITTER_DATE
  $ tree() {
  >   mkdir -p "$1/vendor/jasmin"
  >   git -C "$1" init -q
  >   printf 'a project\n' > "$1/dune-project"
  >   git -C "$1" add -A
  >   git -C "$1" commit -q -m base
  > }
  $ tree tree-a
  $ tree tree-b

The commit and the digests are masked in the transcript: they belong to this
test's own repository and are not part of what the script promises. So is the
compressed size of the source tarball, which no contract here fixes: the host's
zlib decides it, and the digest is what the record is read for. The source
epoch is set, so the recorded value is the one the caller gave.

  $ mask() {
  >   sed -e 's/[0-9a-f]\{64\}/SHA256/g' \
  >     -e 's/[0-9a-f]\{40\}/COMMIT/g' \
  >     -e 's/^platform .*$/platform PLATFORM/' \
  >     -e 's/^\(release-double-build.sh: identical src.tar.gz .*\)([0-9][0-9]* bytes)$/\1(N bytes)/' \
  >     -e '/^artefact src.tar.gz$/,/^match /s/^\([ab]\):bytes [0-9][0-9]*$/\1:bytes N/'
  > }
  $ run() {
  >   DUNE=$PWD/fake-dune SOURCE_DATE_EPOCH=1500000000 \
  >     sh ../../scripts/release-double-build.sh "$@" >run.out 2>&1
  >   status=$?
  >   mask <run.out
  >   echo "exit: $status"
  > }

Both artefacts, by default: the server file and the source tarball of the
commit. Two builds of one commit that produce the same bytes are identical, and
the record says which commit, which trees, and each artefact's digests and
sizes in both passes.

  $ unset JIDE_ARTEFACTS
  $ run tree-a tree-b out
  release-double-build.sh: building 2 artefact(s) in tree-a (pass a) at COMMIT
  release-double-build.sh: building 2 artefact(s) in tree-b (pass b) at COMMIT
  release-double-build.sh: identical server SHA256 (26 bytes)
  release-double-build.sh: identical src.tar.gz SHA256 (N bytes)
  release-double-build.sh: double build: 2 of 2 artefacts identical
  release-double-build.sh: record: out/double-build.txt
  exit: 0
  $ mask <out/double-build.txt
  # Double build of the release artefacts, from two clean trees at one commit
  commit COMMIT
  tree-a tree-a
  tree-b tree-b
  source-date-epoch 1500000000
  platform PLATFORM
  profile release
  jobs 4
  link-flags (none)
  src-prefix jasmin-lsp/
  artefacts 2
  artefact server
  a:sha256 SHA256
  a:bytes 26
  b:sha256 SHA256
  b:bytes 26
  match yes
  artefact src.tar.gz
  a:sha256 SHA256
  a:bytes N
  b:sha256 SHA256
  b:bytes N
  match yes
  verdict identical
  artefacts-compared 2
  artefacts-differing 0
  $ cmp out/pass-a/server out/pass-b/server; echo "exit: $?"
  exit: 0
  $ cmp out/pass-a/src.tar.gz out/pass-b/src.tar.gz; echo "exit: $?"
  exit: 0

The script is deterministic: a second run over the same trees writes the same
record, byte for byte, so a difference between two records is a difference in
what was measured and never in how it was written.

  $ cp out/double-build.txt first.txt
  $ run tree-a tree-b out >/dev/null
  $ cmp first.txt out/double-build.txt; echo "exit: $?"
  exit: 0

Two builds that produce different bytes are a recorded measurement, not a
crash: the verdict says which, and the record names how much differs for that
artefact and whether either tree's path is inside the copies. The tarball does
not carry the path, so it stays identical while the binary does not: the
verdict is per artefact, not one for the run.

  $ rm -rf out
  $ FAKE_EMBED_ROOT=yes; export FAKE_EMBED_ROOT
  $ run tree-a tree-b out
  release-double-build.sh: building 2 artefact(s) in tree-a (pass a) at COMMIT
  release-double-build.sh: building 2 artefact(s) in tree-b (pass b) at COMMIT
  release-double-build.sh: differ server (1 of 16 bytes; first at 15)
  release-double-build.sh: identical src.tar.gz SHA256 (N bytes)
  release-double-build.sh: double build: 1 of 2 artefacts differ
  release-double-build.sh: record: out/double-build.txt
  exit: 0
  $ mask <out/double-build.txt | sed -n '/^artefact server$/,/^artefact src\.tar\.gz$/p' | sed '$d'
  artefact server
  a:sha256 SHA256
  a:bytes 16
  b:sha256 SHA256
  b:bytes 16
  match no
  differing-bytes 1
  first-differing-byte 15
  a:holds-tree-a-path yes
  a:holds-tree-b-path no
  b:holds-tree-a-path no
  b:holds-tree-b-path yes
  $ mask <out/double-build.txt | tail -3
  verdict differ
  artefacts-compared 2
  artefacts-differing 1

A caller that needs the two to match says so, and the run then fails: the
recording still happens, so the evidence for the failure is on disk.

  $ run tree-a tree-b out --require-identical
  release-double-build.sh: building 2 artefact(s) in tree-a (pass a) at COMMIT
  release-double-build.sh: building 2 artefact(s) in tree-b (pass b) at COMMIT
  release-double-build.sh: differ server (1 of 16 bytes; first at 15)
  release-double-build.sh: identical src.tar.gz SHA256 (N bytes)
  release-double-build.sh: double build: 1 of 2 artefacts differ
  release-double-build.sh: record: out/double-build.txt
  release-double-build.sh: error: the two builds of COMMIT are not identical, and --require-identical was given
  exit: 1
  $ unset FAKE_EMBED_ROOT

A caller may name one artefact, and the run then measures only that one and
says so: the count in the record is the check that it measured what it claims.

  $ rm -rf out
  $ JIDE_ARTEFACTS=server; export JIDE_ARTEFACTS
  $ run tree-a tree-b out
  release-double-build.sh: building 1 artefact(s) in tree-a (pass a) at COMMIT
  release-double-build.sh: building 1 artefact(s) in tree-b (pass b) at COMMIT
  release-double-build.sh: identical server SHA256 (26 bytes)
  release-double-build.sh: double build: 1 of 1 artefacts identical
  release-double-build.sh: record: out/double-build.txt
  exit: 0
  $ grep -e '^artefacts ' -e '^artefacts-compared' out/double-build.txt
  artefacts 1
  artefacts-compared 1
  $ ls out/pass-a
  server

An empty list is refused before anything is built: a run that compares no file
would print a verdict while measuring nothing, which is the one result this
script must not report.

  $ JIDE_ARTEFACTS=; export JIDE_ARTEFACTS
  $ run tree-a tree-b out
  release-double-build.sh: error: JIDE_ARTEFACTS names no artefact, so a comparison of none would pass while measuring nothing
  exit: 1

A name the script has no recipe for is a usage error, not a silent skip.

  $ JIDE_ARTEFACTS=server,src.tar.gz; export JIDE_ARTEFACTS
  $ run tree-a tree-b out
  usage: release-double-build.sh <tree-a> <tree-b> <output-directory> [--require-identical]
  exit: 2
  $ unset JIDE_ARTEFACTS

A build that reports success and writes nothing is an error: there is no file
to digest, and the message names the tree.

  $ FAKE_NOTHING=yes; export FAKE_NOTHING
  $ run tree-a tree-b out
  release-double-build.sh: building 2 artefact(s) in tree-a (pass a) at COMMIT
  release-double-build.sh: error: the build in tree-a reported success but wrote no lsp-server/native/main.exe
  exit: 1
  $ unset FAKE_NOTHING

A failed build is an error: there is nothing to compare, and the message says
which tree failed.

  $ FAKE_FAIL=yes; export FAKE_FAIL
  $ run tree-a tree-b out
  release-double-build.sh: building 2 artefact(s) in tree-a (pass a) at COMMIT
  fake-dune: the build failed
  release-double-build.sh: error: the build in tree-a failed
  exit: 1
  $ unset FAKE_FAIL

Two trees at different commits are refused: their builds would measure the
difference between two sources, not a rebuild.

  $ printf 'later\n' >> tree-b/dune-project
  $ git -C tree-b commit -q -am later
  $ run tree-a tree-b out
  release-double-build.sh: error: the trees are at different commits (COMMIT and COMMIT), so a comparison of their builds would measure the difference between two sources
  exit: 1

A tree with a modification is refused for the same reason: the difference
could come from the tree.

  $ printf 'modified\n' >> tree-a/dune-project
  $ run tree-a tree-a out
  release-double-build.sh: error: tree-a is not clean, so a difference could come from the tree and not from the build:  M dune-project
  exit: 1
  $ git -C tree-a checkout -- dune-project

A tree without the vendored library is refused before it builds: its link
would not use the library this repository vendors.

  $ rmdir tree-a/vendor/jasmin
  $ run tree-a tree-a out
  release-double-build.sh: error: tree-a has no vendor/jasmin, so its build would not link the library the repository vendors; assemble it with vendor/fetch.sh first
  exit: 1
