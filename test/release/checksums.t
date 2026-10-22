The checksums of a release directory: written, read back by the standard
tools, and verified with every way a list can be wrong.

  $ sums() { sh ../../scripts/checksums.sh "$@" 2>&1; echo "exit: $?"; }
  $ mkdir release
  $ printf 'alpha\n' > release/jasmin-lsp-0.0.0-linux-x86_64
  $ printf 'beta\n' > release/jasmin-lsp-0.0.0-macos-aarch64
  $ printf 'stale\n' > release/SHA256SUMS
  $ sums write release
  checksums.sh: wrote release/SHA256SUMS and 2 sidecars
  exit: 0
  $ cat release/SHA256SUMS
  b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  jasmin-lsp-0.0.0-linux-x86_64
  f2c82decdd7181cf98945929a62598db7e6b477e11f6e0eb0ae97020eff151ad  jasmin-lsp-0.0.0-macos-aarch64

The list and each sidecar are in the format `sha256sum -c` reads, so a user
needs no tool of ours to check a download.

  $ check() { (cd release && { sha256sum -c "$1" 2>/dev/null || shasum -a 256 -c "$1"; }); }
  $ check SHA256SUMS
  jasmin-lsp-0.0.0-linux-x86_64: OK
  jasmin-lsp-0.0.0-macos-aarch64: OK
  $ check jasmin-lsp-0.0.0-linux-x86_64.sha256
  jasmin-lsp-0.0.0-linux-x86_64: OK
  $ sums verify release
  ok jasmin-lsp-0.0.0-linux-x86_64
  ok jasmin-lsp-0.0.0-macos-aarch64
  checksums.sh: verified 2 files
  exit: 0

One changed byte, a file the list omits and a listed file that is gone each
fail the verification.

  $ printf 'alphb\n' > release/jasmin-lsp-0.0.0-linux-x86_64
  $ sums verify release | grep -v '^ok'
  FAILED jasmin-lsp-0.0.0-linux-x86_64: wanted b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060, the file is 85f03290ae89e66d38551781c74b1eb56bb14c9230110c48b54aa1e5c60237bb
  checksums.sh: 1 problem(s)
  exit: 1
  $ printf 'alpha\n' > release/jasmin-lsp-0.0.0-linux-x86_64
  $ printf 'gamma\n' > release/notes.txt
  $ sums verify release | grep -v '^ok'
  FAILED notes.txt: in the directory and not in the list
  checksums.sh: 1 problem(s)
  exit: 1
  $ rm release/notes.txt release/jasmin-lsp-0.0.0-macos-aarch64
  $ sums verify release | grep -v '^ok'
  FAILED jasmin-lsp-0.0.0-macos-aarch64: the list names a file the directory does not hold
  checksums.sh: 1 problem(s)
  exit: 1

A line that is not well formed names nothing: an uppercase digest, a path
that leaves the directory, a name given twice.

  $ mkdir bad && printf 'alpha\n' > bad/file
  $ printf 'B6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  file\n' > bad/SHA256SUMS
  $ sums verify bad
  FAILED file: the digest is not 64 lowercase hex digits
  FAILED file: in the directory and not in the list
  checksums.sh: 2 problem(s)
  exit: 1
  $ printf 'b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  ../file\n' > bad/SHA256SUMS
  $ sums verify bad
  FAILED ../file: not a plain file name
  FAILED file: in the directory and not in the list
  checksums.sh: 2 problem(s)
  exit: 1
  $ printf 'b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  file\n' > bad/SHA256SUMS
  $ cat bad/SHA256SUMS bad/SHA256SUMS > bad/twice && mv bad/twice bad/SHA256SUMS
  $ sums verify bad
  ok file
  FAILED file: named twice
  checksums.sh: 1 problem(s)
  exit: 1

A listed file that is gone is a failure, not a skip. The sidecar left beside
it is not the list's business and is not reported.

  $ rm release/notes.txt release/jasmin-lsp-0.0.0-linux-x86_64
  $ sh ../../scripts/release-verify-sums.sh release 2>&1; echo "exit: $?"
  FAILED jasmin-lsp-0.0.0-linux-x86_64: the list names a file the directory does not hold
  release-verify-sums.sh: 1 problems: 1 from the list, 0 file(s) the list omits
  exit: 1

A line whose digest is not 64 lowercase hex digits is refused before any file
is read, and the name on it does not count as listed, so the file it meant is
reported as omitted as well.

  $ mkdir uppercase
  $ printf 'alpha\n' > uppercase/file
  $ printf 'B6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  file\n' > uppercase/SHA256SUMS
  $ sh ../../scripts/release-verify-sums.sh uppercase 2>&1; echo "exit: $?"
  FAILED file: the digest is not 64 lowercase hex digits
  FAILED file: in the directory and not in the list
  release-verify-sums.sh: 2 problems: 1 from the list, 1 file(s) the list omits
  exit: 1

A line that names a path climbs out of the flat directory a release is, so it
is refused however well the file it names would digest.

  $ mkdir traversal
  $ printf 'alpha\n' > traversal/file
  $ printf 'b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  ../file\n' > traversal/SHA256SUMS
  $ sh ../../scripts/release-verify-sums.sh traversal 2>&1; echo "exit: $?"
  FAILED ../file: not a plain file name
  FAILED file: in the directory and not in the list
  release-verify-sums.sh: 2 problems: 1 from the list, 1 file(s) the list omits
  exit: 1

A name on two lines makes the list ambiguous, and is refused.

  $ mkdir twice
  $ printf 'alpha\n' > twice/file
  $ printf 'b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  file\n' > twice/SHA256SUMS
  $ printf 'b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  file\n' >> twice/SHA256SUMS
  $ sh ../../scripts/release-verify-sums.sh twice 2>&1; echo "exit: $?"
  ok file
  FAILED file: named twice
  release-verify-sums.sh: 1 problems: 1 from the list, 0 file(s) the list omits
  exit: 1

A directory with no list at all is a usage error, not a pass: there is nothing
to verify.

  $ mkdir empty
  $ sh ../../scripts/release-verify-sums.sh empty 2>&1; echo "exit: $?"
  release-verify-sums.sh: error: no such list: empty/SHA256SUMS
  exit: 2

The writer refuses a directory it cannot name in one flat list: no files, or a
subdirectory that would fall out of it.

  $ mkdir hollow
  $ sh ../../scripts/release-checksums.sh hollow 2>&1; echo "exit: $?"
  release-checksums.sh: error: hollow holds no release file, so a checksum list would be empty
  exit: 1

  $ mkdir nested
  $ printf 'alpha\n' > nested/file
  $ mkdir nested/sub
  $ sh ../../scripts/release-checksums.sh nested 2>&1; echo "exit: $?"
  release-checksums.sh: error: nested holds a subdirectory (sub), which a flat checksum list cannot name
  exit: 1
  $ ls nested
  file
  sub
