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

The writer refuses a directory with nothing in it or with a subdirectory, and
the reader one with no list.

  $ mkdir empty nested nested/sub && printf 'one\n' > nested/one
  $ sums write empty
  checksums.sh: empty holds no release file
  exit: 1
  $ sums write nested
  checksums.sh: nested holds a subdirectory (sub), which a flat list cannot name
  exit: 1
  $ sums verify empty
  checksums.sh: no list: empty/SHA256SUMS
  exit: 2
  $ sums write
  usage: checksums.sh write|verify <directory>
  exit: 2
