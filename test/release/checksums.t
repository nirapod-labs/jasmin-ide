The list covers every release file and not the sidecars beside them.

A cram directory is reused between runs, so the test starts by clearing what
it builds below.

  $ rm -rf release uppercase traversal twice empty hollow nested

  $ mkdir release
  $ printf 'alpha\n' > release/jasmin-lsp-0.0.0-linux-x86_64
  $ printf 'beta\n' > release/jasmin-lsp-0.0.0-linux-x86_64.sha256
  $ sh ../../scripts/release-checksums.sh release
  release-checksums.sh: wrote release/SHA256SUMS: 1 files
    jasmin-lsp-0.0.0-linux-x86_64
  $ cat release/SHA256SUMS
  b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060  jasmin-lsp-0.0.0-linux-x86_64

The verifier passes that list, and says how many files it checked.

  $ sh ../../scripts/release-verify-sums.sh release 2>&1; echo "exit: $?"
  ok jasmin-lsp-0.0.0-linux-x86_64
  release-verify-sums.sh: verified 1 files
  exit: 0

One changed byte is a failure, and the line names the digest the list holds
and the one the file now has.

  $ printf 'alphb\n' > release/jasmin-lsp-0.0.0-linux-x86_64
  $ sh ../../scripts/release-verify-sums.sh release 2>&1; echo "exit: $?"
  FAILED jasmin-lsp-0.0.0-linux-x86_64: wanted b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060, the file is 85f03290ae89e66d38551781c74b1eb56bb14c9230110c48b54aa1e5c60237bb
  release-verify-sums.sh: 1 problems: 1 from the list, 0 file(s) the list omits
  exit: 1

The same list is green again once the bytes are back, so the red above was the
changed byte and not the list.

  $ printf 'alpha\n' > release/jasmin-lsp-0.0.0-linux-x86_64
  $ sh ../../scripts/release-verify-sums.sh release >/dev/null; echo "exit: $?"
  exit: 0

A file in the directory that the list does not name is a failure too: the list
would be incomplete while every line on it is right.

  $ printf 'gamma\n' > release/notes.txt
  $ sh ../../scripts/release-verify-sums.sh release 2>&1; echo "exit: $?"
  ok jasmin-lsp-0.0.0-linux-x86_64
  FAILED notes.txt: in the directory and not in the list
  release-verify-sums.sh: 1 problems: 0 from the list, 1 file(s) the list omits
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
