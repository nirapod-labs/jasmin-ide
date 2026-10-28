check-linked.sh answers with one of three statuses: 0 when every dependency a
binary names is allowed, 1 when one is not, and 2 when the check could not be
made at all. The dependency listing comes from the platform's own tool; here
that tool is a stand-in the test writes, so the test runs on any host, needs no
C compiler and never asks for a binary of the other platform. What it tests is
the decision: which names are allowed, what is printed, and which status
follows.

Both `otool` and `ldd` are read from PATH, so the stand-ins go first. Each one
reads the answer the test wrote beside the binary, which keeps the cases below
readable.

  $ mkdir bin
  $ cat > bin/otool <<'EOF'
  > #!/bin/sh
  > # `otool -L <binary>`: the header line the real tool prints, then the
  > # dependency list the test wrote in <binary>.deps.
  > printf '%s (architecture arm64):\n' "$2"
  > cat "$2.deps"
  > EOF
  $ cat > bin/ldd <<'EOF'
  > #!/bin/sh
  > # `ldd <binary>`: the answer the test wrote in <binary>.deps, unless it
  > # named a case in <binary>.ldd for the two answers that are not a list.
  > if [ -f "$1.ldd" ]; then
  >   case $(cat "$1.ldd") in
  >   static) printf '%s: not a dynamic executable\n' "$1" >&2; exit 1 ;;
  >   broken) printf 'ldd: %s: cannot read file data\n' "$1" >&2; exit 1 ;;
  >   esac
  > fi
  > cat "$1.deps"
  > EOF
  $ chmod +x bin/otool bin/ldd
  $ export PATH=$PWD/bin:$PATH

A Mach-O whose only dependency is libSystem passes, and the size is printed on
the passing path as it is on the failing one.

  $ printf '\317\372\355\376' > ok
  $ printf '\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1359.0.0)\n' > ok.deps
  $ sh ../../scripts/check-linked.sh ok macos
  check-linked.sh: ok
  check-linked.sh: size 4 bytes (0.0 MiB)
  check-linked.sh:   allowed: /usr/lib/libSystem.B.dylib
  check-linked.sh: ok: all 1 dependencies are allowed on macos

One more library is the failure the script exists for: it is named, the count
says how many of how many, and the status is 1.

  $ printf '\317\372\355\376' > extra
  $ printf '\t/usr/lib/libz.1.dylib (compatibility version 1.0.0, current version 1.2.11)\n\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1359.0.0)\n' > extra.deps
  $ sh ../../scripts/check-linked.sh extra macos
  check-linked.sh: extra
  check-linked.sh: size 4 bytes (0.0 MiB)
  check-linked.sh:   not allowed: /usr/lib/libz.1.dylib
  check-linked.sh:   allowed: /usr/lib/libSystem.B.dylib
  check-linked.sh: fail: 1 of 2 dependencies are not allowed on macos
  [1]

On Linux a static binary has nothing to check left, and `ldd` says so with a
non-zero status of its own. That status is not a failure: the message is the
answer.

  $ printf '\177ELF' > static
  $ printf 'static\n' > static.ldd
  $ sh ../../scripts/check-linked.sh static linux
  check-linked.sh: static
  check-linked.sh: size 4 bytes (0.0 MiB)
  check-linked.sh: static: the binary names no dynamic dependency
  check-linked.sh: ok

The musl names a release binary may carry are allowed, and a duplicate line is
counted, not deduplicated: the count is of what the tool printed.

  $ printf '\177ELF' > musl
  $ printf '\tlibc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7f2a)\n\t/lib/ld-musl-x86_64.so.1 (0x7f2a)\n' > musl.deps
  $ sh ../../scripts/check-linked.sh musl linux
  check-linked.sh: musl
  check-linked.sh: size 4 bytes (0.0 MiB)
  check-linked.sh:   allowed: ld-musl-x86_64.so.1
  check-linked.sh:   allowed: ld-musl-x86_64.so.1
  check-linked.sh: ok: all 2 dependencies are allowed on linux

A glibc loader is a failure whatever else is listed, and the reason is printed
with it.

  $ printf '\177ELF' > glibc
  $ printf '\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x7f1b)\n\t/lib64/ld-linux-x86-64.so.2 (0x7f1c)\n' > glibc.deps
  $ sh ../../scripts/check-linked.sh glibc linux
  check-linked.sh: glibc
  check-linked.sh: size 4 bytes (0.0 MiB)
  check-linked.sh:   allowed: libc.so.6
  check-linked.sh:   not allowed: ld-linux-x86-64.so.2
  check-linked.sh:   this is a glibc dynamic build; the release binary is static against musl
  check-linked.sh: fail: 1 of 2 dependencies are not allowed on linux
  [1]

An `ldd` that fails is not a dependency. A file it will not read is "the check
could not be made", so its own message is the reason and the status is 2:
before this, the message was printed as a library named `ldd:` and the run
said "not allowed", which is a finding about a binary that has none.

  $ printf '\177ELF' > broken
  $ printf 'broken\n' > broken.ldd
  $ sh ../../scripts/check-linked.sh broken linux
  check-linked.sh: broken
  check-linked.sh: size 4 bytes (0.0 MiB)
  ldd: broken: cannot read file data
  check-linked.sh: error: ldd failed on broken
  [2]

A listing with no line in it measured nothing, and that is a failure rather
than a pass with nothing to check.

  $ printf '\317\372\355\376' > empty
  $ : > empty.deps
  $ sh ../../scripts/check-linked.sh empty macos
  check-linked.sh: empty
  check-linked.sh: size 4 bytes (0.0 MiB)
  check-linked.sh: the listing is empty, which is not a static binary and not a linked one
  check-linked.sh: fail: nothing was measured
  [1]

A file of the other platform's kind is refused before any tool is asked, and
the four bytes it read are printed so the refusal can be checked.

  $ printf 'not a program\n' > text
  $ sh ../../scripts/check-linked.sh text macos
  check-linked.sh: text
  check-linked.sh: size 14 bytes (0.0 MiB)
  check-linked.sh: error: not a macos executable: the first four bytes are 6e6f7420
  [2]

The refusals that are not a measurement are all 2: no such file, an unknown
platform name, and no argument.

  $ sh ../../scripts/check-linked.sh nope macos
  check-linked.sh: error: no such file: nope
  [2]

  $ sh ../../scripts/check-linked.sh ok windows
  check-linked.sh: error: unknown platform: windows (macos or linux)
  [2]

  $ sh ../../scripts/check-linked.sh
  usage: check-linked.sh <binary> [macos|linux]
  [2]
