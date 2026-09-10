The smoke script must tell a binary that answered every file from one that did
not, and it must fail on each way of not answering: a crash, an exit status no
check returns, an internal diagnostic, or no verdict line at all. It runs here
against stand-ins for the server, so the test needs no binary and no corpus.

A stand-in reads the first line of each file it is given: `error` is a file it
rejects, anything else is a file it accepts.

  $ cat > checker <<'EOF'
  > #!/bin/bash
  > case ${1:-} in
  > --version) echo 'jasmin-lsp 0.0.0 (stand-in for the checker)'; exit ;;
  > esac
  > [ "${1:-}" = check ] || { echo 'not a check' >&2; exit 2; }
  > shift
  > while [ $# -gt 0 ] && [ "$1" != -- ]; do shift; done
  > [ "${1:-}" = -- ] && shift
  > status=0
  > for f in "$@"; do
  >   if [ "$(sed -n 1p "$f")" = error ]; then
  >     echo "$f:1:1: typing: this file is refused"
  >     echo "$f: 1 error"
  >     status=1
  >   else
  >     echo "$f: ok"
  >   fi
  > done
  > exit $status
  > EOF
  $ chmod +x checker
  $ mkdir -p corpus/tests/success corpus/tests/fail
  $ printf 'export fn a() {}\n' > corpus/tests/success/a.jazz
  $ printf 'export fn b() {}\n' > corpus/tests/success/b.jinc
  $ printf 'error\n' > corpus/tests/fail/c.jazz

A binary that answers every file passes, prints a tally per group, and writes
the table it measured when it is asked for one.

  $ ../../scripts/corpus-smoke.sh ./checker corpus -j 1 --write table.tsv
  corpus-smoke.sh: binary ./checker
  corpus-smoke.sh: version jasmin-lsp 0.0.0 (stand-in for the checker)
  corpus-smoke.sh: corpus $TESTCASE_ROOT/corpus: 3 files, 1 at a time
    group         files     ok errors
    fail              1      0      1
    success           2      2      0
  corpus-smoke.sh: wrote the verdict table to table.tsv
  corpus-smoke.sh: 3 files: 2 ok, 1 with errors, 0 failed
  corpus-smoke.sh: ok: every file got a verdict, no internal diagnostic, no crash
  $ cat -t table.tsv
  tests/fail/c.jazz^Ierrors=1
  tests/success/a.jazz^Iok
  tests/success/b.jinc^Iok

A second platform's run is compared with the first's table line for line, and
the same verdicts pass.

  $ ../../scripts/corpus-smoke.sh ./checker corpus --expect table.tsv -j 1 | tail -3
  corpus-smoke.sh: the verdicts match table.tsv line for line
  corpus-smoke.sh: 3 files: 2 ok, 1 with errors, 0 failed
  corpus-smoke.sh: ok: every file got a verdict, no internal diagnostic, no crash

A different verdict on any one file is a failure, and the difference is printed
with the file's name.

  $ printf 'tests/fail/c.jazz\terrors=1\ntests/success/a.jazz\terrors=2\ntests/success/b.jinc\tok\n' > other.tsv
  $ ../../scripts/corpus-smoke.sh ./checker corpus --expect other.tsv -j 1 > /dev/null 2> err.txt
  [1]
  $ sed -n '1p' err.txt
  corpus-smoke.sh: error: the verdicts differ from other.tsv:
  $ sed -n 's/^[-+]//p' err.txt | grep a.jazz | cat -t
  tests/success/a.jazz^Ierrors=2
  tests/success/a.jazz^Iok

A binary that dies on a file is a failure, whatever it printed before it.

  $ cat > crash <<'EOF'
  > #!/bin/bash
  > [ "${1:-}" = --version ] && { echo 'jasmin-lsp 0.0.0'; exit; }
  > shift
  > while [ $# -gt 0 ] && [ "$1" != -- ]; do shift; done
  > [ "${1:-}" = -- ] && shift
  > for f in "$@"; do
  >   case $f in
  >   *c.jazz*) exit 3 ;;
  >   esac
  >   echo "$f: ok"
  > done
  > EOF
  $ chmod +x crash
  $ ../../scripts/corpus-smoke.sh ./crash corpus -j 1 > /dev/null
  corpus-smoke.sh: error: tests/fail/c.jazz exited 3, which no check does
  [1]

An internal diagnostic is a failure even when the summary line looks right: it
is what the checker prints when it could not do the check at all.

  $ cat > internal <<'EOF'
  > #!/bin/bash
  > [ "${1:-}" = --version ] && { echo 'jasmin-lsp 0.0.0'; exit; }
  > shift
  > while [ $# -gt 0 ] && [ "$1" != -- ]; do shift; done
  > [ "${1:-}" = -- ] && shift
  > for f in "$@"; do
  >   echo "$f:1:1: internal: cannot read the file"
  >   echo "$f: 1 error"
  > done
  > exit 1
  > EOF
  $ chmod +x internal
  $ ../../scripts/corpus-smoke.sh ./internal corpus -j 1 > /dev/null
  corpus-smoke.sh: error: tests/fail/c.jazz reported an internal diagnostic:
    tests/fail/c.jazz:1:1: internal: cannot read the file
  corpus-smoke.sh: error: tests/success/a.jazz reported an internal diagnostic:
    tests/success/a.jazz:1:1: internal: cannot read the file
  corpus-smoke.sh: error: tests/success/b.jinc reported an internal diagnostic:
    tests/success/b.jinc:1:1: internal: cannot read the file
  [1]

A binary that runs but answers nothing is a failure too: silence must not read
as a clean corpus.

  $ cat > silent <<'EOF'
  > #!/bin/bash
  > [ "${1:-}" = --version ] && { echo 'jasmin-lsp 0.0.0'; exit; }
  > exit
  > EOF
  $ chmod +x silent
  $ ../../scripts/corpus-smoke.sh ./silent corpus -j 1 > /dev/null
  corpus-smoke.sh: error: tests/fail/c.jazz printed no verdict; the last line was:
  corpus-smoke.sh: error: tests/success/a.jazz printed no verdict; the last line was:
  corpus-smoke.sh: error: tests/success/b.jinc printed no verdict; the last line was:
  [1]

A binary that does not run here fails before the corpus starts, so a binary of
the wrong architecture cannot report a corpus it never read.

  $ cat > wrong-platform <<'EOF'
  > #!/bin/bash
  > echo 'cannot execute: wrong architecture' >&2
  > exit 126
  > EOF
  $ chmod +x wrong-platform
  $ ../../scripts/corpus-smoke.sh ./wrong-platform corpus
  corpus-smoke.sh: binary ./wrong-platform
  corpus-smoke.sh: error: $TESTCASE_ROOT/wrong-platform --version failed, so the binary does not run here:
  cannot execute: wrong architecture
  [1]

The usage errors are refused with status 2: a missing argument, a job count
below one, a binary that is not there, a corpus with no source in it.

  $ ../../scripts/corpus-smoke.sh ./checker
  usage: corpus-smoke.sh <binary> <corpus-root> [-j N] [--arch NAME] [--write FILE] [--expect FILE]
  [2]
  $ ../../scripts/corpus-smoke.sh ./checker corpus -j 0
  corpus-smoke.sh: error: -j is below one: 0
  [2]
  $ ../../scripts/corpus-smoke.sh ./absent corpus
  corpus-smoke.sh: error: no such file: ./absent
  [2]
  $ mkdir -p empty
  $ ../../scripts/corpus-smoke.sh ./checker empty
  corpus-smoke.sh: binary ./checker
  corpus-smoke.sh: version jasmin-lsp 0.0.0 (stand-in for the checker)
  corpus-smoke.sh: error: no .jazz or .jinc file under $TESTCASE_ROOT/empty, so there is nothing to check
  [2]
