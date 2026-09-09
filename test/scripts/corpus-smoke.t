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

