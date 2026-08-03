The runner is asked what a green run proves. It reads a tree of its own here,
with a stand-in for dune in place of dune: the stand-in writes a `cram.out`
record for each test named in `FAKE_EXECUTES` and nothing for the others, so
the runner sees exactly the build a real run would leave.

The tree holds one test that always runs and one that is declared
conditional on `FAKE_CORPUS`, which `test/conditional.tests` names.

  $ cat > fake-dune <<'EOF'
  > #!/bin/sh
  > # A stand-in for dune's own two commands. `clean <path>` takes the cram
  > # records under the path back, as dune does; `runtest` writes a record
  > # for each test named in FAKE_EXECUTES, which is exactly the build a run
  > # that executed those tests leaves behind.
  > set -u
  > case ${1:-} in
  > clean)
  >   shift
  >   for a in "$@"; do
  >     case $a in --root | .) continue ;; esac
  >     [ -d "$a" ] && rm -rf "$a"/.cram.*.t
  >   done
  >   :
  >   ;;
  > runtest)
  >   for t in ${FAKE_EXECUTES:-}; do
  >     d=$(dirname "$t")
  >     b=$(basename "$t")
  >     mkdir -p "_build/default/$d/.cram.$b"
  >     : > "_build/default/$d/.cram.$b/cram.out"
  >   done
  >   ;;
  > *)
  >   printf 'fake-dune: unsupported: %s\n' "$*" >&2
  >   exit 2
  >   ;;
  > esac
  > EOF
  $ chmod +x fake-dune

  $ tree() {
  >   rm -rf root
  >   mkdir -p root/scripts root/test/a root/test/b
  >   cp ../../scripts/runtest.sh root/scripts/runtest.sh
  >   printf 'always runs\n\n  $ true\n' > root/test/a/one.t
  >   printf '(cram)\n' > root/test/a/dune
  >   printf 'needs a corpus\n\n  $ true\n' > root/test/b/two.t
  >   printf '(cram\n (enabled_if (<> %%{env:FAKE_CORPUS=none} "none")))\n' \
  >     > root/test/b/dune
  >   printf 'test/b/two.t FAKE_CORPUS\n' > root/test/conditional.tests
  > }
  $ FAKE_DUNE=$PWD/fake-dune
  $ run() {
  >   ( cd root && env "$@" sh scripts/runtest.sh "$FAKE_DUNE" \
  >       runtest --root . --force -j 4; echo "exit: $?" )
  > }

With the variable unset the conditional test is not meant to run, so the run
is green and says which test it did not run and why. The count is kept
apart, so the green line still says one test executed.

  $ tree
  $ run FAKE_EXECUTES='test/a/one.t'
  runtest: test/b/two.t not executed (FAKE_CORPUS is not set)
  runtest: 1 cram tests executed, 1 not executed
  exit: 0

The word `none` is the other way dune spells "off", and the runner reads it
the same way.

  $ run FAKE_EXECUTES='test/a/one.t' FAKE_CORPUS=none
  runtest: test/b/two.t not executed (FAKE_CORPUS is none)
  runtest: 1 cram tests executed, 1 not executed
  exit: 0

A value that is not the word is "on", and a corpus path with a space in it
is one value, not two words.

  $ run FAKE_EXECUTES='test/a/one.t' FAKE_CORPUS='/a corpus/libjade'
  runtest: test/b/two.t did not execute: no _build/default/test/b/.cram.two.t/cram.out after the run
  runtest: 1 of 2 cram tests did not execute
  exit: 1

An empty value is not the word either: dune substitutes it as itself, so the
stanza is on and the record is demanded.

  $ run FAKE_EXECUTES='test/a/one.t' FAKE_CORPUS=
  runtest: test/b/two.t did not execute: no _build/default/test/b/.cram.two.t/cram.out after the run
  runtest: 1 of 2 cram tests did not execute
  exit: 1

With the variable set the test is meant to run, and a missing record is the
failure the runner exists for: the step cannot be green on a test it did not
execute.

  $ run FAKE_EXECUTES='test/a/one.t' FAKE_CORPUS=/corpus
  runtest: test/b/two.t did not execute: no _build/default/test/b/.cram.two.t/cram.out after the run
  runtest: 1 of 2 cram tests did not execute
  exit: 1

The same run with both records is green, and proves two tests executed.

  $ run FAKE_EXECUTES='test/a/one.t test/b/two.t' FAKE_CORPUS=/corpus
  runtest: 2 cram tests executed
  exit: 0

A record left by the run before does not stand in for the test: the runner
takes the records back before it runs, so the warm tree is the cold one.

  $ run FAKE_EXECUTES='test/b/two.t' FAKE_CORPUS=/corpus
  runtest: test/a/one.t did not execute: no _build/default/test/a/.cram.one.t/cram.out after the run
  runtest: 1 of 2 cram tests did not execute
  exit: 1

The declaration can only excuse a test, so it is checked against the tree.
A `.t` a cram stanza runs under an `enabled_if` and that no line declares is
refused: the runner cannot tell it apart from one that did not run.

  $ tree
  $ printf '(cram\n (enabled_if (<> %%{env:FAKE_CORPUS=none} "none")))\n' > root/test/a/dune
  $ run FAKE_EXECUTES='test/a/one.t'
  runtest: test/a/one.t runs under an enabled_if in test/a/dune and test/conditional.tests does not declare it
  runtest: add the line "test/a/one.t <the variable the enabled_if reads>" to test/conditional.tests
  exit: 1

A line that names a test whose stanza always runs would excuse it on every
run, which is the failure the manifest must not allow either.

  $ tree
  $ printf '(cram)\n' > root/test/b/dune
  $ run FAKE_EXECUTES='test/a/one.t'
  runtest: test/conditional.tests:1: the cram stanza of test/b/dune has no enabled_if, so test/b/two.t always runs: remove the line
  exit: 1

A line that names another variable than the stanza reads would report a skip
for a test that did run, so that is refused too.

  $ tree
  $ printf 'test/b/two.t OTHER_CORPUS\n' > root/test/conditional.tests
  $ run FAKE_EXECUTES='test/a/one.t'
  runtest: test/conditional.tests:1: the cram stanza of test/b/dune does not read OTHER_CORPUS, so the line would excuse test/b/two.t on every run
  exit: 1

A line that names a test which is not in the tree fails the same way, so a
deleted test cannot leave a stale declaration behind.

  $ tree
  $ printf 'test/b/gone.t FAKE_CORPUS\n' > root/test/conditional.tests
  $ run FAKE_EXECUTES='test/a/one.t'
  runtest: test/conditional.tests:1: test/b/gone.t is not a file in the tree
  exit: 1

The second manifest is read by nobody, so it is refused rather than left to
look like a declaration.

  $ tree
  $ printf '(cram)\n' > root/test/a/dune
  $ printf 'test/b/two.t FAKE_CORPUS\n' > root/test/a/conditional.tests
  $ run FAKE_EXECUTES='test/a/one.t'
  runtest: test/conditional.tests is the conditional-test manifest; test/a/conditional.tests is read by nothing
  exit: 1
