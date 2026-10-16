A number is printed only inside a timing window, so a run that cannot name
one is refused before it reads anything else.

Every case below hands the driver a bare environment — `env -i` and PATH —
and then names what it wants set. The driver reads JASMIN_BENCH_WINDOW,
NO_WINDOW, LOCKS, EXE, CORPUS, UNITS, RUNS and COUNT, so every one of those
that a case does not name is unset, and so is every other name the caller
happened to export: a corpus, a slots directory or a binary left over in the
shell would otherwise decide what a case proves.

  $ env -i PATH="$PATH" JASMIN_BENCH_WINDOW= JASMIN_BENCH_NO_WINDOW= sh ../../bench/run.sh
  bench: no timing window: open one and set JASMIN_BENCH_WINDOW to the path it creates, or set JASMIN_BENCH_NO_WINDOW=1 to measure anyway (not window evidence); docs/bench.md
  [2]

A window that was named and is not open is the same refusal.

  $ env -i PATH="$PATH" JASMIN_BENCH_WINDOW=closed.lock sh ../../bench/run.sh
  bench: no timing window: closed.lock does not exist (open one first, or set JASMIN_BENCH_NO_WINDOW=1)
  [2]

The slots beside an open window are checked without the caller naming their
directory: a held slot is a build, and a build makes every number meaningless.

  $ mkdir window
  $ : > window/measure.lock
  $ mkdir window/build.lock.1
  $ env -i PATH="$PATH" JASMIN_BENCH_WINDOW=window/measure.lock sh ../../bench/run.sh
  bench: a build is running (window/build.lock.1 is held)
  [2]

A caller who names another directory gets that one.

  $ mkdir slots
  $ mkdir slots/build.lock.1
  $ env -i PATH="$PATH" JASMIN_BENCH_WINDOW=window/measure.lock JASMIN_BENCH_LOCKS=slots sh ../../bench/run.sh
  bench: a build is running (slots/build.lock.1 is held)
  [2]

The explicit opt-out takes a run past the window check, and says so, but a
build beside it is still refused.

  $ env -i PATH="$PATH" JASMIN_BENCH_WINDOW= JASMIN_BENCH_NO_WINDOW=1 JASMIN_BENCH_LOCKS=slots sh ../../bench/run.sh
  bench: measuring outside a timing window (JASMIN_BENCH_NO_WINDOW is set); the numbers are not window evidence
  bench: a build is running (slots/build.lock.1 is held)
  [2]

The opt-out reaches the rest of the checks rather than stopping at the window
one: with no build and a binary in place, it gets past the window and the
slots and stops at the corpus, which is the next thing it needs. A window
opened in its place stops in the same place, so what the opt-out buys is the
window check and nothing else.

The build check has two halves, and the cases below answer both with things
they own. The slot half reads a directory: `locks` is one this test makes and
keeps empty, so a slot held by a build in another lane cannot answer for it.
The process half reads the host's whole process table, which no directory can
pin, so it is answered by a `ps` of the test's own that reports no build.
Neither half then depends on what the machine is doing, and no case measures
anything: each stops at the corpus check before it.

  $ rm -rf slots window/build.lock.1
  $ mkdir locks
  $ mkdir fakes
  $ printf '#!/bin/sh\n' > fakes/bench.exe
  $ cat > fakes/ps <<'EOF'
  > #!/bin/sh
  > # The driver's build probe reads the host's whole process table. Answer it
  > # with "no build": these cases prove the driver's order, not the machine's
  > # load. The slot half is answered by the empty `locks` directory beside it.
  > :
  > EOF
  $ chmod +x fakes/bench.exe fakes/ps
  $ env -i PATH="$PWD/fakes:$PATH" JASMIN_BENCH_WINDOW= JASMIN_BENCH_NO_WINDOW=1 JASMIN_BENCH_LOCKS=locks JASMIN_BENCH_EXE=fakes/bench.exe sh ../../bench/run.sh
  bench: measuring outside a timing window (JASMIN_BENCH_NO_WINDOW is set); the numbers are not window evidence
  bench: no corpus: set JASMIN_BENCH_CORPUS to a Jasmin compiler tests directory
  [2]

  $ env -i PATH="$PWD/fakes:$PATH" JASMIN_BENCH_WINDOW=window/measure.lock JASMIN_BENCH_LOCKS=locks JASMIN_BENCH_EXE=fakes/bench.exe sh ../../bench/run.sh
  bench: no corpus: set JASMIN_BENCH_CORPUS to a Jasmin compiler tests directory
  [2]

A corpus that is named but is not a directory is refused too, so the run
never reaches the measurement with nothing to measure.

  $ env -i PATH="$PWD/fakes:$PATH" JASMIN_BENCH_WINDOW=window/measure.lock JASMIN_BENCH_LOCKS=locks JASMIN_BENCH_CORPUS=no-such-directory JASMIN_BENCH_EXE=fakes/bench.exe sh ../../bench/run.sh
  bench: not a directory: no-such-directory
  [2]

The directories the cases made are theirs to remove.

  $ rm -rf locks fakes window slots
