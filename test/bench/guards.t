A number is printed only inside a timing window, so a run that cannot name
one is refused before it reads anything else.

  $ env JASMIN_BENCH_WINDOW= JASMIN_BENCH_NO_WINDOW= sh ../../bench/run.sh
  bench: no timing window: set JASMIN_BENCH_WINDOW to a path that exists only inside one, or JASMIN_BENCH_NO_WINDOW=1 to measure anyway
  [2]

A window that was named and is not open is the same refusal.

  $ env JASMIN_BENCH_WINDOW=closed.lock sh ../../bench/run.sh
  bench: no timing window: closed.lock does not exist
  [2]

The slots beside an open window are checked without the caller naming their
directory: a held slot is a build, and a build makes every number meaningless.

  $ mkdir window
  $ : > window/measure.lock
  $ mkdir window/build.lock.1
  $ env JASMIN_BENCH_WINDOW=window/measure.lock sh ../../bench/run.sh
  bench: a build is running (window/build.lock.1 is held)
  [2]

A caller who names another directory gets that one.

  $ mkdir slots
  $ mkdir slots/build.lock.1
  $ env JASMIN_BENCH_WINDOW=window/measure.lock JASMIN_BENCH_LOCKS=slots sh ../../bench/run.sh
  bench: a build is running (slots/build.lock.1 is held)
  [2]

The explicit opt-out takes a run past the window check, and says so, but a
build beside it is still refused.

  $ env JASMIN_BENCH_WINDOW= JASMIN_BENCH_NO_WINDOW=1 JASMIN_BENCH_LOCKS=slots sh ../../bench/run.sh
  bench: measuring outside a timing window (JASMIN_BENCH_NO_WINDOW is set)
  bench: a build is running (slots/build.lock.1 is held)
  [2]
