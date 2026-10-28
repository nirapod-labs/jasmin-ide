The request pass asks the server only for what it advertises in its initialize
result. The tree advertises renameProvider, so the rename row is measured like
the other five: the request goes to the engine's rename and is answered with
its edits, and the budget lines count six measured rows. The fixture declares
one function, so every row has a position to ask at. The entry is named by its
absolute path, which the driver requires of every unit. The case reads no
timing: it checks which rows were measured and what the budget lines say about
them. The lock directory is cleared from the environment, so a build lock
elsewhere on the host cannot decide what the case proves.

  $ env -u JASMIN_BENCH_LOCKS ../../bench/bench.exe request --runs 2 --label capability "$(pwd)/fixtures/unit.jazz" > table.txt; echo "status $?"
  status 0

The rename row is measured against the same 100 ms budget as references.

  $ grep -cE '^capability +rename +x86-64 +2 +100\.0 .* met$' table.txt
  1

The budget lines count the rows measured. The snapshot-request budget has its
four rows, and the project-wide budget has references and rename, both measured
and both met.

  $ grep -oE '^budget (snapshot|project-wide) requests .*rows measured' table.txt
  budget snapshot requests (completion, signature help, hover, definition, p95 at most 20.0 ms): 4 of 4 rows measured
  budget project-wide requests (references and rename, p95 at most 100.0 ms): 2 of 2 rows measured

Nothing was refused and nothing was left unmeasured, so the run exits 0.

  $ ! grep -q refused table.txt
  $ ! grep -q 'not measured:' table.txt

A met or missed verdict is not final while a build lock is held in the lock
directory the caller names. Here one is held, so both budget lines give no met
verdict and name the lock they found.

  $ mkdir locks
  $ mkdir locks/build.lock.1
  $ JASMIN_BENCH_LOCKS=locks ../../bench/bench.exe request --runs 2 --label capability "$(pwd)/fixtures/unit.jazz" > held.txt; echo "status $?"
  status 0
  $ grep -oE '^budget snapshot requests .*: not final.*' held.txt | grep -oE ': not final.*'
  : not final: build.lock.1 is held in the lock directory
  $ grep -c ': not final: build.lock.1 is held in the lock directory' held.txt
  2
  $ ! grep -q ': met$' held.txt
  $ rm -rf locks held.txt table.txt
