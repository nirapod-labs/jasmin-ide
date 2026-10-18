The request pass asks the server only for what it advertises in its initialize
result. The tree at this commit advertises no rename, so the rename row is not
measured: no request is sent for it, the run does not stop, and the other five
requests are measured on the same unit. The fixture declares one function, so
every row has a position to ask at. The entry is named by its absolute path,
which the driver requires of every unit. The case reads no timing: it checks
which rows were measured and what the budget lines say about them. The lock
directory is cleared from the environment, so a build lock elsewhere on the
host cannot decide what the case proves.

  $ env -u JASMIN_BENCH_LOCKS ../../bench/bench.exe request --runs 2 --label capability "$(pwd)/fixtures/unit.jazz" > table.txt; echo "status $?"
  status 0

The rename row says it was not measured and names the capability it lacks.

  $ grep -E '^capability +rename ' table.txt | sed -E 's/ +/ /g'
  capability rename x86-64 0 100.0 - - - not measured: the server does not advertise renameProvider

The budget lines count the rows measured. The snapshot-request budget has its
four rows; the project-wide request budget has only references, and its verdict
is not met in full because rename is missing.

  $ grep -oE '^budget (snapshot|project-wide) requests .*rows measured' table.txt
  budget snapshot requests (completion, signature help, hover, definition, p95 at most 20.0 ms): 4 of 4 rows measured
  budget project-wide requests (references and rename, p95 at most 100.0 ms): 1 of 2 rows measured
  $ grep -cE '^budget project-wide requests .*: not measured in full$' table.txt
  1
  $ grep '^  rename on' table.txt
    rename on 1 of 1 units: not measured: the server does not advertise renameProvider

Nothing was refused, so the run exits 0.

  $ ! grep -q refused table.txt

A met or missed verdict is not final while a build lock is held in the lock
directory the caller names. Here one is held, so the snapshot line gives no met
verdict and names the lock it found. The project-wide line is unchanged.

  $ mkdir locks
  $ mkdir locks/build.lock.1
  $ JASMIN_BENCH_LOCKS=locks ../../bench/bench.exe request --runs 2 --label capability "$(pwd)/fixtures/unit.jazz" > held.txt; echo "status $?"
  status 0
  $ grep -oE '^budget snapshot requests .*: not final.*' held.txt | grep -oE ': not final.*'
  : not final: build.lock.1 is held in the lock directory
  $ ! grep -q ': met$' held.txt
  $ rm -rf locks held.txt table.txt
