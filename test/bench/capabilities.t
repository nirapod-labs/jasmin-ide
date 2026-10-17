The request pass asks the server only for what it advertises in its initialize
result. The tree at this commit advertises no rename, so the rename row is not
measured: no request is sent for it, the run does not stop, and the other five
requests are measured on the same unit. The fixture declares one function, so
every row has a position to ask at. The entry is named by its absolute path,
which the driver requires of every unit. The case reads no timing: it checks which
rows were measured and what the budget lines say about them.

  $ ../../bench/bench.exe request --runs 2 --label capability "$(pwd)/fixtures/unit.jazz" > table.txt; echo "status $?"
  status 0

The rename row says it was not measured and names the capability it lacks.

  $ grep -E '^capability +rename ' table.txt | sed -E 's/ +/ /g'
  capability rename x86-64 0 100.0 - - - not measured: the server does not advertise renameProvider

The budget lines count the rows measured. B2 has its four rows; B3 has only
references, and its verdict is not met in full because rename is missing.

  $ grep -oE '^budget B[23] \(.*rows measured' table.txt
  budget B2 (snapshot requests, p95 at most 20.0 ms): 4 of 4 rows measured
  budget B3 (project requests, p95 at most 100.0 ms): 1 of 2 rows measured
  $ grep -cE '^budget B3 .*: not measured in full$' table.txt
  1
  $ grep '^  rename on' table.txt
    rename on 1 of 1 units: not measured: the server does not advertise renameProvider

Nothing was refused, so the run exits 0.

  $ ! grep -q refused table.txt
