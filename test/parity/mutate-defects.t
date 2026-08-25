The list of known compiler defects in `mutate.sh`, over a corpus this test
writes and stand-ins for both tools, so the rules are checked on every tree
without a compiler. The stand-in checker reports a syntax error at 1:1 of
every file; the stand-in compiler does the same, so every mutant agrees,
unless the file holds the same bytes as the file CRASH names, which it then
answers the way a crashing compiler does. The stand-in checker crashes on
that mutant too when COPY is set.

  $ mkdir -p corpus/success/x86-64 bin
  $ printf 'export fn a() {\n  return;\n}\n' > corpus/success/x86-64/a.jazz
  $ printf 'export fn b() {\n  return;\n}\n' > corpus/success/x86-64/b.jazz
  $ cat > bin/jasminc <<'SH'
  > #!/bin/sh
  > [ "$1" = -version ] && { echo "Stand-in Compiler 1"; exit 0; }
  > for file; do :; done
  > case $file in */probe.jazz) exit 0 ;; esac
  > if [ -n "${CRASH:-}" ] && cmp -s "$file" "$CRASH"; then echo 'Fatal error: exception Failure("hd")'; exit 2; fi
  > printf '"%s", line 1 (0-1):\nparse error: unexpected token\n' "$file"
  > exit 1
  > SH
  $ cat > bin/checker <<'SH'
  > #!/bin/sh
  > for file; do :; done
  > if [ -n "${COPY:-}" ] && cmp -s "$file" "$CRASH"; then echo "$file:1:1: internal: crash"; exit 2; fi
  > printf '%s:1:1: parse: unexpected token\n%s: 1 error\n' "$file" "$file"
  > exit 1
  > SH
  $ chmod +x bin/jasminc bin/checker
  $ export JASMINC=$PWD/bin/jasminc JASMIN_LSP=$PWD/bin/checker
  $ export MUTANTS="$(pwd)/../../test/mutation/mutants.exe"
  $ run() { (CRASH=${1:-}; COPY=${2:-}; export CRASH COPY; sh ../../test/parity/mutate.sh corpus > out.txt 2>&1); echo "status $?"; grep -E '^mutate: (agreement|known compiler defect|stale defect|mismatch|every|the mutants|[0-9]+ entr)' out.txt | sed 's/ mutant [0-9]* / mutant N /'; }

The mutant the list names is the one the emitter makes for the seed, the
path, the family and the label.

  $ "$MUTANTS" --output m --seed 0 --files 0 corpus > /dev/null
  $ label=$(awk -F'\t' '$3 == "success/x86-64/b.jazz" && $2 == "delete" { print $4 }' m/manifest.tsv)
  $ listed=$PWD/m/$(awk -F'\t' '$3 == "success/x86-64/b.jazz" && $2 == "delete" { print $1 }' m/manifest.tsv).jazz
  $ other=$PWD/m/$(awk -F'\t' '$3 == "success/x86-64/a.jazz" && $2 == "delete" { print $1 }' m/manifest.tsv).jazz
  $ printf '0\tsuccess/x86-64/b.jazz\tdelete\t%s\t2\t0\t0\tFatal error: exception Failure("hd")\tthe compiler crashes\n' "$label" > defects.tsv
  $ export PARITY_DEFECTS=$PWD/defects.tsv

With every mutant agreeing, the listed one is stale: the compiler does not do
what the list says.

  $ run
  status 1
  mutate: agreement: 5 of 6; difference: 0; known compiler defects: 0 excused, 1 stale
  mutate: stale defect entry: success/x86-64/b.jazz mutant N (delete, delete token 7 `;`): listed as "the compiler crashes", but the compiler now gives status 1 at 1:0: unexpected token
  mutate: 1 entry(ies) of the known compiler defects do not hold

The listed mutant crashes the compiler as listed: it is excused, and the run
passes because every other mutant agrees.

  $ run "$listed"
  status 0
  mutate: agreement: 5 of 6; difference: 0; known compiler defects: 1 excused, 0 stale
  mutate: known compiler defect: success/x86-64/b.jazz mutant N (delete, delete token 7 `;`): the compiler crashes; the compiler gives status 2 at 0:0: Fatal error: exception Failure("hd"), as listed
  mutate: every mutant agrees, except 1 on which the compiler itself is wrong, as listed

A crash on a mutant the list does not name is a difference, as before.

  $ run "$other"
  status 1
  mutate: agreement: 4 of 6; difference: 1; known compiler defects: 0 excused, 1 stale
  mutate: stale defect entry: success/x86-64/b.jazz mutant N (delete, delete token 7 `;`): listed as "the compiler crashes", but the compiler now gives status 1 at 1:0: unexpected token
  mutate: mismatch success/x86-64/a.jazz mutant N (delete, delete token 8 `}`): expected the compiler exits 2, observed the checker exits 1
  mutate: the mutants do not agree

A checker that gives the compiler's own wrong answer fails the run: the
defect is copied.

  $ run "$listed" copy
  status 1
  mutate: agreement: 5 of 6; difference: 0; known compiler defects: 0 excused, 1 stale
  mutate: stale defect entry: success/x86-64/b.jazz mutant N (delete, delete token 7 `;`): the checker gives the compiler's defective answer (status 2 at 0:0: Fatal error: exception Failure("hd")): the compiler crashes
  mutate: 1 entry(ies) of the known compiler defects do not hold

An entry whose mutant a run over the whole corpus does not make is stale.

  $ printf '0\tsuccess/x86-64/b.jazz\tdelete\tdelete token 99 `nothing`\t2\t0\t0\tFatal error: exception Failure("hd")\tthe compiler crashes\n' > defects.tsv
  $ run "$listed"
  status 1
  mutate: agreement: 5 of 6; difference: 1; known compiler defects: 0 excused, 1 stale
  mutate: stale defect entry: success/x86-64/b.jazz (delete, delete token 99 `nothing`): no such mutant in a run over the whole corpus: the compiler crashes
  mutate: mismatch success/x86-64/b.jazz mutant N (delete, delete token 7 `;`): expected the compiler exits 2, observed the checker exits 1
  mutate: the mutants do not agree
