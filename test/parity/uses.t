The resolver half of the parity gate, `uses.sh`, over a corpus this test
writes itself, so the script's verdicts are checked on every tree. The
comparison is the built `typed_uses.exe`. The corpus has two entries: the
first requires a library, and the second requires a library of its own and
then the first entry, so the uses of the first library are reached from both
checks.

  $ export TYPED_USES="$(pwd)/../../test/parity/typed_uses.exe"
  $ uses() { sh ../../test/parity/uses.sh "$@"; }
  $ mkdir -p good/success/x86-64
  $ cat > good/success/x86-64/lib.jinc <<'SRC'
  > param int K = 2;
  > fn twice(reg u64 a) -> reg u64 {
  >   reg u64 t;
  >   t = a;
  >   t += a;
  >   return t;
  > }
  > SRC
  $ cat > good/success/x86-64/a.jazz <<'SRC'
  > require "lib.jinc"
  > export fn main(reg u64 p) -> reg u64 {
  >   reg u64 r;
  >   r = twice(p);
  >   r += K;
  >   return r;
  > }
  > SRC
  $ cat > good/success/x86-64/own.jinc <<'SRC'
  > fn copy(reg u64 c) -> reg u64 {
  >   reg u64 d;
  >   d = c;
  >   return d;
  > }
  > SRC
  $ cat > good/success/x86-64/b.jazz <<'SRC'
  > require "own.jinc"
  > require "a.jazz"
  > export fn other(reg u64 q) -> reg u64 {
  >   reg u64 s;
  >   s = main(q);
  >   s = copy(s);
  >   return s;
  > }
  > SRC

Every typed use resolves to the compiler's declaration, the uses of the
library are counted once although both checks reach them, and the run passes.

  $ uses good; echo "status $?"
  uses: 2 files, 2 checks; the check accepts 2, refuses 0
  uses: distinct typed variable uses per architecture: 20; measured: 20; the compiler's declaration in its namespace: 20; elsewhere: 0; unresolved: 0; other-name: 0; misplaced: 0; excluded: 0 (flag 0, temporary 0); unchecked: 0
  uses: uses of files a top-level require reaches: 15 (in 3 distinct files; 2 of them are not checked directly, with 9 uses)
  uses: the gate's measure: 20 of 20 measured uses are the compiler's declaration in their own namespace (100.0000%)
  uses: by folder: typed, measured, same, elsewhere, unresolved, other-name, misplaced, excluded, unchecked
  uses:   x86-64: 20 typed, 20 measured, 20 same, 0 elsewhere, 0 unresolved, 0 other-name, 0 misplaced, 0 excluded, 0 unchecked
  uses: every measured typed use of every accepted file is the compiler's declaration in its own namespace
  status 0

The negative control plants a wrong binding in the first measured use of each
check, which is in the library each check requires first. The second check
reaches the first library unplanted, so the two checks judge its planted use
differently: the conflict is counted rather than stopping the run, every
planted binding is reported, and the run fails, as it must.

  $ (USES_NEGATIVE_CONTROL=1; export USES_NEGATIVE_CONTROL; uses good); echo "status $?"
  uses: 2 files, 2 checks; the check accepts 2, refuses 0
  uses: distinct typed variable uses per architecture: 20; measured: 20; the compiler's declaration in its namespace: 18; elsewhere: 2; unresolved: 0; other-name: 0; misplaced: 0; excluded: 0 (flag 0, temporary 0); unchecked: 0
  uses: uses of files a top-level require reaches: 15 (in 3 distinct files; 2 of them are not checked directly, with 9 uses)
  uses: negative control: 1 use(s) are judged differently by two checks of one architecture, since a planted use of a file is reached unplanted from another check
  uses: the gate's measure: 18 of 20 measured uses are the compiler's declaration in their own namespace (90.0000%)
  uses: by folder: typed, measured, same, elsewhere, unresolved, other-name, misplaced, excluded, unchecked
  uses:   x86-64: 20 typed, 20 measured, 18 same, 2 elsewhere, 0 unresolved, 0 other-name, 0 misplaced, 0 excluded, 0 unchecked
  uses: negative control: a wrong binding was planted in each of the 2 accepted checks that have a measured use, and the comparison reported it in 2; the run fails, as it must
  status 1

A file the check refuses has no compiler-side answer, so it fails the run.

  $ mkdir -p bad/success/x86-64
  $ cp good/success/x86-64/lib.jinc good/success/x86-64/a.jazz bad/success/x86-64/
  $ cat > bad/success/x86-64/broken.jazz <<'SRC'
  > export fn f() -> reg u64 {
  >   y = 1;
  >   return y;
  > }
  > SRC
  $ uses bad; echo "status $?"
  uses: 2 files, 2 checks; the check accepts 1, refuses 1
  uses: distinct typed variable uses per architecture: 12; measured: 12; the compiler's declaration in its namespace: 12; elsewhere: 0; unresolved: 0; other-name: 0; misplaced: 0; excluded: 0 (flag 0, temporary 0); unchecked: 0
  uses: uses of files a top-level require reaches: 6 (in 1 distinct files; 1 of them are not checked directly, with 6 uses)
  uses: the gate's measure: 12 of 12 measured uses are the compiler's declaration in their own namespace (100.0000%)
  uses: by folder: typed, measured, same, elsewhere, unresolved, other-name, misplaced, excluded, unchecked
  uses:   x86-64: 12 typed, 12 measured, 12 same, 0 elsewhere, 0 unresolved, 0 other-name, 0 misplaced, 0 excluded, 0 unchecked
  uses: different success/x86-64/broken.jazz (x86-64): the check does not accept it: 2:3 unknown variable: `y'
  uses: the corpus does not agree
  status 1

Without a corpus the script cannot run at all.

  $ uses missing; echo "status $?"
  uses: no corpus directory: missing
  status 2
