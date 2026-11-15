The recovery mutation harness over a corpus this test writes itself, so the
harness's own verdicts are checked on every tree: a passing run, where every
case whose outline changed has a witness that no parser could have kept it,
and the negative control, where a defect a parser could avoid is planted in
the outline under test and the run must fail.

  $ mkdir corpus
  $ cat > corpus/a.jazz <<'SRC'
  > fn add(reg u64 a, reg u64 b) -> reg u64 {
  >   reg u64 r;
  >   r = a;
  >   r += b;
  >   return r;
  > }
  > 
  > param int N = 4;
  > 
  > u64 g = 7;
  > 
  > export fn main(reg u64 x) -> reg u64 {
  >   reg u64 y;
  >   y = x;
  >   y += N;
  >   return y;
  > }
  > SRC
  $ cat > corpus/b.jazz <<'SRC'
  > namespace M {
  >   fn id(reg u64 a) -> reg u64 {
  >     return a;
  >   }
  > }
  > 
  > export fn use(reg u64 x) -> reg u64 {
  >   reg u64 y;
  >   y = M::id(x);
  >   return y;
  > }
  > SRC
  $ run() { ../../test/mutation/mutation.exe corpus --sample 10 --sites 8 "$@" 2>/dev/null; }

The clean run. One deletion takes out the `}` before an item whose start is
the end of that `}`; the same `}` written elsewhere in the blank text gives
the same mutant and asks for another start, so the case is proven, and the
outline clause holds.

  $ run > clean.txt; echo "status $?"
  status 0
  $ grep -E 'no exception|outline away|the rest proven|unchanged or impossible|at most two' clean.txt
    no exception or hang                   63/63      100.0%  PASS
    outline away from the edit unchanged     62/63       98.4%
      the rest proven impossible            1/1              (a witness each, listed below)
    outline: unchanged or impossible       63/63      100.0%  PASS
    at most two errors (single token)      46/47       97.9%  PASS  (needs 95%)
    family        cases no except. outline same  at most two
  $ grep ': witness: ' clean.txt
    b.jazz: delete token 18 `}`: witness: the mutated text with `}` written at byte 63; delete token 18 of it gives the same text; here fn use 64..146, there fn use 63..146

The negative control moves every item after the edit by one byte. The
prover reads only the original and the mutation, so it cannot excuse what
this does to the cases that have no witness, and the run fails.

  $ run --plant-defect > planted.txt; echo "status $?"
  status 1
  $ grep -E 'outline away|the rest proven|unchanged or impossible' planted.txt
    outline away from the edit unchanged     37/63       58.7%
      the rest proven impossible            1/26             (a witness each, listed below)
    outline: unchanged or impossible       38/63       60.3%  FAIL
