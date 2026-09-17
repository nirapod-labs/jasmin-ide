(** The completion measurement: the names completion offers
    against the resolver's own scope, at deterministically sampled bytes of a
    corpus.

    The requirement is that the names in scope at sampled positions equal the
    resolver's scope. The two sides of that equality are read here from the
    two places the feature reads them, and never from a second derivation:
    the offered names are {!Jide_engine.Complete.completion}'s items minus
    the lexer's keywords, and the scope is {!Jide_engine.Resolve.scope_at} at
    the same byte of the same file. Completion's own prefix is applied to the
    scope's names, because that is the answer a client sees: at a byte in the
    middle of a word the resolver still holds every name, and completion
    shows the ones the word being typed can still become.

    A position where the byte follows a [#] is not a name position: completion
    answers that trigger with the annotation keys, and those are not names in
    scope. The run counts those apart instead of asking the question there.
    A position outside every function the file declares is not asked either,
    because the requirement covers positions inside expressions; it is counted
    apart too. A position the resolver never reached has no scope to compare
    with: it is counted apart and fails the run, because both sides would then
    be empty and agree by default.

    The sample is fixed by three numbers, all printed by the run: [step] is
    the distance between sampled bytes, [seed] is a constant folded into the
    file's own hash to move the first sampled byte inside one step, and
    [limit] bounds the whole run. Nothing here draws a random number, so the
    same corpus, step, seed and limit name the same bytes on every host.

    Usage:

    {v
      completion_scope --corpus <dir> [--corpus <dir>]...
                       [--step <n>] [--seed <n>] [--limit <n>] [--quiet]
      completion_scope --selftest
    v}

    Exit status: 0 when every position compared is equal and none is
    unreached, 1 when one is not or one is unreached, 2 when the run cannot be
    made (no corpus, no file, a bad argument). *)

open Jide_engine

let usage =
  String.concat "\n"
    [
      "usage: completion_scope --corpus <dir> [--corpus <dir>]...";
      "                       [--step <n>] [--seed <n>] [--limit <n>] [--quiet]";
      "       completion_scope --selftest";
      "";
      "For deterministically sampled bytes inside the functions of every";
      ".jazz and .jinc file under <dir>, compare the names completion offers";
      "with the names the resolver's scope holds at that byte.";
    ]

(* ------------------------------------------------------------------ *)
(* The sample *)

(* A 32-bit FNV-1a hash of the file name. It only has to be stable across
   runs and hosts, and to spread names over the first [step] bytes; it is not
   used as an identity anywhere. *)
let fnv1a (text : string) =
  let hash = ref 2166136261 in
  String.iter
    (fun character ->
      hash := !hash lxor Char.code character * 16777619 land 0xFFFFFFFF)
    text;
  !hash

(* [sample ~step ~seed ~length ~file] is every byte of a [length]-byte file
   that the sample asks about: the first is the file's own hash (and the seed)
   folded into [0, step), and the rest are one [step] apart. The hash is
   taken of the file name as the run spells it, which is why the run
   normalises the path before it samples. *)
let first_byte ~(step : int) ~(seed : int) ~(file : string) =
  let step = max 1 step in
  (fnv1a file + (max 0 seed * 2654435761)) mod step

(* The first bytes the sample is pinned to, for a formula of the shape of
   [first_byte]. The values are fixed numbers, worked out once from the
   formula and written here as literals. The self-test runs this predicate on
   the real formula and on two formulas that drop a term, and the second kind
   must be rejected. *)
let pinned_first_bytes (first_byte : step:int -> seed:int -> file:string -> int)
    =
  first_byte ~step:5 ~seed:1 ~file:"a.jazz" = 0
  && first_byte ~step:11 ~seed:2 ~file:"dir/b.jazz" = 6
  && first_byte ~step:11 ~seed:3 ~file:"dir/b.jazz" = 5

