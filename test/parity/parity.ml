(** Parity of the checker with the reference compiler.

    Every [.jazz] file under the corpus folders is checked once per
    architecture its path names, with [jasmin-lsp check], and the verdict and
    the first error are compared with the reference:

    - [--jasminc PATH]: the compiler, run here as
      [jasminc -arch <arch> -until_cstexp <file>]. [-until_cstexp] is the first
      checkpoint after [Compile.preprocess] (parameter expansion and the main
      typing pass), which is where the checker's own passes end;
    - no compiler: the release's [negative.expected], which records the first
      error of every negative case. A case it does not name, or names with a
      pass after the checkpoint, is expected to be accepted.

    A check agrees when both sides accept, or both reject with the same line,
    column and message. The release's record is compared where the reference
    reproduces it at the checkpoint. Exit status: 0 when every check agrees,
    1 when one differs, 2 when the run cannot be made. *)

module H = Jide_harness.Harness

let usage =
  "usage: parity --corpus <dir> [--checks <list>] [--jasminc <path>] \
   [--checker <path>]\n\
  \              [--jobs N] [--tsv <file>] [--keep <dir>]\n\
  \       parity --selftest\n\n\
   <dir> is a release's compiler/tests directory or a root that holds it.\n\
   <list> holds one label, path and architecture per line, tab separated.\n\
   Without --jasminc the release's negative.expected is the reference."

(* ------------------------------------------------------------------ *)
(* Text *)

let is_digit c = c >= '0' && c <= '9'
let is_blank s = String.for_all (fun c -> c = ' ' || c = '\t') s

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

let drop n s = String.sub s n (String.length s - n)

(* One space for every run of blanks, none at either end. *)
let collapse s =
  let b = Buffer.create (String.length s) in
  let pending = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' then pending := Buffer.length b > 0
      else (
        if !pending then Buffer.add_char b ' ';
        pending := false;
        Buffer.add_char b c))
    s;
  Buffer.contents b

