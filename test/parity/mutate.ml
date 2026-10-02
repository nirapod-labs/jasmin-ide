(** First-error parity on mutated files: one single-token mutant per corpus
    file per family ({!Emit}), given to the checker and to the compiler, and
    their first errors compared. A mutant agrees when both accept it, or both
    reject it at the same line and column. The compiler runs at
    [-until_cstexp], the part of it the checker models.

    The corpus is copied into a work directory and each worker mutates its
    own copy, one file at a time, putting the original back after each
    mutant, so every check sees exactly one mutated file and the corpus named
    on the command line is never touched.

    A mutant on which the compiler itself is wrong is not one the checker
    should copy. The list of known compiler defects ([--defects]) names each
    such mutant by seed, path, family and label, with the compiler's exit
    status, location and message on it. A listed mutant on which the
    compiler behaves so is excused; one on which it behaves otherwise, one a
    run over the whole corpus does not make, and one on which the checker
    gives the compiler's wrong answer are stale, and fail the run.

    Exit status: 0 when every mutant agrees or is an excused defect, 1 when
    one does not or an entry is stale, 2 when the run cannot be made (no
    mutant, a mutant that is its own source, no mutant rejected at all). *)

module H = Jide_harness.Harness
open Readers

let usage =
  "usage: mutate --corpus <dir> --jasminc <path> --defects <list> [--seed TEXT]\n\
  \              [--files N] [--checker <path>] [--jobs N] [--tsv <file>] \
   [--keep <dir>]\n\
  \       mutate --corpus <dir> --list [--write <dir>] [--seed TEXT] [--files \
   N]\n\n\
   <dir> is a release's compiler/tests directory or a root that holds it.\n\
   --list prints the mutants the run would make, and --write writes each one\n\
   to <dir>/<index>.jazz; nothing is run."

let folders = [ "success"; "fail"; "sct-checker"; "safety"; "exec"; "warning" ]

type result = {
  mutant : Emit.t;
  arch : string;
  c_status : int;
  c : first;
  r_status : int;
  r : first;
  contains : string;
      (** whether the compiler's location is in the checker's output *)
  raw : string list;
}

let family m = Mutant.family_name m.Emit.family
let dash s = if s = "" then "-" else s

(* One worker: its own copy of the corpus, its mutants one after another. *)
let worker ~master ~work ~checker ~compiler (number, mutants) =
  let tree = Filename.concat work (Printf.sprintf "tree.%d" number) in
  let copy = H.run "cp" [ "-R"; master; tree ] in
  if copy.H.status <> 0 then failwith ("cannot copy the corpus: " ^ copy.H.err);
  List.map
    (fun (m : Emit.t) ->
      let file = Filename.concat tree m.relative in
      let original = H.read_file file in
      H.write_file file m.text;
      let arch = H.arch_of m.relative in
      let out =
        H.run ~cwd:tree checker [ "check"; "--arch"; arch; "--"; m.relative ]
      in
      let log =
        H.run ~merge:true compiler [ "-arch"; arch; "-until_cstexp"; file ]
      in
      H.write_file file original;
      let c = checker_first out.H.out
      and r = compiler_first ~root:tree log.H.out in
      let contains =
        if log.H.status <> 0 && r.line <> 0 then
          let pattern = Printf.sprintf ":%d:%d: " r.line (r.column + 1) in
          if index_from out.H.out 0 pattern <> None then "yes" else "no"
        else "na"
      in
      {
        mutant = m;
        arch;
        c_status = out.H.status;
        c = { c with kind = dash c.kind; message = dash c.message };
        r_status = log.H.status;
        r = { r with kind = "-"; message = dash r.message };
        contains;
        raw = [ out.H.out; out.H.err; log.H.out ];
      })
    mutants

type defect = {
  path : string;
  d_family : string;
  label : string;
  status : int;
  line : int;
  column : int;
  message : string;
  what : string;
}

let read_defects ~seed file =
  let text =
    try H.read_file file with Sys_error message -> H.die "%s" message
  in
  List.concat
    (List.mapi
       (fun number line ->
         if line = "" || line.[0] = '#' || is_blank line then []
         else
           match String.split_on_char '\t' line with
           | s :: path :: d_family :: label :: status :: l :: c :: message
             :: what :: _ -> (
               match
                 ( int_of_string_opt status,
                   int_of_string_opt l,
                   int_of_string_opt c )
               with
               | Some status, Some line, Some column ->
                   if s <> seed then []
                   else
                     [
                       {
                         path;
                         d_family;
                         label;
                         status;
                         line;
                         column;
                         message;
                         what;
                       };
                     ]
               | _ ->
                   H.die
                     "the list of known compiler defects has a malformed line: \
                      %s line %d"
                     file (number + 1))
           | _ ->
               H.die
                 "the list of known compiler defects has a malformed line: %s \
                  line %d"
                 file (number + 1))
       (String.split_on_char '\n' text))

