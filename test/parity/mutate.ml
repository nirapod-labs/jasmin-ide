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

let run (args : H.args) =
  H.name := "mutate";
  let corpus = H.directory (Option.value ~default:"" (args.value "--corpus")) in
  let root =
    let tests = Filename.concat corpus "compiler/tests" in
    if Sys.file_exists tests && Sys.is_directory tests then tests else corpus
  in
  let seed = Option.value ~default:"0" (args.value "--seed") in
  let files = H.int_arg args "--files" ~default:0 in
  let present =
    List.filter (fun f -> Sys.file_exists (Filename.concat root f)) folders
  in
  if present = [] then
    H.die "no corpus: %s holds none of %s" root (String.concat " " folders);
  (* One list over every folder, in byte order, as the emitter always read
     them: the order fixes each mutant's number. *)
  let relatives =
    List.sort String.compare
      (List.concat_map
         (fun f ->
           List.map (fun r -> f ^ "/" ^ r) (H.files (Filename.concat root f)))
         present)
  in
  let read relative =
    try Some (H.read_file (Filename.concat root relative))
    with Sys_error _ -> None
  in
  let mutants = Emit.emit ~seed ~files ~read relatives in
  if args.flag "--list" then (
    List.iter
      (fun (m : Emit.t) ->
        Printf.printf "%d\t%s\t%s\t%s\n" m.index (family m) m.relative m.label;
        Option.iter
          (fun dir ->
            H.write_file
              (Filename.concat dir (Printf.sprintf "%d.jazz" m.index))
              m.text)
          (args.value "--write"))
      mutants;
    exit 0);
  let checker =
    match args.value "--checker" with
    | Some path -> H.executable ~what:"--checker" path
    | None -> H.built "lsp-server/native/main.exe"
  in
  let compiler, version = H.compiler (args.value "--jasminc") in
  let defects =
    match args.value "--defects" with
    | Some file -> read_defects ~seed file
    | None -> H.die "name the list of known compiler defects with --defects"
  in
  if mutants = [] then H.die "no mutant was made";
  let mutated =
    List.length
      (List.sort_uniq compare
         (List.map (fun (m : Emit.t) -> m.relative) mutants))
  in
  Printf.printf "mutants: %d of %d files, %d mutants%s\n" mutated
    (List.length relatives) (List.length mutants)
    (String.concat ""
       (List.map
          (fun f ->
            Printf.sprintf ", %s %d" (Mutant.family_name f)
              (List.length
                 (List.filter (fun (m : Emit.t) -> m.family = f) mutants)))
          Emit.families));
  let unchanged =
    List.filter (fun (m : Emit.t) -> read m.relative = Some m.text) mutants
  in
  if unchanged <> [] then
    H.die "%d of %d mutants are the corpus text; nothing would be compared"
      (List.length unchanged) (List.length mutants);
  (* The work tree: the corpus folders, then pending/, which a require may
     read but which is never mutated. *)
  let work = H.temp_dir "mutate" in
  let master = Filename.concat work "master" in
  Unix.mkdir master 0o755;
  List.iter
    (fun f ->
      let copy =
        H.run "cp" [ "-R"; Filename.concat root f; Filename.concat master f ]
      in
      if copy.H.status <> 0 then H.die "cannot copy %s" f)
    (present
    @
    if Sys.file_exists (Filename.concat root "pending") then [ "pending" ]
    else []);
  let probe = Filename.concat work "probe.jazz" in
  H.write_file probe "export fn probe() {\n  return;\n}\n";
  let answer =
    H.run ~merge:true compiler [ "-arch"; "x86-64"; "-until_cstexp"; probe ]
  in
  if answer.H.status <> 0 then
    H.die "the compiler cannot check %s (exit %d): %s" probe answer.H.status
      answer.H.out;
  (* Files go to workers whole, by their order in the list. *)
  let jobs = H.jobs args in
  let ordinal = Hashtbl.create 1024 in
  List.iter
    (fun (m : Emit.t) ->
      if not (Hashtbl.mem ordinal m.relative) then
        Hashtbl.add ordinal m.relative (Hashtbl.length ordinal + 1))
    mutants;
  let plans =
    List.init jobs (fun w ->
        ( w + 1,
          List.filter
            (fun (m : Emit.t) -> Hashtbl.find ordinal m.relative mod jobs = w)
            mutants ))
  in
  let results =
    List.concat_map
      (function
        | Ok results -> results
        | Error why -> H.die "a worker gave no result: %s" why)
      (H.parallel ~jobs (worker ~master ~work ~checker ~compiler) plans)
    |> List.sort (fun a b -> compare a.mutant.index b.mutant.index)
  in
  if List.length results <> List.length mutants then
    H.die "%d of %d mutants wrote no result"
      (List.length mutants - List.length results)
      (List.length mutants);
  Option.iter
    (fun file ->
      H.write_file file
        (String.concat ""
           (List.map
              (fun x ->
                Printf.sprintf
                  "%d\t%s\t%s\t%s\t%d\t%d\t%d\t%s\t%s\t%d\t%d\t%d\t%s\t%s\t%s\n"
                  x.mutant.index (family x.mutant) x.mutant.relative x.arch
                  x.c_status x.c.line x.c.column x.c.kind x.c.message x.r_status
                  x.r.line x.r.column x.r.message x.contains x.mutant.label)
              results)))
    (args.value "--tsv");
  (* The defects list against this run. *)
  let entry x =
    List.find_opt
      (fun d ->
        d.path = x.mutant.relative
        && d.d_family = family x.mutant
        && d.label = x.mutant.label)
      defects
  in
  let excused = ref [] and stale = ref [] and mismatches = ref [] in
  let same = ref 0
  and differ = ref 0
  and accepted = ref 0
  and rejected = ref 0 in
  let agreed = ref 0
  and faults = ref 0
  and contains = ref 0
  and preceded = ref 0 in
  let compared = ref [] in
  List.iter
    (fun x ->
      let m = x.mutant in
      let name =
        Printf.sprintf "%s mutant %d (%s, %s)" m.relative m.index (family m)
          m.label
      in
      match entry x with
      | Some d ->
          let behaviour =
            Printf.sprintf "status %d at %d:%d: %s" x.r_status x.r.line
              x.r.column x.r.message
          in
          let reproduced =
            x.r_status = d.status && x.r.message = d.message
            && (d.status > 1 || (x.r.line = d.line && x.r.column = d.column))
          in
          let copied =
            x.c_status = x.r_status
            && (x.r_status <> 1
               || (x.c.line = x.r.line && x.c.column = x.r.column))
          in
          if reproduced && not copied then
            excused :=
              Printf.sprintf "%s: %s; the compiler gives %s, as listed" name
                d.what behaviour
              :: !excused
          else if reproduced then
            stale :=
              Printf.sprintf
                "%s: the checker gives the compiler's defective answer (%s): %s"
                name behaviour d.what
              :: !stale
          else
            stale :=
              Printf.sprintf
                "%s: listed as \"%s\", but the compiler now gives %s" name
                d.what behaviour
              :: !stale
      | None ->
          compared := x :: !compared;
          let expected =
            if x.r_status = 0 then "accept"
            else Printf.sprintf "reject at %d:%d" x.r.line x.r.column
          in
          let observed =
            if x.c_status = 0 then "accept"
            else Printf.sprintf "reject at %d:%d" x.c.line x.c.column
          in
          let checker_text =
            if x.c_status = 0 then "accept" else x.c.kind ^ ": " ^ x.c.message
          in
          let compiler_text =
            if x.r_status = 0 then "accept" else x.r.message
          in
          let mismatch expected observed =
            mismatches :=
              (name, expected, observed, checker_text, compiler_text, x)
              :: !mismatches
          in
          if x.c_status > 1 || x.r_status > 1 then (
            incr faults;
            mismatch
              (Printf.sprintf "the compiler exits %d" x.r_status)
              (Printf.sprintf "the checker exits %d" x.c_status))
          else if x.c_status = 0 && x.r_status = 0 then incr agreed
          else if x.c_status = 0 then (
            incr accepted;
            mismatch expected observed)
          else if x.r_status = 0 then (
            incr rejected;
            mismatch expected observed)
          else if x.c.line = x.r.line && x.c.column = x.r.column then (
            incr same;
            if x.contains = "yes" then incr contains)
          else (
            incr differ;
            if x.contains = "yes" then (
              incr contains;
              incr preceded);
            mismatch expected observed))
    results;
  let absent =
    List.filter
      (fun d ->
        not
          (List.exists
             (fun x ->
               d.path = x.mutant.relative
               && d.d_family = family x.mutant
               && d.label = x.mutant.label)
             results))
      defects
  in
  let absent_lines =
    List.map
      (fun d ->
        if files = 0 then
          let line =
            Printf.sprintf
              "%s (%s, %s): no such mutant in a run over the whole corpus: %s"
              d.path d.d_family d.label d.what
          in
          Printf.sprintf "stale defect entry: %s" line
        else
          Printf.sprintf "defect entry not in this run: %s (%s, %s)" d.path
            d.d_family d.label)
      absent
  in
  let stale_count =
    List.length !stale + if files = 0 then List.length absent else 0
  in
  let excused_count = List.length !excused in
  let total = List.length results in
  (* The tables: per group, then per family, over every mutant. *)
  let table title key =
    Printf.printf "mutate: %-32s %6s %6s %6s %6s %8s %6s\n" title "cases" "same"
      "differ" "accept" "rejected" "faults";
    let keys =
      List.fold_left
        (fun acc x -> if List.mem (key x) acc then acc else acc @ [ key x ])
        [] results
    in
    let line name xs =
      let n p = List.length (List.filter p xs) in
      Printf.printf "mutate: %-32s %6d %6d %6d %6d %8d %6d\n" name
        (List.length xs)
        (n (fun x ->
             x.c_status = 1 && x.r_status = 1 && x.c.line = x.r.line
             && x.c.column = x.r.column))
        (n (fun x ->
             x.c_status = 1 && x.r_status = 1
             && (x.c.line <> x.r.line || x.c.column <> x.r.column)))
        (n (fun x -> x.c_status = 0 && x.r_status <> 0 && x.r_status <= 1))
        (n (fun x -> x.c_status <> 0 && x.c_status <= 1 && x.r_status = 0))
        (n (fun x -> x.c_status > 1 || x.r_status > 1))
    in
    List.iter (fun k -> line k (List.filter (fun x -> key x = k) results)) keys;
    if title = "group" then line "total" results
  in
  H.say "%s" version;
  if files = 0 then H.say "seed %s; every corpus file mutated" seed
  else H.say "seed %s; %d files to mutate" seed files;
  H.say "files mutated: %d of %d" mutated (List.length relatives);
  H.say "cases compared: %d; both accept: %d; both reject: %d" total !agreed
    (!same + !differ);
  H.say "the same line and column: %d; a different location: %d" !same !differ;
  H.say "accepted where the compiler rejects: %d; rejected where it accepts: %d"
    !accepted !rejected;
  H.say "did not finish: %d" !faults;
  H.say
    "agreement: %d of %d; difference: %d; known compiler defects: %d excused, \
     %d stale"
    (!agreed + !same) total
    (!differ + !accepted + !rejected + !faults)
    excused_count stale_count;
  H.say
    "the compiler's location is somewhere in the checker's output: %d of %d \
     rejected mutants"
    !contains (!same + !differ);
  H.say
    "of the %d at a different location, the compiler's is in the checker's \
     output too: %d"
    !differ !preceded;
  table "group" (fun x -> H.group_of x.mutant.relative);
  table "family" (fun x -> family x.mutant);
  List.iter (H.say "known compiler defect: %s") (List.rev !excused);
  List.iter (H.say "stale defect entry: %s") (List.rev !stale);
  List.iter (H.say "%s") absent_lines;
  let mismatches = List.rev !mismatches in
  List.iter
    (fun (name, expected, observed, checker_text, compiler_text, _) ->
      H.say "mismatch %s: expected %s, observed %s" name expected observed;
      H.say "  the checker's first diagnostic: %s" checker_text;
      H.say "  the compiler's first diagnostic: %s" compiler_text)
    mismatches;
  Option.iter
    (fun dir ->
      if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
      List.iter
        (fun (_, _, _, _, _, x) ->
          List.iteri
            (fun i text ->
              if text <> "" then
                H.write_file
                  (Filename.concat dir
                     (Printf.sprintf "mutant-%d.%s" x.mutant.index
                        (List.nth [ "c"; "c.err"; "o" ] i)))
                  text)
            x.raw)
        mismatches)
    (args.value "--keep");
  if !same + !differ = 0 && !accepted = 0 && !rejected = 0 then (
    H.say "no mutant changed either verdict: the run compared no first error";
    exit 2);
  if mismatches <> [] then (
    H.say "the mutants do not agree";
    exit 1);
  if stale_count > 0 then (
    H.say "%d entry(ies) of the known compiler defects do not hold" stale_count;
    exit 1);
  if excused_count > 0 then (
    H.say
      "every mutant agrees, except %d on which the compiler itself is wrong, \
       as listed"
      excused_count;
    exit 0);
  H.say "every mutant agrees";
  exit 0

