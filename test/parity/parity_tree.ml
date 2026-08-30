(** The tree half of the parser parity gate.

    For one file this program answers three questions and prints one tree.

    - [driver]: the item list the recovering driver returns, and how many
      diagnostics it reported for the file. The driver is
      {!Jide_jasmin.Parse.parse}, the entry the server's parse step uses.
    - [same-tree]: whether that item list is the list the compiler's own
      parser returns for the same bytes, {!Jasmin.Parseio.parse_program}.
      The two are compared as values, field for field, sharing ignored: every
      location and every annotation counts, and nothing is stripped,
      reordered, renumbered or normalised on either side.

      This answer is true by construction; it is not an external observation.
      {!Jide_jasmin.Parse.parse} runs [Parseio.parse_program] on the same
      bytes under the same name and returns that call's item list unchanged,
      so on a file the compiler parses the comparison asks whether that list
      equals itself. What it guards is the driver: it fails if the fast path
      ever rebuilds, filters, reorders or normalises the list, or if the two
      calls stop agreeing. The one external comparison in this harness is the
      printing described below, which the caller (test/parity/tree.sh) makes
      by running the release's own binary on the same bytes.
    - [same-recovery]: the same comparison for the tree the recovery engine
      itself returns ({!Jide_jasmin.Recover.recover}) when it is asked for
      the file directly, instead of being reached only through an error.
      That is the driver's own assembly of the item list, taken without the
      fast path, so a location moved or an item dropped by a checkpoint,
      a restart or a blanking shows up as a difference rather than being
      assumed away. The parser on the other side is the same one, so this
      answer is by construction as to the compiler too: what it tests is our
      assembly, not an independent parser.

    Stdout carries the driver's tree, rendered by the compiler's own AST
    printer ([Latex_printer.pp_prog]), and nothing else, so a caller can
    compare it byte for byte with the release's [jasmin2tex] on the same
    bytes. The answers go to stderr, one per line, and the exit status is 0
    only when the driver reported no diagnostic and both trees are the
    compiler parser's own.

    The release's printer elides source locations by construction: every
    located node it prints is read through [Location.unloc], so it cannot
    show a difference in them. That is why the structural comparison above
    does not use the printing, and why the printing is not the gate: it is
    the cross-check that our build of the release's printer, given our tree,
    produces the same bytes as the release's own binary given its own.

    There is exactly one observer outside this program, and it is that
    binary. Neither structural answer claims that two independent parsers
    agree: both compare the one parser with itself, and the run says so in the
    labels it prints.

    Usage:

    {v
      parity_tree <file>        print the tree, then the answers
      parity_tree --selftest    check this program against small sources
    v} *)

let usage =
  String.concat "\n"
    [
      "usage: parity_tree <file>";
      "       parity_tree --selftest";
      "";
      "Print the tree the recovering driver returns for <file>, as the";
      "compiler's own AST printer renders it, and on stderr how many";
      "diagnostics the driver reported and whether the driver's tree and the";
      "recovery engine's tree are the compiler parser's own.";
    ]

type answer = {
  items : Jasmin.Syntax.pprogram;
  diagnostics : int;
  same_tree : bool option;
      (** [None] when the compiler's parser rejects the text, so there is no
          tree to compare with. *)
  same_recovery : bool option;  (** [None] for the same reason. *)
  difference : string option;
      (** The first structural difference found, when one was. *)
}
(** One run over one source text. *)

(** [structural a b] is true when the two item lists are the same value.

    [Marshal] with [No_sharing] writes every field of every node and writes
    them without sharing, so two values that describe the same tree compare
    equal whatever their internal sharing is, and two values that differ in
    any field — a location included — do not. The tree is a value of records,
    variants, lists and strings; it holds no function and no cycle, which is
    what makes this a comparison rather than an encoding. *)
let structural (a : Jasmin.Syntax.pprogram) (b : Jasmin.Syntax.pprogram) =
  String.equal
    (Marshal.to_string a [ Marshal.No_sharing ])
    (Marshal.to_string b [ Marshal.No_sharing ])

(** The first line of [text] that carries something the printer wrote, so a
    diagnostic can quote an item without the printer's banner. *)
let first_meaningful_line (text : string) =
  let lines = String.split_on_char '\n' text in
  let keep line =
    let line = String.trim line in
    line <> "" && line.[0] <> '%'
  in
  match List.find_opt keep lines with
  | None -> "(nothing printed)"
  | Some line ->
      let line = String.trim line in
      if String.length line > 120 then String.sub line 0 120 ^ "..." else line

(** [label item] names one item as the printer renders it. *)
let label (item : Jasmin.Syntax.pitem Jasmin.Location.located) =
  first_meaningful_line
    (Format.asprintf "%a" Jasmin.Latex_printer.pp_prog [ item ])

(** [first_difference ours theirs] describes the first place the two item
    lists part: the item's index and what each side has there. *)
let rec first_difference ~(index : int) (ours : Jasmin.Syntax.pprogram)
    (theirs : Jasmin.Syntax.pprogram) =
  match (ours, theirs) with
  | [], [] -> Printf.sprintf "item %d: both lists end but compare unequal" index
  | ours :: _, [] ->
      Printf.sprintf "item %d: ours is `%s`, the compiler's tree ends here"
        index (label ours)
  | [], theirs :: _ ->
      Printf.sprintf "item %d: the compiler's tree has `%s`, ours ends here"
        index (label theirs)
  | ours :: rest_ours, theirs :: rest_theirs ->
      if structural [ ours ] [ theirs ] then
        first_difference ~index:(index + 1) rest_ours rest_theirs
      else if String.equal (label ours) (label theirs) then
        (* The printer reads every located node through [Location.unloc], so
           two items that differ only in a location or an annotation print
           the same. Saying so is the point: the printing cannot see this. *)
        Printf.sprintf
          "item %d: the two items print alike; a location or an annotation \
           differs (`%s`)"
          index (label ours)
      else
        Printf.sprintf "item %d: ours is `%s`, the compiler's is `%s`" index
          (label ours) (label theirs)

(** [compare_with ours compiler] is the answer for one pair of trees: equal,
    or the first difference. *)
let compare_with ~(what : string) (ours : Jasmin.Syntax.pprogram)
    (compiler : Jasmin.Syntax.pprogram) =
  if structural ours compiler then None
  else
    Some
      (Printf.sprintf "%s: %s" what (first_difference ~index:0 ours compiler))

(* The compiler's parser, over the same bytes. [Parseio.parse_program] raises
   [Syntax.ParseError] on a text it cannot parse, which is the one case where
   there is no compiler tree to compare with. *)
let compiler_tree ~(name : string) (text : string) =
  match
    Jasmin.Parseio.parse_program ~name (Jasmin.Utils.IO.input_string text)
  with
  | items -> Some items
  | exception _ -> None

let parse_text ~(name : string) (text : string) : answer =
  let parsed = Jide_jasmin.Parse.parse ~name text in
  let items = Jide_jasmin.Parse.items parsed in
  let recovered = Jide_jasmin.Recover.recover ~name ~text in
  let compiler = compiler_tree ~name text in
  let same_tree = Option.map (fun theirs -> structural items theirs) compiler in
  let same_recovery =
    Option.map
      (fun theirs -> structural recovered.Jide_jasmin.Recover.items theirs)
      compiler
  in
  let difference =
    match compiler with
    | None -> None
    | Some theirs -> (
        match compare_with ~what:"driver" items theirs with
        | Some _ as difference -> difference
        | None ->
            compare_with ~what:"recovery" recovered.Jide_jasmin.Recover.items
              theirs)
  in
  {
    items;
    diagnostics = List.length (Jide_jasmin.Parse.diagnostics parsed);
    same_tree;
    same_recovery;
    difference;
  }

let say_answer (answer : answer) =
  Printf.eprintf "driver: %d diagnostics\n" answer.diagnostics;
  (match answer.same_tree with
  | None -> Printf.eprintf "same-tree: n/a\n"
  | Some true -> Printf.eprintf "same-tree: yes\n"
  | Some false -> Printf.eprintf "same-tree: no\n");
  (match answer.same_recovery with
  | None -> Printf.eprintf "same-recovery: n/a\n"
  | Some true -> Printf.eprintf "same-recovery: yes\n"
  | Some false -> Printf.eprintf "same-recovery: no\n");
  (match answer.difference with
  | None -> ()
  | Some difference -> Printf.eprintf "first-difference: %s\n" difference);
  flush stderr

let print_tree (answer : answer) =
  Format.printf "%a@." Jasmin.Latex_printer.pp_prog answer.items;
  Format.pp_print_flush Format.std_formatter ()

let read (path : string) = In_channel.with_open_bin path In_channel.input_all

let status (answer : answer) =
  match (answer.diagnostics, answer.same_tree, answer.same_recovery) with
  | 0, Some true, Some true -> 0
  | _ -> 1

(** Two sources the self-test reads. The first is a program every Jasmin
    accepts; the second differs from it in one item's name, so the tree
    comparison and its first difference are exercised on a pair that really
    differs. *)
let valid =
  String.concat "\n"
    [
      "export fn main(reg u64 p) -> reg u64 {";
      "  reg u64 r;";
      "  r = [:u64 p];";
      "  r += [:u64 p - 8];";
      "  return r;";
      "}";
      "";
    ]

let also_valid =
  String.concat "\n"
    [ "fn other(reg u64 x) -> reg u64 {"; "  return x;"; "}"; "" ]

let different = String.map (fun c -> if c = 'm' then 'n' else c) valid
let invalid = "fn broken( {\n"

(** [contains ~needle haystack] is true when [needle] occurs in [haystack]. *)
let contains ~(needle : string) (haystack : string) =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else at (i + 1)
  in
  n > 0 && at 0

let check_selftest () =
  let failures = ref 0 in
  let check name condition =
    if not condition then (
      incr failures;
      Printf.eprintf "selftest: %s: failed\n" name)
  in
  let good = parse_text ~name:"valid.jazz" valid in
  check "valid: the driver reports no diagnostic" (good.diagnostics = 0);
  check "valid: the tree is the compiler parser's own"
    (good.same_tree = Some true);
  check "valid: the recovery engine's tree is too"
    (good.same_recovery = Some true);
  check "valid: there is no first difference" (good.difference = None);
  check "valid: the exit status is the passing one" (status good = 0);
  let rendered = Format.asprintf "%a" Jasmin.Latex_printer.pp_prog good.items in
  check "valid: one function is printed"
    (contains ~needle:"\\jasminkw{fn}" rendered);
  check "valid: the printer is the compiler's, banner included"
    (contains ~needle:"The produced LATEX snippet" rendered);
  let bad = parse_text ~name:"invalid.jazz" invalid in
  check "invalid: the driver reports a diagnostic" (bad.diagnostics > 0);
  check "invalid: there is no compiler tree to compare with"
    (bad.same_tree = None && bad.same_recovery = None);
  check "invalid: the exit status is the failing one" (status bad = 1);
  let again = parse_text ~name:"valid.jazz" valid in
  check "reparsing the valid source is stable"
    (structural again.items good.items);
  (* The structural comparison is a comparison and not a constant: the same
     bytes read under another name carry another file name in every location,
     and the answer changes. That is what lets it fail on a location, which is
     the only thing it can add over the call it is compared with. *)
  let renamed_file = parse_text ~name:"another-name.jazz" valid in
  check "the same bytes under another name are a different tree"
    (not (structural renamed_file.items good.items));
  check "the same bytes under another name are still the compiler parser's own"
    (renamed_file.same_tree = Some true);
  (* The comparison itself, on two trees that really differ. *)
  let mine = parse_text ~name:"a.jazz" valid
  and other = parse_text ~name:"b.jazz" also_valid in
  check "two different sources have different trees"
    (not (structural mine.items other.items));
  (match first_difference ~index:0 mine.items other.items with
  | difference ->
      check
        (Printf.sprintf "the first difference is at item zero (%s)" difference)
        (contains ~needle:"item 0" difference);
      check
        (Printf.sprintf "the first difference names both sides (%s)" difference)
        (contains ~needle:"main" difference
        && contains ~needle:"other" difference)
  | exception _ -> check "the first difference is computable" false);
  let renamed = parse_text ~name:"c.jazz" different in
  check "one renamed function is still the compiler's tree"
    (renamed.same_tree = Some true);
  check "one renamed function is not the first source's tree"
    (not (structural renamed.items mine.items));
  if !failures = 0 then print_endline "parity_tree: selftest ok"
  else (
    Printf.eprintf "parity_tree: %d selftest check(s) failed\n" !failures;
    exit 1)

(* ------------------------------------------------------------------ *)
(* The corpus run: every file the release's parser accepts, our answers and
   the release's own printing of the same bytes. *)

module H = Jide_harness.Harness

type printed =
  | Same
  | Differs of string
  | Refused of string
  | Silent
  | Uncompared

type row = {
  relative : string;
  diagnostics : int option;  (** [None]: the printer gave no answer *)
  tree : bool option;
  recovery : bool option;
  first : string option;
  printed : printed;
}

let corpus_usage =
  "usage: parity_tree --corpus <dir> [--checks <list>] [--jasmin2tex <path>] \
   [--jobs N]\n\n\
   <dir> is a release's compiler directory (its tests/success and examples) or \
   its\n\
   tests directory; <list> holds label, path and architecture per line."

(* The first line where two printings part, from each side. *)
let first_line_difference ours theirs =
  let rec go = function
    | a :: rest_a, b :: rest_b ->
        if String.equal a b then go (rest_a, rest_b)
        else Printf.sprintf "ours: %s; compiler: %s" a b
    | a :: _, [] -> "ours: " ^ a ^ "; compiler: (end)"
    | [], b :: _ -> "ours: (end); compiler: " ^ b
    | [], [] -> "the two printings differ"
  in
  go (String.split_on_char '\n' ours, String.split_on_char '\n' theirs)

let one ~root ~printer (relative, arch) =
  let path = Filename.concat root relative in
  let base = Filename.basename path in
  let answer = parse_text ~name:base (read path) in
  let printed =
    match printer with
    | None -> Uncompared
    | Some printer ->
        let ours =
          Format.asprintf "%a@." Jasmin.Latex_printer.pp_prog answer.items
        in
        let out =
          H.run ~cwd:(Filename.dirname path) printer [ "--arch"; arch; base ]
        in
        if out.H.status <> 0 then
          Refused (List.hd (String.split_on_char '\n' out.H.err))
        else if out.H.out = "" then Silent
        else if String.equal ours out.H.out then Same
        else Differs (first_line_difference ours out.H.out)
  in
  {
    relative;
    diagnostics = Some answer.diagnostics;
    tree = answer.same_tree;
    recovery = answer.same_recovery;
    first = answer.difference;
    printed;
  }

let corpus_run (args : H.args) =
  H.name := "tree";
  let root = H.directory (Option.value ~default:"" (args.value "--corpus")) in
  let printer =
    Option.map (H.executable ~what:"--jasmin2tex") (args.value "--jasmin2tex")
  in
  let is_dir path = Sys.file_exists path && Sys.is_directory path in
  let folders =
    if is_dir (Filename.concat root "tests/success") then
      [ "tests/success"; "examples" ]
    else if is_dir (Filename.concat root "success") then [ "success" ]
    else
      H.die "no tests/success under %s: name the release's compiler directory"
        root
  in
  let checks =
    match args.value "--checks" with
    | Some list ->
        List.map (fun (c : H.check) -> (c.path, c.arch)) (H.read_checks list)
    | None ->
        List.concat_map
          (fun folder ->
            let dir = Filename.concat root folder in
            if is_dir dir then
              List.map
                (fun file ->
                  let relative = folder ^ "/" ^ file in
                  (relative, H.arch_of relative))
                (H.files dir)
            else [])
          folders
  in
  let seen = Hashtbl.create 1024 in
  let files =
    List.filter
      (fun (relative, _) ->
        if Hashtbl.mem seen relative then false
        else (
          Hashtbl.add seen relative ();
          true))
      checks
  in
  if files = [] then
    H.die "no files: the corpus holds no .jazz under its success folder";
  List.iter
    (fun (relative, _) ->
      if not (Sys.file_exists (Filename.concat root relative)) then
        H.die "no such file under %s: %s" root relative)
    files;
  let rows =
    List.map2
      (fun (relative, _) -> function
        | Ok row -> row
        | Error _ ->
            {
              relative;
              diagnostics = None;
              tree = None;
              recovery = None;
              first = None;
              printed = Uncompared;
            })
      files
      (H.parallel ~jobs:(H.jobs args) (one ~root ~printer) files)
  in
  let total = List.length rows in
  let count p = List.length (List.filter p rows) in
  let answered = function Some _ -> true | None -> false in
  (* A row with no answer is the printer's fault, not a tree that differs. *)
  let noanswer r = r.diagnostics = None in
  let rejected r = (not (noanswer r)) && r.tree = None in
  let tree_yes = count (fun r -> r.tree = Some true) in
  let tree_no = count (fun r -> r.tree = Some false) in
  let tree_na = count rejected in
  let tree_unknown = count noanswer in
  let recovery_yes = count (fun r -> r.recovery = Some true) in
  let recovery_no = count (fun r -> r.recovery = Some false) in
  let printed p = count (fun r -> p r.printed) in
  let printed_yes = printed (( = ) Same) in
  let printed_no = printed (function Differs _ -> true | _ -> false) in
  let refused = printed (function Refused _ -> true | _ -> false) in
  let silent = printed (( = ) Silent) in
  let mismatch r =
    if noanswer r then
      Some "the tree printer gave no answer: the driver raised or was killed"
    else if r.tree = None then
      Some
        (Printf.sprintf
           "the compiler rejects it, so there is no compiler tree to compare \
            with (driver diagnostics: %d)"
           (Option.get r.diagnostics))
    else if r.tree = Some true && r.recovery = Some true then
      if r.diagnostics <> Some 0 then
        Some
          (Printf.sprintf
             "the driver reports %d diagnostic(s) on a file the compiler parses"
             (Option.get r.diagnostics))
      else
        match r.printed with
        | Differs detail -> Some ("printing: " ^ detail)
        | Refused reason ->
            Some
              (Printf.sprintf
                 "the parse-and-print produced no tree (refused: %s); the \
                  printing was not compared"
                 reason)
        | Silent ->
            Some
              "the parse-and-print produced no tree (silent); the printing was \
               not compared"
        | Same | Uncompared -> None
    else Some (Option.value ~default:"a tree differs" r.first)
  in
  if args.value "--checks" <> None then
    H.say
      "the check list names %d checks over %d files; the tree does not depend \
       on the target, so one comparison per file covers them"
      (List.length checks) total;
  H.say
    "%d files; the compiler's parser accepts %d, rejects %d, and the tree \
     printer did not answer for %d"
    total (tree_yes + tree_no) tree_na tree_unknown;
  H.say "no error from the driver: %d of %d"
    (count (fun r -> r.diagnostics = Some 0))
    total;
  H.say
    "the same tree as the compiler, by construction: %d of %d answered; %d no \
     answer; %d the compiler's parser rejects the file"
    tree_yes (tree_yes + tree_no) tree_unknown tree_na;
  H.say
    "the same recovery tree as the compiler, by construction: %d of %d \
     answered; %d no answer"
    recovery_yes
    (recovery_yes + recovery_no)
    (count (fun r -> noanswer r));
  (match printer with
  | Some _ ->
      H.say
        "the same printing as the compiler, external, byte for byte: %d of %d \
         compared; %d not compared (%d refused, %d answered nothing)"
        printed_yes (printed_yes + printed_no) (refused + silent) refused silent
  | None -> H.say "no --jasmin2tex named: the printing was not compared");
  (* By group, in the order the checks name them. *)
  let group relative =
    let relative =
      if String.length relative > 6 && String.sub relative 0 6 = "tests/" then
        String.sub relative 6 (String.length relative - 6)
      else relative
    in
    H.group_of relative
  in
  let groups =
    List.fold_left
      (fun acc (relative, _) ->
        let g = group relative in
        if List.mem g acc then acc else acc @ [ g ])
      [] checks
  in
  H.say
    "by group: checks, files, the compiler's parser answered, no tree, a \
     different tree";
  List.iter
    (fun g ->
      let mine = List.filter (fun r -> group r.relative = g) rows in
      let n p = List.length (List.filter p mine) in
      H.say "  %s: %d checks, %d files, %d answered, %d no tree, %d unequal" g
        (List.length
           (List.filter (fun (relative, _) -> group relative = g) checks))
        (List.length mine)
        (n (fun r -> answered r.tree))
        (n rejected)
        (n (fun r -> r.tree = Some false)))
    groups;
  let differing =
    List.filter_map (fun r -> Option.map (fun m -> (r, m)) (mismatch r)) rows
  in
  List.iter (fun (r, m) -> H.say "different %s: %s" r.relative m) differing;
  if tree_unknown > 0 && tree_yes + tree_no = 0 then (
    H.say "the tree printer answered with a tree for none of the %d files" total;
    exit 2);
  if printer <> None && printed_yes + printed_no = 0 && tree_na <> total then (
    H.say
      "the parse-and-print produced a tree for none of the %d files: it is not \
       a working oracle (%d refused, %d answered nothing)"
      total refused silent;
    exit 2);
  if differing = [] then (
    H.say "every file the compiler parses passes";
    exit 0);
  H.say "the corpus does not agree";
  exit 1

let main () =
  match Array.to_list Sys.argv with
  | _ :: [ "--selftest" ] -> check_selftest ()
  | _ :: [ "--help" ] | _ :: [ "-help" ] ->
      print_endline usage;
      print_endline corpus_usage;
      exit 0
  | _ :: [ path ] -> (
      match read path with
      | text ->
          let answer = parse_text ~name:path text in
          print_tree answer;
          say_answer answer;
          exit (status answer)
      | exception Sys_error message ->
          Printf.eprintf "parity_tree: cannot read %s: %s\n" path message;
          exit 2)
  | _ :: args ->
      corpus_run
        (H.parse_args ~usage:corpus_usage ~flags:[]
           ~options:[ "--corpus"; "--checks"; "--jasmin2tex"; "--jobs" ]
           args)
  | [] ->
      prerr_endline usage;
      exit 2

let () = main ()
