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
    - [same-recovery]: the same comparison for the tree the recovery engine
      itself returns ({!Jide_jasmin.Recover.recover}) when it is asked for
      the file directly, instead of being reached only through an error.
      That is the driver's own assembly of the item list, taken without the
      fast path, so a location moved or an item dropped by a checkpoint,
      a restart or a blanking shows up as a difference rather than being
      assumed away.

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

