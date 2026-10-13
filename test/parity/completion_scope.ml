(** The completion measurement: the names completion offers against the
    resolver's own scope, and what completion offers after a [#] against the
    compiler's own intrinsic list, for every architecture.

    The name requirement is that the names in scope at sampled positions equal
    the resolver's scope. The two sides of that equality are read here from the
    two places the feature reads them, and never from a second derivation: the
    offered names are {!Jide_engine.Complete.completion}'s items minus the
    lexer's keywords, and the scope is {!Jide_engine.Resolve.scope_at} at the
    same byte of the same file. Completion's own prefix is applied to the
    scope's names, because that is the answer a client sees: at a byte in the
    middle of a word the resolver still holds every name, and completion shows
    the ones the word being typed can still become.

    The intrinsic requirement is that, after a [#], completion offers exactly
    the intrinsic names of the document's architecture, which is the list
    [jasminc -arch <arch> -help-intrinsics] prints. The compiler's list is read
    by running the command named by [--compiler] once per architecture, never
    from the linked library, and every answer is compared with it. Every byte
    just after a [#] in a corpus file is asked, sampled or not, because a
    primitive call can be written anywhere in a body. The prefix is the word
    being typed after that [#], as completion reads it.

    A position where the byte follows a [#] and an opening bracket is not a name
    position: completion answers that with the annotation keys, which are not
    names in scope. The run counts those apart instead of asking the question
    there. A position outside every function the file declares is not asked
    either, because the requirement covers positions inside expressions; it is
    counted apart too. A position the resolver never reached has no scope to
    compare with: it is counted apart and fails the run, because both sides
    would then be empty and agree by default.

    The sample is fixed by three numbers, all printed by the run: [step] is the
    distance between sampled bytes, [seed] is a constant folded into the file's
    own hash to move the first sampled byte inside one step, and [limit] bounds
    the number of name positions compared. Nothing here draws a random number,
    so the same corpus, step, seed and limit name the same bytes on every host.

    A negative control runs on a document whose answer is known. Each
    architecture must answer with its own list, and none may answer with another
    architecture's list, or the comparison could not tell the architectures
    apart.

    Usage:

    {v
      completion_scope --compiler <command> --corpus <dir> [--corpus <dir>]...
                       [--step <n>] [--seed <n>] [--limit <n>] [--quiet]
      completion_scope --selftest
    v}

    Exit status: 0 when every name position compared is equal, none is
    unreached, every intrinsic answer is equal and the negative control holds;
    1 when one of those does not hold; 2 when the run cannot be made (no corpus,
    no file, no compiler, a compiler whose list the program does not understand,
    a bad argument). *)

open Jide_engine

let usage =
  String.concat "\n"
    [
      "usage: completion_scope --compiler <command> --corpus <dir> [--corpus \
       <dir>]...";
      "                       [--step <n>] [--seed <n>] [--limit <n>] [--quiet]";
      "       completion_scope --selftest";
      "";
      "For deterministically sampled bytes inside the functions of every";
      ".jazz and .jinc file under <dir>, compare the names completion offers";
      "with the names the resolver's scope holds at that byte. After every hash";
      "in those files, compare what completion offers with the compiler's own";
      "intrinsic list (-arch <arch> -help-intrinsics) for every architecture.";
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

let sample ~(step : int) ~(seed : int) ~(length : int) ~(file : string) =
  let step = max 1 step in
  if length <= 0 then []
  else
    let first = first_byte ~step ~seed ~file in
    let rec walk acc byte =
      if byte >= length then List.rev acc else walk (byte :: acc) (byte + step)
    in
    walk [] first

(* The byte just after each hash of the text. Every one is asked, because a
   primitive call's name starts there, whether or not the sample reaches it. *)
let after_hash_bytes (text : string) =
  let rec collect acc index =
    match String.index_from_opt text index '#' with
    | None -> List.rev acc
    | Some hash -> collect ((hash + 1) :: acc) (hash + 1)
  in
  collect [] 0

(* The spans of the file's own functions, the ones a byte must be inside for
   the clause's "inside expressions" to hold. A namespace nests them, so the
   walk goes through every item's children. *)
let function_spans (index : Index.t) =
  let rec walk acc (items : Index.item list) =
    List.fold_left
      (fun acc (item : Index.item) ->
        let acc =
          match item.Index.item_kind with
          | Index.Function -> item.Index.item_span :: acc
          | _ -> acc
        in
        walk acc item.Index.item_children)
      acc items
  in
  walk [] index.Index.outline

let inside (spans : Jide_lang.Span.t list) (byte : int) =
  List.exists
    (fun (span : Jide_lang.Span.t) ->
      span.Jide_lang.Span.start_byte <= byte
      && byte < span.Jide_lang.Span.end_byte)
    spans

(* ------------------------------------------------------------------ *)
(* What completion offers and what the scope holds *)

let scope_names (scope : Resolve.scope) =
  List.filter_map
    (fun (binding : Resolve.binding) -> Some binding.Resolve.name)
    (scope.Resolve.types @ scope.Resolve.funs @ scope.Resolve.vars)
  |> List.sort_uniq String.compare

let offered_names (completion : Complete.completion) =
  List.filter_map
    (fun (item : Complete.item) ->
      match item.Complete.kind with
      | Complete.Keyword -> None
      | _ -> Some item.Complete.label)
    completion.Complete.items
  |> List.sort_uniq String.compare

(* Every label a completion offers, sorted. After a hash every candidate is an
   intrinsic, so no kind is filtered out. *)
let offered_labels (completion : Complete.completion) =
  List.map
    (fun (item : Complete.item) -> item.Complete.label)
    completion.Complete.items
  |> List.sort_uniq String.compare

(* The names of a compiler's list that a completion with [prefix] keeps. *)
let expected_intrinsics ~(prefix : string) (names : string list) =
  List.filter (String.starts_with ~prefix) names
  |> List.sort_uniq String.compare

(* The architecture a name of the lists stands for. The lists are read for
   [Jide_jasmin.Intrinsics.arch_names], which are the names the library's own
   architecture table parses. *)
let arch_of_name (name : string) =
  match Jide_jasmin.Arch.of_string name with
  | Some arch -> arch
  | None -> invalid_arg ("completion_scope: unknown architecture " ^ name)

(* The architecture name completion is asked under when the question is about
   names alone: the engine's default, which the names do not depend on. *)
let names_arch = Jide_jasmin.Arch.X86_64

type answer = {
  arch : string;  (** the architecture the answer is for *)
  offered : string list;  (** what completion showed, sorted *)
  expected : string list;
      (** the compiler's list, filtered by the same prefix *)
  agrees : bool;
}

type outcome =
  | Annotation
      (** the byte follows a hash and an opening bracket: the annotation keys,
          not a name position *)
  | Intrinsic of { prefix : string; answers : answer list }
      (** the byte follows a hash: one answer per architecture *)
  | Outside  (** the byte is not inside any function the file declares *)
  | Unreached  (** the resolver never reached the file: there is no scope *)
  | Compared of {
      prefix : string;  (** the word being typed, as completion read it *)
      offered : string list;  (** what completion showed, keywords removed *)
      expected : string list;  (** the scope, filtered by the same prefix *)
      equal : bool;
    }

(* [examine ~index ~resolve ~file ~spans ~lists ~byte] is the answer at one
   byte. [lists] pairs each architecture with the compiler's list for it. It is
   the whole of the comparison, so the self-test can ask it the same question
   the run asks and look at the answer. *)
let examine ~(index : Index.t) ~(resolve : Resolve.t) ~(file : string)
    ~(spans : Jide_lang.Span.t list) ~(lists : (string * string list) list)
    ~(byte : int) =
  let text = index.Index.text in
  let completion =
    Complete.completion ~arch:names_arch ~index ~resolve ~byte ()
  in
  let start = completion.Complete.span.Jide_lang.Span.start_byte in
  let prefix = completion.Complete.prefix in
  if start >= 1 && text.[start - 1] = '#' then
    Intrinsic
      {
        prefix;
        answers =
          List.map
            (fun (arch, names) ->
              let offered =
                offered_labels
                  (Complete.completion ~arch:(arch_of_name arch) ~index ~resolve
                     ~byte ())
              in
              let expected = expected_intrinsics ~prefix names in
              { arch; offered; expected; agrees = offered = expected })
            lists;
      }
  else if start >= 2 && text.[start - 1] = '[' && text.[start - 2] = '#' then
    Annotation
  else if not (inside spans byte) then Outside
  else
    match Resolve.scope_at resolve ~file ~byte with
    | None -> Unreached
    | Some scope ->
        let offered = offered_names completion in
        let expected =
          List.filter (String.starts_with ~prefix) (scope_names scope)
        in
        Compared { prefix; offered; expected; equal = offered = expected }

(* The negative control of the intrinsic comparison, on a document whose answer
   is known: a hash at the end of a body, with no prefix. Each architecture must
   answer with its own list (the positive half), and no other architecture may
   answer with the list of one that differs from it (the negative half). The
   second half is what stops the comparison passing for every architecture at
   once when completion ignores the architecture. *)
let intrinsic_control (lists : (string * string list) list) =
  let text = "fn f() {\n  reg u64 x;\n  x = #" in
  let file = "/control.jazz" in
  let index, _diagnostics = Index.of_text ~file text in
  let resolve = Resolve.of_text ~idirs:[] ~read:(fun _ -> None) ~file text in
  let byte = String.length text in
  let answer arch =
    offered_labels
      (Complete.completion ~arch:(arch_of_name arch) ~index ~resolve ~byte ())
  in
  let own (arch, names) = answer arch = expected_intrinsics ~prefix:"" names in
  let differs (arch, _) (other, names) =
    String.equal arch other
    || answer arch <> expected_intrinsics ~prefix:"" names
  in
  lists <> [] && List.for_all own lists
  && List.for_all (fun left -> List.for_all (differs left) lists) lists

(* ------------------------------------------------------------------ *)
(* Finding the corpus *)

let read_file (path : string) =
  In_channel.with_open_bin path (fun channel -> In_channel.input_all channel)

(* Every .jazz and .jinc file under [directory], in a fixed order: the walk is
   depth first with the names of each directory sorted, so the corpus's order
   never depends on the file system's. *)
let corpus_files (directory : string) =
  let rec walk acc path =
    match Sys.is_directory path with
    | true ->
        let names =
          Sys.readdir path |> Array.to_list |> List.sort String.compare
        in
        List.fold_left
          (fun acc name -> walk acc (Filename.concat path name))
          acc names
    | false -> (
        match Filename.extension path with
        | ".jazz" | ".jinc" -> path :: acc
        | _ -> acc)
    | exception Sys_error _ -> acc
  in
  List.rev (walk [] directory)

(* ------------------------------------------------------------------ *)
(* One run *)

type tally = {
  mutable files : int;
  mutable no_index : int;
  mutable positions : int;
  mutable equal : int;
  mutable different : int;
  mutable annotation : int;
  mutable outside : int;
  mutable unreached : int;
  mutable nonempty : int;
  mutable intrinsic_positions : int;
  mutable intrinsic_equal : int;
  mutable intrinsic_different : int;
}

let print_set names = "[" ^ String.concat ";" names ^ "]"

let run ~(compiler : string) ~(corpora : string list) ~(step : int)
    ~(seed : int) ~(limit : int) ~(quiet : bool) =
  let files = List.concat_map corpus_files corpora in
  if files = [] then (
    Printf.eprintf "completion_scope: no .jazz or .jinc file under %s\n%!"
      (String.concat " " corpora);
    exit 2);
  let lists =
    try
      List.map
        (fun arch -> (arch, Compiler_help.names_of ~compiler ~arch))
        Jide_jasmin.Intrinsics.arch_names
    with Compiler_help.Unparsable message ->
      Printf.eprintf "completion_scope: %s\n%!" message;
      exit 2
  in
  let control = intrinsic_control lists in
  let tally =
    {
      files = 0;
      no_index = 0;
      positions = 0;
      equal = 0;
      different = 0;
      annotation = 0;
      outside = 0;
      unreached = 0;
      nonempty = 0;
      intrinsic_positions = 0;
      intrinsic_equal = 0;
      intrinsic_different = 0;
    }
  in
  let read path = try Some (read_file path) with Sys_error _ -> None in
  List.iter
    (fun path ->
      let file = Index.normalise path in
      match read file with
      | None -> ()
      | Some text ->
          tally.files <- tally.files + 1;
          let resolve = Resolve.of_text ~idirs:[] ~read ~file text in
          let index =
            match Resolve.file resolve file with
            | Some index -> index
            | None -> fst (Index.of_text ~file text)
          in
          if index.Index.occurrences = [] then
            tally.no_index <- tally.no_index + 1;
          let spans = function_spans index in
          let bytes =
            List.sort_uniq Int.compare
              (sample ~step ~seed ~length:(String.length text) ~file
              @ after_hash_bytes text)
          in
          List.iter
            (fun byte ->
              match examine ~index ~resolve ~file ~spans ~lists ~byte with
              | Annotation -> tally.annotation <- tally.annotation + 1
              | Outside -> tally.outside <- tally.outside + 1
              | Unreached ->
                  tally.unreached <- tally.unreached + 1;
                  Printf.printf
                    "unreached %s:%d (the resolver never reached this file)\n%!"
                    file byte
              | Intrinsic { prefix; answers } ->
                  tally.intrinsic_positions <- tally.intrinsic_positions + 1;
                  List.iter
                    (fun answer ->
                      if answer.agrees then
                        tally.intrinsic_equal <- tally.intrinsic_equal + 1
                      else (
                        tally.intrinsic_different <-
                          tally.intrinsic_different + 1;
                        Printf.printf
                          "different-intrinsic %s:%d arch=%s prefix=%S \
                           offered=%s expected=%s\n\
                           %!"
                          file byte answer.arch prefix
                          (print_set answer.offered)
                          (print_set answer.expected)))
                    answers
              | Compared { prefix; offered; expected; equal } ->
                  if tally.positions < limit then (
                    tally.positions <- tally.positions + 1;
                    if expected <> [] then tally.nonempty <- tally.nonempty + 1;
                    if equal then tally.equal <- tally.equal + 1
                    else (
                      tally.different <- tally.different + 1;
                      Printf.printf
                        "different %s:%d prefix=%S offered=%s expected=%s\n%!"
                        file byte prefix (print_set offered)
                        (print_set expected))))
            bytes)
    files;
  let clean =
    tally.different = 0 && tally.unreached = 0
    && tally.intrinsic_positions > 0
    && tally.intrinsic_different = 0
    && control
  in
  if not quiet then (
    Printf.printf
      "completion_scope: %d files (%d with no index), %d positions compared, \
       %d equal, %d different, %d unreached; %d of the compared have a \
       non-empty scope; %d skipped after #[, %d outside a function; step %d, \
       seed %d, limit %d\n\
       %!"
      tally.files tally.no_index tally.positions tally.equal tally.different
      tally.unreached tally.nonempty tally.annotation tally.outside step seed
      limit;
    Printf.printf
      "completion_scope: %d positions after #, each answered for %d \
       architectures: %d answers equal, %d different\n\
       %!"
      tally.intrinsic_positions (List.length lists) tally.intrinsic_equal
      tally.intrinsic_different;
    Printf.printf
      "completion_scope: negative control (each architecture answers its own \
       list and no other's): %s\n\
       %!"
      (if control then "holds" else "FAILS");
    if clean then
      Printf.printf
        "completion_scope: the names completion offers are the resolver's \
         scope at every sampled byte, and after # it offers the compiler's \
         list for every architecture\n\
         %!"
    else
      Printf.printf
        "completion_scope: %d sampled byte(s) disagree with the resolver's \
         scope, %d are unreached, %d intrinsic answer(s) disagree with the \
         compiler\n\
         %!"
        tally.different tally.unreached tally.intrinsic_different);
  if clean then 0 else 1

(* ------------------------------------------------------------------ *)
(* The self-test *)

(* The self-test is the plumbing check `dune runtest` can make without a
   corpus or a compiler: the sample is deterministic, the comparison answers
   what it should on a program whose scope is known, the branch that reports
   a difference is reached when the two sides do differ, and the intrinsic
   answer is compared with the list it is given. It cannot prove the equality
   over a corpus; only a run does. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let selftest () =
  (* The sample: a fixed length, step and seed name a fixed list. The first
     byte is the formula's own output for this file name, pinned so that a
     change to the formula or to the hash fails here. *)
  check "sample: the pinned walk for a.jazz"
    (sample ~step:5 ~seed:1 ~length:20 ~file:"a.jazz" = [ 0; 5; 10; 15 ]);
  check "sample: the pinned first bytes" (pinned_first_bytes first_byte);
  check
    "negative control: the pinned first bytes reject a formula without the seed"
    (not
       (pinned_first_bytes (fun ~step ~seed:_ ~file ->
            first_byte ~step ~seed:0 ~file)));
  check
    "negative control: the pinned first bytes reject a formula without the hash"
    (not
       (pinned_first_bytes (fun ~step ~seed ~file:_ ->
            first_byte ~step ~seed ~file:"")));
  check "sample: the first byte is inside one step"
    (first_byte ~step:5 ~seed:1 ~file:"a.jazz" >= 0
    && first_byte ~step:5 ~seed:1 ~file:"a.jazz" < 5);
  check "sample: empty file" (sample ~step:5 ~seed:1 ~length:0 ~file:"a" = []);
  check "sample: step is clamped to one"
    (sample ~step:0 ~seed:1 ~length:3 ~file:"a" = [ 0; 1; 2 ]);
  check "sample: the same question twice"
    (sample ~step:7 ~seed:3 ~length:100 ~file:"dir/b.jazz"
    = sample ~step:7 ~seed:3 ~length:100 ~file:"dir/b.jazz");
  check "hash: every byte after a hash is listed"
    (after_hash_bytes "a#b#" = [ 2; 4 ]);

  (* The document the comparison is asked about: a parameter, a local and a
     function declared after the body that uses it, so the scope changes
     between positions. *)
  let text =
    String.concat "\n"
      [
        "fn twice(reg u64 v) -> reg u64 {";
        "  reg u64 r;";
        "  r = v;";
        "  return r;";
        "}";
        "";
        "fn caller(reg u64 x) -> reg u64 {";
        "  reg u64 y;";
        "  y = twice(x);";
        "  return y;";
        "}";
        "";
        "// after the last function";
        "";
      ]
  in
  let file = "/selftest.jazz" in
  let index, _diagnostics = Index.of_text ~file text in
  let resolve = Resolve.of_text ~idirs:[] ~read:(fun _ -> None) ~file text in
  let spans = function_spans index in
  check "selftest: two functions" (List.length spans = 2);
  let at fragment =
    let rec from index =
      if index + String.length fragment > String.length text then -1
      else if
        String.equal (String.sub text index (String.length fragment)) fragment
      then index
      else from (index + 1)
    in
    from 0
  in
  check "selftest: the header byte is inside its function"
    (inside spans (at "fn twice"));
  check "selftest: the second header byte is inside its function"
    (inside spans (at "fn caller"));
  check "selftest: a comment after the last function is outside"
    (inside spans (at "// after") = false);
  (* The local [r] is declared before this position and [v] is the parameter;
     [twice] is not in scope inside its own body, because pretyping pushes a
     function after its body, which is the rule the resolver implements. *)
  let position = at "= v;" + 2 in
  (match examine ~index ~resolve ~file ~spans ~lists:[] ~byte:position with
  | Compared { offered; expected; equal; _ } ->
      check "selftest: the local and the parameter are offered"
        (List.mem "r" offered && List.mem "v" offered);
      check "selftest: a function is not in scope in its own body"
        (not (List.mem "twice" offered));
      check "selftest: completion equals the scope" (equal && offered = expected)
  | Annotation | Intrinsic _ | Outside | Unreached ->
      check "selftest: the position is compared" false);
  (* The branch that reports a difference: the same position, with one name
     taken from the offered side, must not be equal. This is also the
     negative control of the comparison itself. *)
  (match examine ~index ~resolve ~file ~spans ~lists:[] ~byte:position with
  | Compared { offered; expected; _ } ->
      check "selftest: a missing name is a difference"
        (not (List.filter (fun name -> name <> "r") offered = expected))
  | Annotation | Intrinsic _ | Outside | Unreached ->
      check "selftest: the position is compared" false);
  (* Inside a call's parentheses, in the second function, [twice] is in scope
     and [caller] is not. *)
  let call = at "= twice(x);" + 2 in
  (match examine ~index ~resolve ~file ~spans ~lists:[] ~byte:call with
  | Compared { offered; expected; equal; _ } ->
      check "selftest: a declared function is offered"
        (List.mem "twice" offered);
      check "selftest: the enclosing function is not offered"
        (not (List.mem "caller" offered));
      check "selftest: the call position equals the scope"
        (equal && offered = expected)
  | Annotation | Intrinsic _ | Outside | Unreached ->
      check "selftest: the call position is compared" false);
  (* A byte after a hash and an opening bracket is an annotation position,
     counted apart. The text need not be a valid annotation: completion's rule
     is the characters, and the comment keeps the program itself parseable. *)
  let annotated =
    String.concat "\n"
      [
        "fn f(reg u64 x) -> reg u64 {";
        "  reg u64 y;";
        "  y = x; // #[ct]";
        "  return y;";
        "}";
        "";
      ]
  in
  let file = "/annotated.jazz" in
  let index, _diagnostics = Index.of_text ~file annotated in
  let resolve =
    Resolve.of_text ~idirs:[] ~read:(fun _ -> None) ~file annotated
  in
  let spans = function_spans index in
  let after_bracket =
    match String.index_opt annotated '[' with
    | None -> 0
    | Some bracket -> bracket + 3
  in
  check "selftest: a byte after # is an annotation position"
    (match examine ~index ~resolve ~file ~spans ~byte:after_hash with
    | Annotation -> true
    | _ -> false);
  (* A file the resolver never read has no scope. Its position must come back
     unreached and never compared, because an empty scope on both sides would
     agree by default. The position is the operand after the first [=] of the
     annotated document, which is inside [f]. *)
  let operand =
    match String.index_opt annotated '=' with Some at -> at + 2 | None -> 0
  in
  check "negative control: a file the resolver never read is unreached"
    (match
       examine ~index ~resolve ~file:"/never-read.jazz" ~spans ~byte:operand
     with
    | Unreached -> true
    | _ -> false);
  if !failures = 0 then Printf.printf "completion_scope: selftest passed\n%!"
  else (
    Printf.printf "completion_scope: %d selftest failure(s)\n%!" !failures;
    exit 1)

(* ------------------------------------------------------------------ *)
(* The command line *)

let () =
  let corpora = ref []
  and step = ref 13
  and seed = ref 1
  and limit = ref 20000 in
  let quiet = ref false and selftest_requested = ref false in
  let spec =
    [
      ( "--corpus",
        Arg.String (fun directory -> corpora := directory :: !corpora),
        "a corpus directory to sample" );
      ("--step", Arg.Set_int step, "sample every Nth byte (default 13)");
      ("--seed", Arg.Set_int seed, "the seed folded into the sample (default 1)");
      ( "--limit",
        Arg.Set_int limit,
        "the most positions to compare (default 20000)" );
      ("--quiet", Arg.Set quiet, "print only the differences");
      ( "--selftest",
        Arg.Set selftest_requested,
        "check this program's own plumbing" );
    ]
  in
  let anonymous argument =
    raise (Arg.Bad ("unexpected argument: " ^ argument))
  in
  Arg.parse spec anonymous usage;
  if !selftest_requested then selftest ()
  else
    let corpora = List.rev !corpora in
    if corpora = [] then (
      prerr_endline usage;
      exit 2);
    exit (run ~corpora ~step:!step ~seed:!seed ~limit:!limit ~quiet:!quiet)
