(** One reference per typed call.

    The requirement is one sentence:

    {v
      Each typed call in the corpus has exactly one reference to its callee
      inside that instruction's range.
    v}

    This program measures that requirement and fails when it does not hold.
    For every file of a corpus it builds what the server builds for an open
    document: the compiler's check ({!Jide_jasmin.Adapter.check}), the typed
    facts it leaves, and the index and the resolver over the same bytes
    ({!Jide_engine.Locate.of_text}). It then asks the engine's own references
    question ({!Jide_engine.Locate.references_at}) at the byte each typed call
    writes its callee at, and counts the answers whose range falls inside the
    instruction that makes the call.

    The question is asked in the file the call's bytes belong to, which is the
    checked file for everything the file writes itself and the required file
    for a call that a [.jinc] writes: the parser splices a required file's
    items into the requiring one and keeps their ranges in the required file's
    own bytes, so the facts carry the file with the ranges
    ({!Jide_jasmin.Facts.call}). Asking in the checked file would read the
    wrong text for such a call.

    - [one]: exactly one reference falls inside the instruction, and it is the
      callee's own name there. This is the requirement.
    - [none]: the instruction holds no reference to the callee. The resolver
      did not resolve the callee, or the index recorded no occurrence where the
      syntax writes it, or both.
    - [more than one]: the instruction holds two references or more, which is
      one instruction naming its callee twice.
    - [not the callee]: one reference falls inside the instruction but the
      declaration it names is not the callee (another name, or not a function),
      or the one reference is a different span. The requirement asks for a
      reference to the callee, so this is a difference too. The names are
      compared unqualified, [A::id] against [id], because that is how the
      index keeps a declaration's name.
    - [not located]: the facts hold the call but no range for its callee's
      name, so there is no reference to ask about. This is a difference too.
      A call that the harness dropped would be neither judged nor counted, so
      the facts keep every call the compiler typed and this verdict reports
      the ones the syntax cannot place.

    The calls come from the typed facts ({!Jide_jasmin.Facts.call}): the
    compiler typed each of them, and the facts carry the instruction's range
    and the range the callee's name was written at. The facts hold every call
    the compiler typed in the items it reached, including the items typed
    before a diagnostic, so a call the facts do not hold is not a typed call.
    The run counts the files with no typed call apart, so the gap is visible.

    Usage:

    {v
      calls --corpus <dir> [--limit N]   measure the requirement over a corpus
      calls --selftest                   check this program against small sources
      calls --help                       print this text
    v}

    <dir> is the `compiler/tests` directory of a Jasmin release, the `compiler`
    directory that holds it, or the release root that holds that. Every
    `.jazz` file under it is read, so no file is skipped by a folder rule. The
    architecture a file is checked for is the deepest directory of its path
    that names one, `common` meaning the compiler's default (x86-64), as
    `test/parity/parity.sh` reads the same corpus. Nothing here needs a
    network, a compiler binary or a second process.

    Stdout carries one line per count and one line per difference; the exit
    status is 0 when every typed call has exactly one reference inside its
    instruction, 1 when one does not, and 2 when the command line is wrong or
    the corpus names no file. *)

module Facts = Jide_jasmin.Facts
module Index = Jide_engine.Index
module Locate = Jide_engine.Locate
module Span = Jide_lang.Span
module Types = Jide_engine.Types

let usage =
  String.concat "\n"
    [
      "usage: calls --corpus <dir> [--limit N]";
      "       calls --selftest";
      "       calls --help";
      "";
      "  --corpus <dir>  a Jasmin release's compiler/tests, compiler or root";
      "  --limit N       read at most N files (for a quick run)";
      "  --selftest      check the counting against small sources";
      "";
      "See the comment at the top of test/parity/calls.ml for the requirement";
      "it measures and what each count means.";
    ]

(* ------------------------------------------------------------------ *)
(* Reading files and choosing an architecture *)

let read (path : string) : string option =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception _ -> None

let default_arch = "x86-64"

(* The compiler's default architecture, and the one a `common` folder means:
   the release checks a `common` file on every architecture it has, and x86-64
   is the one the compiler itself defaults to. *)
let arch_of_path (path : string) : Jide_jasmin.Adapter.arch =
  let rec deepest = function
    | [] -> None
    | part :: rest -> (
        match Jide_jasmin.Adapter.arch_of_string part with
        | Some arch -> Some arch
        | None -> deepest rest)
  in
  match deepest (String.split_on_char '/' path) with
  | Some arch -> arch
  | None -> Option.get (Jide_jasmin.Adapter.arch_of_string default_arch)

(* ------------------------------------------------------------------ *)
(* Judging one call *)

(* The verdict of one call, and what it was read from. The two go together
   because a difference is only useful with the references that were found. *)
type verdict = One | None_found | Many | Not_the_callee | Unlocated

type judgement = {
  call : Facts.call;
  declaration : Index.declaration option;
  inside : Types.location list;
  verdict : verdict;
}

let same_file (left : string) (right : string) =
  String.equal (Index.normalise left) (Index.normalise right)

(* [inside range span] is true when [span] lies inside [range], both ends
   included as the ranges are written: a range that ends where the
   instruction's does is inside it. *)
let inside (range : Span.t) (span : Span.t) =
  span.Span.start_byte >= range.Span.start_byte
  && span.Span.end_byte <= range.Span.end_byte

(* [unqualified name] is the last [::]-separated part of a name: the name a
   declaration carries. The compiler writes a call's callee as the source
   qualified it, [A::id], and the index keeps a declaration's [name] as the
   name written, [id], so the two agree on the last part and nowhere else.
   The qualification itself is not thrown away: the occurrence the question
   asks about is the qualified one, and the engine resolves all of it. *)
let unqualified (name : string) =
  match String.rindex_opt name ':' with
  | Some index -> String.sub name (index + 1) (String.length name - index - 1)
  | None -> name

(* [judge snapshot call] is the requirement's answer for one call: the
   declaration the callee's name resolves to, the references that fall inside
   the instruction that makes the call, and which of the verdicts that is.

   The question is asked in the file the call's bytes belong to, [c_file], and
   not in the file that was checked: a call a required [.jinc] writes has
   ranges in that file's bytes, and asking in the checked file would read
   another text altogether. The two are the same file for every call the
   checked file writes itself, which is nearly all of them.

   A declaration is the callee only when it is a function with the callee's
   name: a call to [add] that resolves to a variable called [add] would
   otherwise count as one reference and pass, which is the opposite of what
   the requirement asks. The index keeps a declaration's name as the source
   wrote it: [inline fn ble::e] keeps [ble::e], and a function written [id]
   inside a namespace keeps [id]. So the declaration is the callee when its
   name is the callee's name as the compiler typed it, or that name's last part:
   a call [ble::e] resolves to [ble::e] and to [e], and [A::id] to [id].

   A call with no name range has no byte to ask at, so it is [Unlocated] and
   nothing is asked. *)
let judge (snapshot : Locate.t) (call : Facts.call) : judgement =
  match call.Facts.c_span with
  | None -> { call; declaration = None; inside = []; verdict = Unlocated }
  | Some name ->
      let byte = name.Span.start_byte in
      let written_in = call.Facts.c_file in
      let declaration = Locate.declaration_at snapshot ~file:written_in ~byte in
      let answers =
        Locate.references_at snapshot ~file:written_in ~byte
          ~include_declaration:false
      in
      let inside =
        List.filter
          (fun (answer : Types.location) ->
            same_file answer.Types.file written_in
            && inside call.Facts.c_range answer.Types.span)
          answers
      in
      let names_the_callee =
        match declaration with
        | Some declaration ->
            (String.equal declaration.Index.name call.Facts.c_callee
            || String.equal declaration.Index.name
                 (unqualified call.Facts.c_callee))
            && declaration.Index.kind = Index.Function
        | None -> false
      in
      let verdict =
        if not names_the_callee then
          match declaration with None -> None_found | Some _ -> Not_the_callee
        else
          match inside with
          | [] -> None_found
          | [ answer ] ->
              if Span.compare answer.Types.span name = 0 then One
              else Not_the_callee
          | _ -> Many
      in
      { call; declaration; inside; verdict }

(* ------------------------------------------------------------------ *)
(* One checked file *)

type result = {
  typed : bool;  (** the compiler typed at least one call in the file *)
  diagnostics : int;
  judgements : judgement list;
}

(* [check ~file text] is what the server would hold for that document: the
   typed facts of the compiler's check, and the resolution of the same bytes.
   The snapshot is built only when the file makes a call, because nothing
   else asks the engine a question. *)
let check ~(file : string) (text : string) : result =
  let checked =
    Jide_jasmin.Adapter.check (arch_of_path file)
      (Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux"))
      ~file ~text ~idirs:[]
  in
  let facts = checked.Jide_jasmin.Adapter.facts in
  let calls = facts.Facts.calls in
  let judgements =
    match calls with
    | [] -> []
    | calls ->
        let snapshot =
          Locate.of_text ~facts:[ (file, facts) ] ~idirs:[] ~read ~file text
        in
        List.map (judge snapshot) calls
  in
  {
    typed = calls <> [];
    diagnostics = List.length checked.Jide_jasmin.Adapter.diagnostics;
    judgements;
  }

(* ------------------------------------------------------------------ *)
(* A corpus *)

(* The corpus root the caller named, as the `compiler/tests` directory the
   release keeps its corpus in: a root that holds it is read through it, so a
   release root and its `compiler/tests` are the same corpus. *)
let corpus_root (path : string) : string =
  let nested = Filename.concat (Filename.concat path "compiler") "tests" in
  if Sys.file_exists nested && Sys.is_directory nested then nested else path

(* Every `.jazz` file under [root], in a stable order. *)
let rec files_under (root : string) : string list =
  match Sys.readdir root with
  | entries ->
      let entries = Array.to_list entries |> List.sort String.compare in
      List.concat_map
        (fun entry ->
          let path = Filename.concat root entry in
          if Sys.is_directory path then files_under path
          else if Filename.check_suffix entry ".jazz" then [ path ]
          else [])
        entries
  | exception Sys_error _ -> []

(* ------------------------------------------------------------------ *)
(* Printing *)

let position_of (lines : Jide_lang.Line_index.t) (byte : int) =
  let position = Jide_lang.Line_index.position_of_byte lines byte in
  Printf.sprintf "%d:%d"
    (position.Jide_lang.Position.line + 1)
    (position.Jide_lang.Position.character + 1)

let range_of (lines : Jide_lang.Line_index.t) (span : Span.t) =
  Printf.sprintf "%s-%s"
    (position_of lines span.Span.start_byte)
    (position_of lines span.Span.end_byte)

let verdict_name = function
  | One -> "one"
  | None_found -> "none"
  | Many -> "more than one"
  | Not_the_callee -> "not the callee"
  | Unlocated -> "not located"

let declaration_name (declaration : Index.declaration) =
  Printf.sprintf "%s at %s" declaration.Index.name
    (Filename.basename declaration.Index.file)

(* One difference, on one line: the call, the callee, the declaration the
   callee resolved to, and every reference that fell inside the instruction.
   The line starts with the verdict so a list of them can be counted with
   `grep -c '^none'` as well as read. *)
let describe (lines : Jide_lang.Line_index.t) (judgement : judgement) =
  let call = judgement.call in
  match call.Facts.c_span with
  | None ->
      Printf.sprintf
        "%s: call %s in instruction %s: the source holds no range for its \
         callee's name"
        (verdict_name judgement.verdict)
        call.Facts.c_callee
        (range_of lines call.Facts.c_range)
  | Some name ->
      let references =
        match judgement.inside with
        | [] -> "none"
        | references ->
            String.concat ", "
              (List.map
                 (fun (answer : Types.location) ->
                   Printf.sprintf "%s:%s"
                     (Filename.basename answer.Types.file)
                     (range_of lines answer.Types.span))
                 references)
      in
      let declaration =
        match judgement.declaration with
        | Some declaration -> declaration_name declaration
        | None -> "unresolved"
      in
      Printf.sprintf
        "%s: call %s in instruction %s: %s resolves to %s; inside: %s"
        (verdict_name judgement.verdict)
        call.Facts.c_callee
        (range_of lines call.Facts.c_range)
        (position_of lines name.Span.start_byte)
        declaration references

(* ------------------------------------------------------------------ *)
(* Measuring a corpus *)

type tally = {
  mutable files : int;
  mutable typed : int;
  mutable with_diagnostics : int;
  mutable calls : int;
  mutable one : int;
  mutable none : int;
  mutable many : int;
  mutable not_the_callee : int;
  mutable unlocated : int;
}

let empty_tally () =
  {
    files = 0;
    typed = 0;
    with_diagnostics = 0;
    calls = 0;
    one = 0;
    none = 0;
    many = 0;
    not_the_callee = 0;
    unlocated = 0;
  }

let count (tally : tally) (result : result) =
  tally.files <- tally.files + 1;
  if result.typed then tally.typed <- tally.typed + 1;
  if result.diagnostics > 0 then
    tally.with_diagnostics <- tally.with_diagnostics + 1;
  List.iter
    (fun (judgement : judgement) ->
      tally.calls <- tally.calls + 1;
      match judgement.verdict with
      | One -> tally.one <- tally.one + 1
      | None_found -> tally.none <- tally.none + 1
      | Many -> tally.many <- tally.many + 1
      | Not_the_callee -> tally.not_the_callee <- tally.not_the_callee + 1
      | Unlocated -> tally.unlocated <- tally.unlocated + 1)
    result.judgements

let measure ~(root : string) ~(limit : int option) :
    tally * (string * judgement) list * int =
  let files =
    match limit with
    | None -> files_under root
    | Some limit ->
        let rec take count = function
          | [] -> []
          | _ when count >= limit -> []
          | file :: rest -> file :: take (count + 1) rest
        in
        take 0 (files_under root)
  in
  let tally = empty_tally () in
  let differences = ref [] in
  List.iter
    (fun file ->
      match read file with
      | None -> ()
      | Some text ->
          let result = check ~file text in
          count tally result;
          List.iter
            (fun (judgement : judgement) ->
              if judgement.verdict <> One then
                differences := (file, judgement) :: !differences)
            result.judgements)
    files;
  (tally, List.rev !differences, List.length files)

(* The path as the corpus names it: a corpus is read relative to its root
   where it can be, because a difference is looked up by the file name the
   report prints and two folders may hold one name. *)
let relative ~(root : string) (path : string) =
  let prefix = if Filename.check_suffix root "/" then root else root ^ "/" in
  if
    String.length path > String.length prefix
    && String.sub path 0 (String.length prefix) = prefix
  then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

let report ~(root : string) ~(files : int) (tally : tally)
    (differences : (string * judgement) list) =
  Printf.printf "corpus: %s\n" root;
  Printf.printf "files: %d read, %d with a typed call, %d with a diagnostic\n"
    files tally.typed tally.with_diagnostics;
  Printf.printf "typed calls: %d\n" tally.calls;
  Printf.printf "exactly one reference inside the instruction: %d\n" tally.one;
  Printf.printf "none: %d\n" tally.none;
  Printf.printf "more than one: %d\n" tally.many;
  Printf.printf "not the callee: %d\n" tally.not_the_callee;
  Printf.printf "not located in the source: %d\n" tally.unlocated;
  List.iter
    (fun (file, judgement) ->
      match read file with
      | None -> ()
      | Some text ->
          let lines = Jide_lang.Line_index.create text in
          Printf.printf "%s: %s\n" (relative ~root file)
            (describe lines judgement))
    differences;
  flush stdout

(* ------------------------------------------------------------------ *)
(* The selftest *)

(* A program with one call, and a second one with two calls, one inside an
   [if] and one inside a [for], so the walk into nested statements is
   exercised as well as the count. *)
let one_call =
  {|
fn add(reg u64 a) -> reg u64 { return a; }

fn caller(reg u64 n) -> reg u64 {
  reg u64 s;
  s = 0;
  s = add(s);
  return s;
}
|}

let nested_calls =
  {|
fn add(reg u64 a) -> reg u64 { return a; }

fn caller(reg u64 n) -> reg u64 {
  reg u64 s;
  reg u64 i;
  s = 0;
  if n > 0 {
    s = add(s);
  }
  for i = 0 to n {
    s = add(s);
  }
  return s;
}
|}

(* A call in a declaration's initialiser is a call as much as one in an
   assignment: [reg u64 t = add(n);] makes one. The second program has two
   initialisers in one declaration. They share the declaration's instruction
   range, and each keeps the name it was written with. *)
let declared_call =
  {|
fn add(reg u64 a) -> reg u64 { return a; }

fn caller(reg u64 n) -> reg u64 {
  reg u64 t = add(n);
  return t;
}
|}

let two_declared_calls =
  {|
fn add(reg u64 a) -> reg u64 { return a; }
fn dbl(reg u64 a) -> reg u64 { return a; }

fn caller(reg u64 n) -> reg u64, reg u64 {
  reg u64 t = add(n), u = dbl(n);
  return t, u;
}
|}

(* A function declared with a qualified name and called by that name. The
   index keeps [ble::e] as the declaration's name, so the callee must match
   whole, and the last part alone is not what decides it. *)
let qualified_declaration =
  {|
inline fn ble::e () -> reg u32 {
  reg u32 x = 0;
  return x;
}

export fn caller() -> reg u32 {
  reg u32 x = ble::e();
  return x;
}
|}

let index_of text needle =
  let width = String.length needle in
  let rec loop index =
    if index + width > String.length text then None
    else if String.sub text index width = needle then Some index
    else loop (index + 1)
  in
  loop 0

let span_in text ?(offset = 0) ?length needle =
  let start_byte =
    match index_of text needle with Some index -> index + offset | None -> 0
  in
  let width =
    match length with
    | Some width -> width
    | None -> String.length needle - offset
  in
  Span.make ~start_byte ~end_byte:(start_byte + width)

(* [named_at call check] is [check] applied to the range of the callee's name,
   and false when the call has no such range. *)
let named_at (call : Facts.call) (check : Span.t -> bool) : bool =
  match call.Facts.c_span with Some name -> check name | None -> false

let check_selftest () =
  let failures = ref 0 in
  let expect name condition =
    if not condition then (
      incr failures;
      Printf.eprintf "selftest: %s: failed\n" name)
  in
  let file = "/selftest/calls.jazz" in
  (* The facts and the resolver snapshot of [text], as the server holds them. *)
  let snapshot_of text =
    let facts =
      (Jide_jasmin.Adapter.check
         (arch_of_path default_arch)
         (Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux"))
         ~file ~text ~idirs:[])
        .Jide_jasmin.Adapter.facts
    in
    (facts, Locate.of_text ~facts:[ (file, facts) ] ~idirs:[] ~read ~file text)
  in
  expect "a qualified callee is compared by its last part"
    (String.equal (unqualified "A::id") "id");
  expect "an unqualified callee is compared whole"
    (String.equal (unqualified "add") "add");
  let result = check ~file one_call in
  expect "one call: the facts hold one call" (List.length result.judgements = 1);
  (match result.judgements with
  | [ judgement ] ->
      let call = judgement.call in
      expect "one call: the callee is add"
        (String.equal call.Facts.c_callee "add");
      expect "one call: the call's bytes are in the checked file"
        (String.equal call.Facts.c_file file);
      expect "one call: the callee's name is where the source writes it"
        (named_at call (fun name ->
             Span.compare name
               (span_in one_call "s = add(s);" ~offset:4 ~length:3)
             = 0));
      expect "one call: the instruction's range is the whole statement"
        (Span.compare call.Facts.c_range (span_in one_call "s = add(s);") = 0);
      expect "one call: the callee's name lies inside the instruction"
        (named_at call (inside call.Facts.c_range));
      expect "one call: the instruction lies inside the item"
        (inside call.Facts.c_item call.Facts.c_range);
      expect "one call: the requirement holds" (judgement.verdict = One)
  | _ -> ());
  let nested = check ~file nested_calls in
  expect "nested calls: the facts hold two calls"
    (List.length nested.judgements = 2);
  expect "nested calls: both hold the requirement"
    (List.for_all
       (fun (judgement : judgement) -> judgement.verdict = One)
       nested.judgements);
  (* A call in a declaration's initialiser is judged like any other. With one
     initialiser, the call is the only one; with two, each keeps the callee it
     was written with, and the second is not the first's name. *)
  let declared = check ~file declared_call in
  expect "a declaration's initialiser: the facts hold its call"
    (List.length declared.judgements = 1);
  (match declared.judgements with
  | [ judgement ] ->
      expect "a declaration's initialiser: the callee is add"
        (String.equal judgement.call.Facts.c_callee "add");
      expect
        "a declaration's initialiser: the callee's name is where it is written"
        (named_at judgement.call (fun name ->
             Span.compare name (span_in declared_call "add(n);" ~length:3) = 0));
      expect "a declaration's initialiser: the instruction is the declaration"
        (Span.compare judgement.call.Facts.c_range
           (span_in declared_call "reg u64 t = add(n);")
        = 0);
      expect "a declaration's initialiser: the requirement holds"
        (judgement.verdict = One)
  | _ -> ());
  let two = check ~file two_declared_calls in
  expect "two initialisers in one declaration: the facts hold both calls"
    (List.length two.judgements = 2);
  (match two.judgements with
  | [ first; second ] ->
      expect "two initialisers: the first callee is add"
        (String.equal first.call.Facts.c_callee "add");
      expect "two initialisers: the second callee is dbl"
        (String.equal second.call.Facts.c_callee "dbl");
      expect "two initialisers: the second's name is where dbl is written"
        (named_at second.call (fun name ->
             Span.compare name (span_in two_declared_calls "dbl(n);" ~length:3)
             = 0));
      expect "two initialisers: both share the declaration's range"
        (Span.compare first.call.Facts.c_range second.call.Facts.c_range = 0);
      expect "two initialisers: both hold the requirement"
        (first.verdict = One && second.verdict = One)
  | _ -> ());
  let qualified = check ~file qualified_declaration in
  expect "a qualified declaration: the facts hold its call"
    (List.length qualified.judgements = 1);
  (match qualified.judgements with
  | [ judgement ] ->
      expect "a qualified declaration: the callee is called by its full name"
        (String.equal judgement.call.Facts.c_callee "ble::e");
      expect "a qualified declaration: the requirement holds"
        (judgement.verdict = One)
  | _ -> ());
  (* Negative controls. A declaration's callee read at its variable's name is
     not the callee, and a call with no name range is a difference of its own,
     never a call that goes uncounted. *)
  let declared_facts, declared_snapshot = snapshot_of declared_call in
  (match declared_facts.Facts.calls with
  | call :: _ ->
      let at_variable =
        judge declared_snapshot
          {
            call with
            Facts.c_span =
              Some
                (span_in declared_call "reg u64 t = add(n);" ~offset:8 ~length:1);
          }
      in
      expect "a declaration's name read at its variable is not the callee"
        (at_variable.verdict = Not_the_callee)
  | [] -> expect "the declaration program has a call to judge" false);
  let nested_facts, nested_snapshot = snapshot_of nested_calls in
  let call =
    match nested_facts.Facts.calls with
    | call :: _ -> call
    | [] ->
        expect "the selftest program has a call to judge" false;
        {
          Facts.c_span = None;
          c_callee = "";
          c_item = Span.make ~start_byte:0 ~end_byte:0;
          c_range = Span.make ~start_byte:0 ~end_byte:0;
          c_file = file;
        }
  in
  let unlocated = judge nested_snapshot { call with Facts.c_span = None } in
  expect "a call the source gives no name range is not located"
    (unlocated.verdict = Unlocated);
  let text = nested_calls in
  let snapshot = nested_snapshot in
  let fabricated c_span c_range =
    { call with Facts.c_span = Some c_span; c_range }
  in
  (* One instruction range that covers both calls to [add] holds two
     references to it: more than one. *)
  let twice =
    judge snapshot
      (fabricated
         (span_in text "add(s);" ~offset:0 ~length:3)
         (Span.make ~start_byte:0 ~end_byte:(String.length text)))
  in
  expect "two calls to one callee inside one range are more than one"
    (twice.verdict = Many);
  (* The variable on the left of the assignment: the declaration it names is a
     variable, so it is not the callee however many references fall inside. *)
  let variable =
    judge snapshot
      (fabricated
         (span_in text "s = add(s);" ~offset:0 ~length:1)
         (span_in text "s = add(s);"))
  in
  expect "a name that is not the callee is a difference"
    (variable.verdict = Not_the_callee);
  (* Whitespace holds no occurrence at all, so the callee resolves to nothing
     and the instruction holds no reference: none. *)
  let nothing =
    judge snapshot
      (fabricated
         (span_in text "s = 0;" ~offset:5 ~length:1)
         (span_in text "s = add(s);"))
  in
  expect "a call whose callee resolves to nothing has none"
    (nothing.verdict = None_found);
  if !failures = 0 then print_endline "calls: selftest ok"
  else (
    Printf.eprintf "calls: %d selftest check(s) failed\n" !failures;
    exit 1)

(* ------------------------------------------------------------------ *)
(* The command line *)

(* [arguments] splits `--name value` pairs from the words that stand alone,
   and refuses anything it does not know: a misspelt flag must not become a
   silent default. *)
type options = { corpus : string option; limit : int option; selftest : bool }

let parse_arguments (arguments : string list) : options =
  let rec loop options = function
    | [] -> options
    | "--selftest" :: rest -> loop { options with selftest = true } rest
    | "--corpus" :: path :: rest ->
        loop { options with corpus = Some path } rest
    | "--limit" :: value :: rest ->
        let limit =
          match int_of_string_opt value with
          | Some limit when limit > 0 -> limit
          | _ ->
              prerr_endline
                ("calls: --limit takes a positive number, not " ^ value);
              exit 2
        in
        loop { options with limit = Some limit } rest
    | "--help" :: _ | "-help" :: _ | "-h" :: _ ->
        print_endline usage;
        exit 0
    | argument :: _ ->
        prerr_endline ("calls: unknown argument " ^ argument);
        prerr_endline usage;
        exit 2
  in
  loop { corpus = None; limit = None; selftest = false } arguments

let main () =
  let options = parse_arguments (List.tl (Array.to_list Sys.argv)) in
  if options.selftest then (
    check_selftest ();
    exit 0);
  match options.corpus with
  | None ->
      prerr_endline usage;
      exit 2
  | Some given ->
      let root = corpus_root given in
      if not (Sys.file_exists root && Sys.is_directory root) then (
        Printf.eprintf "calls: no corpus directory at %s\n" given;
        exit 2);
      let tally, differences, files = measure ~root ~limit:options.limit in
      report ~root ~files tally differences;
      if files = 0 then (
        Printf.eprintf "calls: no .jazz file under %s\n" root;
        exit 2);
      if tally.calls = 0 then (
        Printf.eprintf
          "calls: the corpus typed no call, so nothing was measured\n";
        exit 2);
      exit (if tally.one = tally.calls then 0 else 1)

let () = main ()
