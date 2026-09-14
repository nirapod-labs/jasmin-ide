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

