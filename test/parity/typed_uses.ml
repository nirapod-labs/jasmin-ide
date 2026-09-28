(** The typed-use half of the resolver parity gate: for every variable
    occurrence the compiler typed in one file, the declaration the resolver
    answers with against the declaration the compiler bound the occurrence to.

    The two sides come from one text and one architecture:

    - the compiler's side is the typed facts the adapter returns for the file
      ({!Jide_jasmin.Adapter.check}). A {!Jide_jasmin.Facts.use} is one
      variable occurrence of a typed body, with [u_decl] and [u_dfile]: the
      range [v_dloc] names and the file that range indexes. That pair is the
      declaration the compiler's pretyping bound the occurrence to.
    - the resolver's side is {!Jide_engine.Resolve}, asked at the byte the
      occurrence starts at. The answer is a declaration when the name is one,
      and that declaration carries the file and the range of the name it
      declares.

    The comparison is the design's identity of a declaration (section 9): the
    normalised file and the byte where the name starts. Nothing else takes
    part, because nothing else survives a re-check.

    The file is compared through {!Jide_engine.Index.normalise}, so the two
    sides agree on a spelling: the compiler records the name the file was
    parsed under, the resolver records the path it resolved.

    A leading UTF-8 byte-order mark is removed once, and the same bytes go to
    the check and to the resolver. The engine strips the mark for the check
    alone (the resolver reads the document as the caller holds it), which is a
    question about the engine's byte coordinates and not about the resolver's
    answer; here both sides see one text, so a mark cannot make a difference
    of its own.

    A file is measured only when the check accepts it: a file the check
    reports an error for has no compiler-side answer to compare with, and its
    typed uses are not counted. The run says so per file ("accepted: no"), and
    the caller (test/parity/uses.sh) counts those files apart and fails on
    them, because the gate's corpus is the release's positive folder.

    A use is compared in the file its span indexes, which is not always the
    document: a [require] inside a namespace is spliced into the document's
    item stream, so the compiler types the required file's body there and
    records its occurrences with that file's offsets. The typed facts name
    only the declaration's file ([u_dfile]), never the use's, so the file of a
    use is read from the bytes: the first file the resolution reached whose
    text writes the use's name at the span, the document first.

    A use whose span writes no such name in any file of the resolution is one
    the compiler placed with a location that is not the source's: an implicit
    flag it made up ([_zf_] and its siblings), an intrinsic's expansion
    ([bytes] of [#randombytes]), or the variable a [?{...}] group receives.
    Nothing in the text declares or uses that name there, so no occurrence
    holds the byte and no resolver can answer; those uses are counted apart
    as [unlocated] and listed, never as a difference.

    A qualified name is compared by its last segment: the compiler holds
    [C::A::x] for a use the index records as [x], because a byte only spells
    the segment written at it. The declaration both sides name is still
    compared by file and byte, so the segment is all either side needs.

    Nothing here raises. Every question the file asks is answered by a count.

    Usage:

    {v
      typed_uses <file> [<architecture>]
      typed_uses --selftest
    v}

    The architecture is one the adapter knows ([x86-64] by default), because
    the corpus names the target in the folder a file sits under.

    Stdout carries nothing. Stderr carries the counts and, for each use that
    is not the compiler's declaration, one line naming it:

    {v
      accepted: yes
      errors: 0
      typed-uses: 12
      same-declaration: 12
      elsewhere: 0
      unresolved: 0
      other-name: 0
      unlocated: 0
    v}

    and, for each difference and each use that cannot be compared:

    {v
      use-elsewhere: 41 (3:10) x: the compiler declares it at /a/lib.jinc:1:13, the resolver answers /a/main.jazz:2:13
      use-unresolved: 77 (5:3) y: no occurrence at that byte
      use-other-name: 90 (6:9) a: the resolver's occurrence at that byte is b
      use-unlocated: 113 (7:3) _zf_: no file the resolution reached writes that name at those bytes
    v}

    A position is written one-based, [line:column], because a reader looks it
    up in an editor; the byte before it is the identity and is printed too, so
    the two readings cannot be confused.

    The exit status is 0 when the check accepted the file and every typed use
    is the compiler's declaration, 1 when one use is not (a use the resolver
    leaves unresolved, one it resolves elsewhere, or a byte whose occurrence
    carries another name), and 2 when the program cannot run at all. An
    unlocated use is neither: it has no occurrence to compare. *)

module A = Jide_jasmin.Adapter
module Arch = Jide_jasmin.Arch
module F = Jide_jasmin.Facts
module I = Jide_engine.Index
module R = Jide_engine.Resolve
module LI = Jide_lang.Line_index
module S = Jide_lang.Span

let usage =
  String.concat "\n"
    [
      "usage: typed_uses <file> [<architecture>]";
      "       typed_uses --selftest";
      "";
      "For <file>, compare the declaration every typed variable use resolves";
      "to with the declaration the compiler's v_dloc names, and print the";
      "counts and each difference on stderr.";
      "";
      "<architecture> is one of " ^ String.concat ", " A.arch_names
      ^ "; x86-64 by default.";
    ]

(* ------------------------------------------------------------------ *)
(* Reading *)

let read (path : string) =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception Sys_error _ -> None

(* A UTF-8 byte-order mark, as three bytes. [strip_bom text] is the text the
   compiler is given and how many bytes were removed, so a position the
   compiler reports can be read back in the document as the caller holds it. *)
let strip_bom (text : string) =
  if
    String.length text >= 3 && text.[0] = '\xef' && text.[1] = '\xbb'
    && text.[2] = '\xbf'
  then (3, String.sub text 3 (String.length text - 3))
  else (0, text)

(* ------------------------------------------------------------------ *)
(* The comparison *)

(** One use and what the resolver answered for it. *)
type outcome =
  | Same  (** the resolver's declaration is the one [v_dloc] names *)
  | Elsewhere of I.declaration
      (** the resolver resolved the use to another declaration *)
  | Unresolved of string
      (** the resolver answered no declaration, and why: no occurrence at the
          byte, [Unresolved], or an answer that is not a declaration at all *)
  | Other_name of string
      (** the occurrence at the byte carries another name than the compiler
          typed there, so the two sides are not speaking about one name *)
  | Unlocated of string
      (** no file the resolution reached writes the use's name where the
          compiler placed it, so there is no occurrence to ask about: the
          compiler invented the use and gave it an instruction's location *)

let same_declaration (compiler : F.use) (resolver : I.declaration) =
  String.equal (I.normalise resolver.I.file) (I.normalise compiler.F.u_dfile)
  && resolver.I.name_span.S.start_byte = compiler.F.u_decl.S.start_byte

(** [last_segment name] is the part of a name one byte range spells: a
    qualified use's [u_name] is the whole path the compiler keeps
    ([C::A::x]), while the occurrence at the byte is the segment written
    there ([x]). *)
let last_segment (name : string) =
  let rec start index =
    if index <= 0 then 0
    else match name.[index - 1] with ':' -> index | _ -> start (index - 1)
  in
  let first = start (String.length name) in
  String.sub name first (String.length name - first)

(** [squash text] is [text] without its spaces, tabs and newlines: a
    qualified name may be written with spaces around its colons, and the
    compiler's own spelling drops them. *)
let squash (text : string) =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun character ->
      match character with
      | ' ' | '\t' | '\r' | '\n' -> ()
      | _ -> Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

(** [spelled index use] is true when the bytes [use]'s span covers in [index]
    write the use's name: the source really names it there. *)
let spelled (index : I.t) (use : F.use) =
  String.equal
    (last_segment (squash (I.text_of_span index.I.text use.F.u_span)))
    (last_segment use.F.u_name)

(** [file_of_use resolve use] is the file the compiler's span of [use]
    indexes: the first file the resolution reached whose text writes the name
    at that span, the document first. [None] when no file does, which is what
    a use the compiler made up itself looks like (see [Unlocated]). *)
let file_of_use (resolve : R.t) (use : F.use) : I.t option =
  List.find_opt (fun (index : I.t) -> spelled index use) (R.files resolve)

let name_of_answer (answer : R.answer) =
  match answer with
  | R.Resolved _ -> "a declaration"
  | R.Intrinsic -> "an intrinsic"
  | R.Annotation -> "an annotation"
  | R.Include_path -> "an include path"
  | R.Unresolved -> "nothing"

let outcome_of (resolve : R.t) (use : F.use) : outcome =
  match file_of_use resolve use with
  | None ->
      Unlocated
        "no file the resolution reached writes that name at those bytes"
  | Some index -> (
      match
        R.answer_at resolve ~file:index.I.file
          ~byte:use.F.u_span.S.start_byte
      with
      | None -> Unresolved "no occurrence at that byte"
      | Some (occurrence, answer) ->
          if
            not
              (String.equal
                 (last_segment occurrence.I.name)
                 (last_segment use.F.u_name))
          then Other_name occurrence.I.name
          else
            match answer with
            | R.Resolved declaration ->
                if same_declaration use declaration then Same
                else Elsewhere declaration
            | other -> Unresolved ("the resolver answers " ^ name_of_answer other))

(** What one file's comparison found. *)
type answer = {
  accepted : bool;  (** the check reported no error for the file *)
  diagnostics : int;  (** how many error diagnostics it reported *)
  first_diagnostic : string option;
      (** the first of them, [line:column message], for the run's output *)
  uses : int;  (** the typed variable occurrences of the checked file *)
  same : int;
      (** the ones the resolver resolves to the compiler's declaration *)
  elsewhere : (F.use * I.declaration) list;
  unresolved : (F.use * string) list;
  other_name : (F.use * string) list;
  unlocated : (F.use * string) list;
      (** the ones the compiler placed where no file writes their name *)
}

let empty_answer =
  {
    accepted = true;
    diagnostics = 0;
    first_diagnostic = None;
    uses = 0;
    same = 0;
    elsewhere = [];
    unresolved = [];
    other_name = [];
    unlocated = [];
  }

let position (index : LI.t) (byte : int) =
  let position = LI.position_of_byte index byte in
  Printf.sprintf "%d:%d" (position.Jide_lang.Position.line + 1)
    (position.Jide_lang.Position.character + 1)

let is_error (diagnostic : Jide_lang.Diagnostic.t) =
  match diagnostic.Jide_lang.Diagnostic.severity with
  | Jide_lang.Diagnostic.Error -> true
  | Warning | Information | Hint -> false

let message_of_diagnostic index (diagnostic : Jide_lang.Diagnostic.t) =
  Printf.sprintf "%s %s"
    (position index diagnostic.Jide_lang.Diagnostic.span.S.start_byte)
    diagnostic.Jide_lang.Diagnostic.message

(** [answer_of_facts ~resolve facts] is every use of [facts] against the
    resolver [resolve]: the counts, and the uses that are not the compiler's
    declaration. *)
let answer_of_facts ~(resolve : R.t) (facts : F.t) =
  let answer = ref { empty_answer with uses = List.length facts.F.uses } in
  List.iter
    (fun (use : F.use) ->
      match outcome_of resolve use with
      | Same -> answer := { !answer with same = !answer.same + 1 }
      | Elsewhere declaration ->
          answer :=
            { !answer with elsewhere = (use, declaration) :: !answer.elsewhere }
      | Unresolved why ->
          answer := { !answer with unresolved = (use, why) :: !answer.unresolved }
      | Other_name found ->
          answer :=
            { !answer with other_name = (use, found) :: !answer.other_name }
      | Unlocated why ->
          answer := { !answer with unlocated = (use, why) :: !answer.unlocated })
    facts.F.uses;
  {
    !answer with
    elsewhere = List.rev !answer.elsewhere;
    unresolved = List.rev !answer.unresolved;
    other_name = List.rev !answer.other_name;
    unlocated = List.rev !answer.unlocated;
  }

(** [compare_text ~arch ~file text] is the whole comparison for one text: the
    check, the resolver over the same bytes, and every typed use of the
    check's facts. *)
let compare_text ~(arch : A.arch) ~(file : string) (text : string) : answer =
  let file = I.normalise file in
  let _bom, body = strip_bom text in
  let checked = A.check arch Arch.Linux ~file ~text:body ~idirs:[] in
  let errors = List.filter is_error checked.A.diagnostics in
  let index = LI.create body in
  let base =
    {
      empty_answer with
      accepted = errors = [];
      diagnostics = List.length errors;
      first_diagnostic =
        (match errors with
        | [] -> None
        | first :: _ -> Some (message_of_diagnostic index first));
      uses = List.length checked.A.facts.F.uses;
    }
  in
  if not base.accepted then base
  else
    let resolve = R.of_text ~idirs:[] ~read ~file body in
    let answer = answer_of_facts ~resolve checked.A.facts in
    { answer with accepted = true; diagnostics = 0 }

let compare_file ~(arch : A.arch) (path : string) =
  match read path with
  | None -> Error (Printf.sprintf "cannot read %s" path)
  | Some text -> Ok (compare_text ~arch ~file:path text)

(* ------------------------------------------------------------------ *)
(* The report *)

let status (answer : answer) =
  if
    answer.accepted
    && answer.elsewhere = []
    && answer.unresolved = []
    && answer.other_name = []
  then 0
  else 1

let say_answer (answer : answer) =
  Printf.eprintf "accepted: %s\n" (if answer.accepted then "yes" else "no");
  Printf.eprintf "errors: %d\n" answer.diagnostics;
  (match answer.first_diagnostic with
  | None -> ()
  | Some message -> Printf.eprintf "first-error: %s\n" message);
  Printf.eprintf "typed-uses: %d\n" answer.uses;
  Printf.eprintf "same-declaration: %d\n" answer.same;
  Printf.eprintf "elsewhere: %d\n" (List.length answer.elsewhere);
  Printf.eprintf "unresolved: %d\n" (List.length answer.unresolved);
  Printf.eprintf "other-name: %d\n" (List.length answer.other_name);
  Printf.eprintf "unlocated: %d\n" (List.length answer.unlocated);
  flush stderr

(** [say_differences ~index answer] writes one line per use that is not the
    compiler's declaration, and one per use there is no occurrence for. The
    positions are of [index], the text the check and the resolver both saw;
    the compiler's own range indexes [u_dfile], which is the checked file for
    the facts of one document. *)
let say_differences ~(index : LI.t) (answer : answer) =
  let at span = position index span.S.start_byte in
  let use_line (use : F.use) = Printf.sprintf "%d (%s)" use.F.u_span.S.start_byte (at use.F.u_span) in
  List.iter
    (fun ((use : F.use), (declaration : I.declaration)) ->
      Printf.eprintf
        "use-elsewhere: %s %s: the compiler declares it at %s:%s, the \
         resolver answers %s:%s\n"
        (use_line use) use.F.u_name use.F.u_dfile (at use.F.u_decl)
        declaration.I.file (at declaration.I.name_span))
    answer.elsewhere;
  List.iter
    (fun ((use : F.use), why) ->
      Printf.eprintf "use-unresolved: %s %s: %s\n" (use_line use)
        use.F.u_name why)
    answer.unresolved;
  List.iter
    (fun ((use : F.use), found) ->
      Printf.eprintf
        "use-other-name: %s %s: the resolver's occurrence at that byte is %s\n"
        (use_line use) use.F.u_name found)
    answer.other_name;
  List.iter
    (fun ((use : F.use), why) ->
      Printf.eprintf "use-unlocated: %s %s: %s\n" (use_line use) use.F.u_name why)
    answer.unlocated;
  flush stderr

(* ------------------------------------------------------------------ *)
(* The self-test *)

let valid =
  String.concat "\n"
    [
      "export fn main(reg u64 p) -> reg u64 {";
      "  reg u64 r s;";
      "  r = p;";
      "  s = r;";
      "  r = s;";
      "  return r;";
      "}";
      "";
    ]

(* One local declares the same name as another, in a nested block: the two
   declarations are different declarations, which is what makes a resolver
   that answers the outer one on an inner use show up as a difference. *)
let shadowed =
  String.concat "\n"
    [
      "export fn main(reg u64 p) -> reg u64 {";
      "  reg u64 x;";
      "  x = p;";
      "  if true {";
      "    reg u64 x;";
      "    x = p;";
      "    p = x;";
      "  }";
      "  return x;";
      "}";
      "";
    ]

(* A use the compiler cannot type: the check reports an error, so the file is
   not accepted and its uses are not counted. *)
let untyped =
  String.concat "\n" [ "export fn main() -> reg u64 {"; "  y = 1;"; "  return y;"; "}"; "" ]

(* A global declared in a required file and used in the document: the
   compiler's declaration is in the other file, so the file half of the
   identity is exercised. *)
let requirer =
  String.concat "\n"
    [
      "require \"lib.jinc\"";
      "";
      "export fn main() -> reg u64 {";
      "  reg u64 r;";
      "  r = n;";
      "  return r;";
      "}";
      "";
    ]

let library = String.concat "\n" [ "param int n = 3;"; "" ]

(* A use of a qualified name: the compiler keeps [A::x] for it while the
   index holds [x] at the same byte, so a comparison of the whole names would
   report a difference that is only a spelling. *)
let qualified =
  String.concat "\n"
    [
      "namespace A {";
      "  u32 x = 1;";
      "}";
      "";
      "export fn main(reg u32 a) -> reg u32 {";
      "  reg u32 r;";
      "  r = A::x;";
      "  return r;";
      "}";
      "";
    ]

(* A required file brought in by a namespace: the compiler types its body as
   part of the document's item and records its uses with the required file's
   offsets, so a comparison that reads every use in the document finds no
   occurrence for any of them. *)
let namespaced_requirer =
  String.concat "\n"
    [
      "namespace M {";
      "  require \"body.jinc\"";
      "}";
      "";
      "export fn main(reg u64 p) -> reg u64 {";
      "  reg u64 r;";
      "  r = M::f(p);";
      "  return r;";
      "}";
      "";
    ]

let body =
  String.concat "\n"
    [
      "fn f(reg u64 q) -> reg u64 {";
      "  reg u64 v;";
      "  v = q;";
      "  return v;";
      "}";
      "";
    ]

(* [with_decl ~byte use] is [use] with its compiler-side declaration moved to
   [byte]: a fact the compiler would not produce, used to prove the comparison
   answers a difference. *)
let with_decl ~(byte : int) (use : F.use) : F.use =
  {
    F.u_span = use.F.u_span;
    u_name = use.F.u_name;
    u_item = use.F.u_item;
    u_decl = S.make ~start_byte:byte ~end_byte:(byte + 1);
    u_dfile = use.F.u_dfile;
  }

let check_selftest () =
  let failures = ref 0 in
  let check name condition =
    if not condition then (
      incr failures;
      Printf.eprintf "selftest: %s: failed\n" name)
  in
  let arch = Arch.X86_64 in
  let good = compare_text ~arch ~file:"valid.jazz" valid in
  check "valid: the check accepts it" good.accepted;
  check "valid: it holds typed uses" (good.uses > 0);
  check "valid: every use is the compiler's declaration"
    (good.same = good.uses && good.elsewhere = [] && good.unresolved = []
   && good.other_name = []);
  check "valid: the exit status is the passing one" (status good = 0);
  (* The comparison is a comparison and not a constant: moving one use's
     compiler-side declaration to another byte makes it a difference, whether
     that byte starts another declaration or none at all. *)
  let one_use_moved =
    let checked = A.check arch Arch.Linux ~file:"valid.jazz" ~text:valid ~idirs:[] in
    match checked.A.facts.F.uses with
    | [] -> { empty_answer with accepted = true }
    | first :: rest ->
        let facts =
          {
            F.items = checked.A.facts.F.items;
            uses = with_decl ~byte:1 first :: rest;
          }
        in
        let resolve = R.of_text ~idirs:[] ~read ~file:"valid.jazz" valid in
        let answer = answer_of_facts ~resolve facts in
        { answer with accepted = true }
  in
  check "a moved declaration is a difference"
    (one_use_moved.elsewhere <> [] || one_use_moved.unresolved <> []);
  check "a moved declaration fails the run" (status one_use_moved = 1);
  check "a moved declaration leaves the other uses alone"
    (one_use_moved.same + List.length one_use_moved.elsewhere
   + List.length one_use_moved.unresolved
   + List.length one_use_moved.other_name
   + List.length one_use_moved.unlocated
   = one_use_moved.uses);
  (* A use the compiler made up itself: its span is an instruction's, so the
     bytes spell no such name in any file and no occurrence holds it. It is
     counted apart and listed, never as a difference, so it cannot fail a run
     whose every source-written use agrees. *)
  let made_up =
    let checked = A.check arch Arch.Linux ~file:"valid.jazz" ~text:valid ~idirs:[] in
    match checked.A.facts.F.uses with
    | [] -> empty_answer
    | first :: _ ->
        let use =
          {
            F.u_span = first.F.u_span;
            u_name = "_zf_";
            u_item = first.F.u_item;
            u_decl = first.F.u_decl;
            u_dfile = first.F.u_dfile;
          }
        in
        let resolve = R.of_text ~idirs:[] ~read ~file:"valid.jazz" valid in
        answer_of_facts ~resolve { F.items = []; uses = [ use ] }
  in
  check "a made-up use is not located" (List.length made_up.unlocated = 1);
  check "a made-up use is not a difference"
    (made_up.unresolved = [] && made_up.elsewhere = []
   && made_up.other_name = []);
  check "a made-up use does not fail the run"
    (status { made_up with accepted = true } = 0);
  (* A qualified name is one name on both sides: the compiler's [A::x] is the
     index's [x] at the same byte, and the declaration compared is the same. *)
  let qualification = compare_text ~arch ~file:"qualified.jazz" qualified in
  check "qualified: the check accepts it" qualification.accepted;
  check "qualified: it holds typed uses" (qualification.uses > 0);
  check "qualified: every use is the compiler's declaration"
    (qualification.same = qualification.uses && qualification.elsewhere = []
   && qualification.unresolved = [] && qualification.other_name = []);
  (* The shadowing source is the same answer: every use is the declaration its
     own block wrote, which is the case a resolver that ignores blocks gets
     wrong. *)
  let blocks = compare_text ~arch ~file:"shadowed.jazz" shadowed in
  check "shadowed: the check accepts it" blocks.accepted;
  check "shadowed: every use is its own block's declaration"
    (blocks.same = blocks.uses && blocks.uses > 0);
  (* A file the compiler cannot type is not accepted, and its uses are not
     counted: there is no compiler-side answer to compare with. *)
  let broken = compare_text ~arch ~file:"untyped.jazz" untyped in
  check "untyped: the check does not accept it" (not broken.accepted);
  check "untyped: no use is counted" (broken.uses = 0);
  check "untyped: the first error is reported" (broken.first_diagnostic <> None);
  (* The byte-order mark is stripped once, for both sides: the same source with
     a mark gives the same answer. *)
  let marked = compare_text ~arch ~file:"valid.jazz" ("\xef\xbb\xbf" ^ valid) in
  check "a marked file holds the same typed uses" (marked.uses = good.uses);
  check "a marked file resolves every use the same way"
    (marked.same = marked.uses && marked.accepted);
  (* A declaration in a required file: the compiler names that file in
     [u_dfile] and the resolver has to name it too. *)
  let directory =
    let path = Filename.temp_file "typed-use-parity" ".dir" in
    Sys.remove path;
    Unix.mkdir path 0o700;
    path
  in
  let write name text =
    Out_channel.with_open_bin (Filename.concat directory name) (fun channel ->
        Out_channel.output_string channel text)
  in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat directory name))
        (Sys.readdir directory);
      Unix.rmdir directory)
    (fun () ->
      write "lib.jinc" library;
      write "main.jazz" requirer;
      write "body.jinc" body;
      write "namespaced.jazz" namespaced_requirer;
      let path = Filename.concat directory "main.jazz" in
      (match compare_file ~arch path with
      | Error message -> check ("required: it runs (" ^ message ^ ")") false
      | Ok required ->
          check "required: the check accepts it" required.accepted;
          check "required: it holds typed uses" (required.uses > 0);
          check "required: every use is the compiler's declaration"
            (required.same = required.uses && required.elsewhere = []
           && required.unresolved = [] && required.other_name = []
           && required.unlocated = []);
          check "required: the exit status is the passing one"
            (status required = 0));
      (* The required file a namespace brought in is typed as part of the
         document's item, so its body's uses carry the required file's offsets.
         They are compared in that file, not in the document. *)
      let path = Filename.concat directory "namespaced.jazz" in
      match compare_file ~arch path with
      | Error message -> check ("namespaced: it runs (" ^ message ^ ")") false
      | Ok namespaced ->
          check "namespaced: the check accepts it" namespaced.accepted;
          check "namespaced: it holds typed uses" (namespaced.uses > 0);
          check "namespaced: the document's use is the compiler's declaration"
            (namespaced.unresolved = [] && namespaced.elsewhere = []
           && namespaced.other_name = []
           && namespaced.same = namespaced.uses);
          check "namespaced: the exit status is the passing one"
            (status namespaced = 0));
  if !failures = 0 then print_endline "typed_uses: selftest ok"
  else (
    Printf.eprintf "typed_uses: %d selftest check(s) failed\n" !failures;
    exit 1)

(* ------------------------------------------------------------------ *)

let arch_of_name name =
  match A.arch_of_string name with
  | Some arch -> Ok arch
  | None -> Error (Printf.sprintf "%S is not an architecture of the adapter" name)

let main () =
  match Array.to_list Sys.argv with
  | _ :: [ "--selftest" ] -> check_selftest ()
  | _ :: [ "--help" ] | _ :: [ "-help" ] ->
      print_endline usage;
      exit 0
  | _ :: (path :: rest) -> (
      let name = match rest with [] -> "x86-64" | [ name ] -> name | _ -> "" in
      match arch_of_name name with
      | Error message ->
          Printf.eprintf "typed_uses: %s\n%s\n" message usage;
          exit 2
      | Ok arch -> (
          match compare_file ~arch path with
          | Error message ->
              Printf.eprintf "typed_uses: %s\n" message;
              exit 2
          | Ok answer ->
              let text = match read path with Some text -> text | None -> "" in
              let _bom, body = strip_bom text in
              say_answer answer;
              say_differences ~index:(LI.create body) answer;
              exit (status answer)))
  | _ ->
      prerr_endline usage;
      exit 2

let () = main ()
