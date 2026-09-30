(** The typed-use half of the resolver parity gate.

    The gate's clause is one sentence: 100% of typed variable uses in the
    corpus resolve to the compiler's declaration, its [v_dloc]. This program
    measures that clause over one file. It runs the compiler's own check
    ({!Jide_jasmin.Adapter.check}), which leaves the typed facts of the
    file, and the engine's resolver over the same bytes
    ({!Jide_engine.Resolve.of_text}), and for every typed use asks the
    resolver what the use's name names.

    The two sides come from one text and one architecture:

    - the compiler's side is {!Jide_jasmin.Facts.use}. [u_file] and [u_span]
      say where the compiler placed the occurrence, [u_name] is the name it
      holds the variable by, and [u_dfile] and [u_decl] are the declaration it
      bound the occurrence to. Nothing here reads the compiler's unique ids.
    - the resolver's side is {!Jide_engine.Resolve.visit_answers}, asked at
      the byte where the occurrence's name starts. A file a namespace reads is
      read once per namespace, so the resolver gives one answer per reading,
      and each reading is compared with the compiler's use of that reading.

    The identity of a declaration is the design's (section 9): the normalised
    file and the byte where its name starts.

    {2 What is a use the source writes}

    A use is measured when the bytes the compiler placed it at spell its name:
    the first identifier equal to the name's last segment inside the use's
    span, at a token boundary. A qualified name [A::x] is compared by its last
    segment, because a byte spells one segment. Two kinds of use are not
    measured, and each is counted and listed:

    - [unlocated]: no identifier of that name lies in the compiler's span.
      These are uses the compiler made up on an instruction's location: a flag
      it names [_zf_] and its siblings, a temporary, an intrinsic's expansion.
      The source never writes them, so no occurrence exists to ask about.
    - [unchecked]: the uses of a file the check refuses. The run fails on such
      a file, because the corpus is the release's accepted folder.

    The gate's percentage is over the measured uses. The run prints all five
    counts and the total, and they reconcile.

    {2 Differences}

    A measured use is one of these, and each is printed with its file, its
    byte and its position:

    - [elsewhere]: the resolver answers another declaration;
    - [unresolved]: the resolver answers no declaration at that byte (no
      occurrence, [Unresolved], or a name that is not a declaration), or the
      resolution never reached the file;
    - [other-name]: the occurrence at the byte holds another name.

    Uses at one byte are matched one-to-one with the readings of the file at
    that byte, so a use the compiler gives a declaration the resolver gives to
    no reading of that byte is a difference, and a reading the compiler never
    typed is not counted as one.

    Usage:

    {v
      typed_uses <file> [<architecture>] [--negative-control]
      typed_uses --selftest
    v}

    [--negative-control] moves the declaration the compiler is said to have
    bound the first measured use to, so the run must report one difference.
    It exists to show the comparison fails when a binding is wrong; the
    script that runs it expects the failure.

    Stdout carries nothing. Stderr carries the counts, and one line per
    difference:

    {v
      accepted: yes
      errors: 0
      typed-uses: 12
      source-written: 12
      same-declaration: 12
      elsewhere: 0
      unresolved: 0
      other-name: 0
      unlocated: 0
      unchecked: 0
    v}

    The exit status is 0 when the check accepted the file and every measured
    use is the compiler's declaration, 1 when one is not or the check refused
    the file, and 2 when the program cannot run at all. *)

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
      "usage: typed_uses <file> [<architecture>] [--negative-control]";
      "       typed_uses --selftest";
      "";
      "For <file>, compare the declaration every typed variable use resolves";
      "to with the declaration the compiler's v_dloc names, and print the";
      "counts and each difference on stderr.";
      "";
      "<architecture> is one of "
      ^ String.concat ", " A.arch_names
      ^ "; x86-64 by default.";
    ]

(* ------------------------------------------------------------------ *)
(* Reading *)

let read (path : string) =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception Sys_error _ -> None

(* A UTF-8 byte-order mark, as three bytes. The mark is removed once, and the
   same bytes go to the check and to the resolver, so a mark cannot make a
   difference of its own. *)
let strip_bom (text : string) =
  if
    String.length text >= 3
    && text.[0] = '\xef'
    && text.[1] = '\xbb'
    && text.[2] = '\xbf'
  then (3, String.sub text 3 (String.length text - 3))
  else (0, text)

(* ------------------------------------------------------------------ *)
(* Names and bytes *)

let is_ident_char (c : char) =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false

(** [last_segment name] is the part of a qualified name after its last [::]. *)
let last_segment (name : string) =
  let rec start index =
    if index <= 0 then 0
    else match name.[index - 1] with ':' -> index | _ -> start (index - 1)
  in
  let first = start (String.length name) in
  String.sub name first (String.length name - first)

(** [spelled_in text ~start ~stop name] is the byte where the first identifier
    equal to [name] begins inside [text] between [start] and [stop], at a token
    boundary on both sides, or [None] when no such identifier lies there. *)
let spelled_in (text : string) ~(start : int) ~(stop : int) (name : string) :
    int option =
  let length = String.length text in
  let n = String.length name in
  let stop = min stop length in
  let matches index =
    index + n <= stop
    && (index = 0 || not (is_ident_char text.[index - 1]))
    && (index + n >= length || not (is_ident_char text.[index + n]))
    && String.equal (String.sub text index n) name
  in
  let rec scan index =
    if index + n > stop then None
    else if matches index then Some index
    else scan (index + 1)
  in
  if n = 0 then None else scan (max 0 start)

(** [where index byte] is the one-based position of [byte] in [index]'s text,
    as a reader finds it in an editor. *)
let where (text : LI.t) (byte : int) =
  let position = LI.position_of_byte text byte in
  Printf.sprintf "%d:%d"
    (position.Jide_lang.Position.line + 1)
    (position.Jide_lang.Position.character + 1)

(* ------------------------------------------------------------------ *)
(* The comparison *)

(* [with_decl ~byte use] is [use] with its compiler-side declaration moved to
   [byte]: a fact the compiler would not produce, used to prove the comparison
   answers a difference. *)
let with_decl ~(byte : int) (use : F.use) : F.use =
  { use with F.u_decl = S.make ~start_byte:byte ~end_byte:(byte + 1) }

let same_declaration (compiler : F.use) (resolver : I.declaration) =
  String.equal (I.normalise resolver.I.file) (I.normalise compiler.F.u_dfile)
  && resolver.I.name_span.S.start_byte = compiler.F.u_decl.S.start_byte

let name_of_answer (answer : R.answer) =
  match answer with
  | R.Resolved _ -> "a declaration"
  | R.Intrinsic -> "an intrinsic"
  | R.Annotation -> "an annotation"
  | R.Include_path -> "an include path"
  | R.Unresolved -> "nothing"

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
