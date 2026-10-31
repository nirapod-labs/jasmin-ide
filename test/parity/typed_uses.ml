(** The typed-use half of the resolver parity gate.

    The gate's clause is one sentence: 100% of typed variable uses in the corpus
    resolve to the compiler's declaration, its [v_dloc]. This program measures that
    clause over one file. It runs the compiler's check ({!Jide_jasmin.Adapter.check}),
    which leaves the typed facts of the file, and the engine's resolver over the same
    bytes, and for every typed use asks what the use's name names in the namespace the
    compiler typed the use in.

    The two sides:

    - the compiler's side is {!Jide_jasmin.Facts.use}. [u_file] and [u_span] say where
      the compiler placed the occurrence, [u_name] is the name, [u_dfile] and [u_decl]
      are the declaration the occurrence was bound to, and [u_reading] is the namespace
      path the use was typed in.
    - the resolver's side is {!Jide_engine.Resolve.reading_answer}, asked at the byte
      where the occurrence's name starts, in the reading of the same namespace.

    A use is paired with the reading it was typed in and with no other. A use the
    compiler typed in namespace [Long] is judged by the resolver's answer for namespace
    [Long] only, so a resolver that answers the right declaration in one reading and a
    wrong one in the other fails on exactly the wrong use. A declaration is identified
    by its normalised file and the byte where its name starts, never by a compiler
    unique id, so the two sides can be compared without sharing a numbering.

    {2 The classes}

    The facts hold the typed uses of the checked file and of every file a top-level
    [require] reaches, each naming its own file. Every one of them falls in exactly
    one class:

    - [same], [elsewhere], [unresolved], [other-name]: measured. They are the resolver's
      answers for the use's byte in its own reading.
    - [misplaced]: measured and a failure. The compiler's span holds no identifier of
      the name, but the file writes the name somewhere else. The span is wrong, so the
      use may be a user's own use, and it is never excluded.
    - [excluded]: not measured. The name is not an identifier token anywhere in the file
      the use's span indexes, so the source writes no occurrence for the compiler to
      have placed. The compiler invents these uses on an instruction: a flag it names
      [_zf_] or one of its siblings ([flag]), or a temporary such as the [bytes] of
      [#randombytes] ([temporary]). The run prints the count of each reason and lists
      every excluded use.
    - [unchecked]: a use of a file the check refused. The run fails on such a file.

    The gate's percentage is over the measured uses. The run checks that the classes
    add up to the typed uses, so no use leaves the tally.

    {2 Output}

    Usage:

    {v
      typed_uses <file> [<architecture>] [--negative-control]
      typed_uses --selftest
    v}

    [--negative-control] plants a wrong binding: the declaration of the first measured
    use of the check, in the checked file or in a file it requires, is moved to another
    byte. The run must then report that use and exit 1, which proves the comparison
    fails when a binding is wrong; [planted: yes] on stderr says a use was planted. The
    script that runs it expects the failure.

    Stdout carries the ledger, one tab-separated line per typed use. Stderr carries the
    counts, then one line per difference and one per excluded use. A position is
    one-based, [line:column].

    The exit status is 0 when the check accepted the file and every measured use is the
    compiler's declaration in its own reading, 1 when one is not or the check refused
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
      "For <file>, compare the declaration every typed variable use resolves \
       to,";
      "in the namespace the compiler typed it in, with the declaration the";
      "compiler's v_dloc names, and print the counts and each difference on \
       stderr.";
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

(* A UTF-8 byte-order mark, as three bytes. The mark is removed once, and the same
   bytes go to the check and to the resolver, so a mark cannot make a difference of
   its own. *)
let strip_bom (text : string) =
  if
    String.length text >= 3
    && text.[0] = '\xef'
    && text.[1] = '\xbb'
    && text.[2] = '\xbf'
  then (3, String.sub text 3 (String.length text - 3))
  else (0, text)

(* ------------------------------------------------------------------ *)
(* Names and tokens *)

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

(** [find_token text ~from ~until name] is the byte where the first identifier token
    equal to [name] begins, the token lying wholly between [from] and [until], or
    [None]. A token has an identifier boundary on both sides. *)
let find_token (text : string) ~(from : int) ~(until : int) (name : string) :
    int option =
  let length = String.length text in
  let until = min until length in
  let n = String.length name in
  let matches index =
    index + n <= until
    && (index = 0 || not (is_ident_char text.[index - 1]))
    && (index + n >= length || not (is_ident_char text.[index + n]))
    && String.equal (String.sub text index n) name
  in
  let rec scan index =
    if index + n > until then None
    else if matches index then Some index
    else scan (index + 1)
  in
  if n = 0 then None else scan (max 0 from)

(** [find_sub text fragment] is the byte where [fragment] first occurs in [text]. *)
let find_sub (text : string) (fragment : string) : int option =
  let length = String.length text and n = String.length fragment in
  let rec scan index =
    if index + n > length then None
    else if String.equal (String.sub text index n) fragment then Some index
    else scan (index + 1)
  in
  scan 0

(** [is_flag_name name] is true for the name shape of a flag the compiler names for an
    intrinsic's unnamed flag: [_xf_] with one lower-case letter [x]. The corpus writes
    no such name. *)
let is_flag_name (name : string) =
  String.length name = 4
  && name.[0] = '_'
  && name.[2] = 'f'
  && name.[3] = '_'
  && match name.[1] with 'a' .. 'z' -> true | _ -> false

(** [where lines byte] is the one-based [line:column] of [byte]. *)
let where (lines : LI.t) (byte : int) =
  let position = LI.position_of_byte lines byte in
  Printf.sprintf "%d:%d"
    (position.Jide_lang.Position.line + 1)
    (position.Jide_lang.Position.character + 1)

(* ------------------------------------------------------------------ *)
(* The comparison *)

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

let reading_name (reading : string) =
  if String.equal reading "" then "the top level" else "namespace " ^ reading

type answer = {
  accepted : bool;  (** the check reported no error for the file *)
  diagnostics : int;  (** how many error diagnostics it reported *)
  first_diagnostic : string option;
      (** the first of them, [line:column message], for the run's output *)
  uses : int;  (** the typed variable occurrences of the checked file *)
  measured : int;
      (** the uses the gate counts: same, elsewhere, unresolved, other-name and
          misplaced *)
  same : int;
      (** the ones the resolver answers with the compiler's declaration *)
  elsewhere : string list;
  unresolved : string list;
  other_name : string list;
  misplaced : string list;
      (** the span is wrong, and the name is in the file *)
  excluded_flag : string list;
      (** excluded: a compiler flag the source never writes *)
  excluded_temporary : string list;
      (** excluded: a compiler temporary the source never writes *)
  unchecked : int;  (** the uses of a file the check refused *)
  planted : bool;
      (** the negative control moved the declaration of one measured use *)
  ledger : string list;
      (** one tab-separated line per typed use, in the order of the facts: the
          class, whether the use is the document's own or a required file's, its
          file, its byte, its namespace reading, its occurrence index among the
          uses at the same key, and its name. The uses script takes the distinct
          uses of an architecture from these lines. *)
}
(** What one file's comparison found. *)

let empty_answer =
  {
    accepted = true;
    diagnostics = 0;
    first_diagnostic = None;
    uses = 0;
    measured = 0;
    same = 0;
    elsewhere = [];
    unresolved = [];
    other_name = [];
    misplaced = [];
    excluded_flag = [];
    excluded_temporary = [];
    unchecked = 0;
    planted = false;
    ledger = [];
  }

let is_error (diagnostic : Jide_lang.Diagnostic.t) =
  match diagnostic.Jide_lang.Diagnostic.severity with
  | Jide_lang.Diagnostic.Error -> true
  | Warning | Information | Hint -> false

let message_of_diagnostic index (diagnostic : Jide_lang.Diagnostic.t) =
  Printf.sprintf "%s %s"
    (where index diagnostic.Jide_lang.Diagnostic.span.S.start_byte)
    diagnostic.Jide_lang.Diagnostic.message

(* The text of a file the resolution reached, when it reached it. *)
let text_of (resolve : R.t) (file : string) : string option =
  Option.map (fun (index : I.t) -> index.I.text) (R.file resolve file)

(* One line index per file, made on first use within one resolution. *)
let line_indexes (resolve : R.t) =
  let made : (string, LI.t option) Hashtbl.t = Hashtbl.create 8 in
  fun (file : string) ->
    match Hashtbl.find_opt made file with
    | Some lines -> lines
    | None ->
        let lines = Option.map LI.create (text_of resolve file) in
        Hashtbl.add made file lines;
        lines

let describe (lines_of : string -> LI.t option) (file : string) (byte : int) =
  match lines_of file with
  | Some lines -> Printf.sprintf "%s:%s" file (where lines byte)
  | None -> Printf.sprintf "%s:byte %d" file byte

(* The class of one typed use. [Excluded] carries the class name and the message. *)
type outcome =
  | Same
  | Elsewhere of string
  | Unresolved of string
  | Other_name of string
  | Misplaced of string
  | Excluded of string * string

let class_of (outcome : outcome) =
  match outcome with
  | Same -> "same"
  | Elsewhere _ -> "elsewhere"
  | Unresolved _ -> "unresolved"
  | Other_name _ -> "other-name"
  | Misplaced _ -> "misplaced"
  | Excluded (cls, _) -> cls

(** [classify ~lines_of ~resolve use] is the class of one typed use: what the resolver
    answers for its name in the namespace the compiler typed it in, or why the use is
    misplaced or excluded. *)
let classify ~(lines_of : string -> LI.t option) ~(resolve : R.t) (use : F.use)
    : outcome =
  let file = I.normalise use.F.u_file in
  let start = use.F.u_span.S.start_byte in
  let stop = use.F.u_span.S.end_byte in
  let name = last_segment use.F.u_name in
  match text_of resolve file with
  | None ->
      Unresolved
        (Printf.sprintf "%s %s: the resolution never reached the file"
           (describe lines_of file start)
           use.F.u_name)
  | Some text -> (
      match find_token text ~from:start ~until:stop name with
      | None -> (
          let shown =
            Printf.sprintf "%s %s" (describe lines_of file start) use.F.u_name
          in
          match find_token text ~from:0 ~until:(String.length text) name with
          | Some written_at ->
              Misplaced
                (Printf.sprintf
                   "%s: the compiler's span holds no identifier of the name, \
                    but the file writes it at %s"
                   shown
                   (describe lines_of file written_at))
          | None ->
              Excluded
                ( (if is_flag_name name then "excluded-flag"
                   else "excluded-temporary"),
                  shown ))
      | Some byte -> (
          let shown = describe lines_of file byte in
          let reading = use.F.u_reading in
          let message =
            Printf.sprintf "%s %s in %s" shown use.F.u_name
              (reading_name reading)
          in
          if not (List.mem reading (R.readings resolve file)) then
            Unresolved
              (Printf.sprintf
                 "%s: the resolver never walked the file in that namespace"
                 message)
          else
            match R.reading_answer resolve ~file ~byte ~reading with
            | None ->
                Unresolved
                  (Printf.sprintf "%s: no occurrence at that byte" message)
            | Some (occurrence, answer) -> (
                if not (String.equal (last_segment occurrence.I.name) name) then
                  Other_name
                    (Printf.sprintf "%s: the occurrence at that byte is %s"
                       message occurrence.I.name)
                else
                  match answer with
                  | R.Resolved declaration when same_declaration use declaration
                    ->
                      Same
                  | R.Resolved declaration ->
                      Elsewhere
                        (Printf.sprintf
                           "%s: the compiler declares it at %s, the resolver \
                            answers %s"
                           message
                           (describe lines_of use.F.u_dfile
                              use.F.u_decl.S.start_byte)
                           (describe lines_of declaration.I.file
                              declaration.I.name_span.S.start_byte))
                  | other ->
                      Unresolved
                        (Printf.sprintf "%s: the resolver answers %s" message
                           (name_of_answer other)))))

(** [ledger_of ~document classes] is one ledger line per use, in order. A use's key
    is its file, byte and reading; the occurrence index separates the uses that share
    a key, such as the two typed uses of [s += e] at one byte. *)
let ledger_of ~(document : string) (classes : (F.use * string) list) :
    string list =
  let occurrences = Hashtbl.create 64 in
  List.map
    (fun ((use : F.use), cls) ->
      let file = I.normalise use.F.u_file in
      let start = use.F.u_span.S.start_byte in
      let key = (file, start, use.F.u_reading) in
      let index = Option.value ~default:0 (Hashtbl.find_opt occurrences key) in
      Hashtbl.replace occurrences key (index + 1);
      let origin = if String.equal file document then "own" else "required" in
      Printf.sprintf "%s\t%s\t%s\t%d\t%s\t%d\t%s" cls origin file start
        use.F.u_reading index use.F.u_name)
    classes

(** [answer_of_facts ~document ~resolve facts] is every typed use of [facts] against
    [resolve], each in its own namespace, with the classes counted. *)
let answer_of_facts ~(document : string) ~(resolve : R.t) (facts : F.t) : answer
    =
  let lines_of = line_indexes resolve in
  let outcomes =
    List.map
      (fun (use : F.use) -> (use, classify ~lines_of ~resolve use))
      facts.F.uses
  in
  let collect pick =
    List.filter_map (fun (_, outcome) -> pick outcome) outcomes
  in
  let count predicate =
    List.length (List.filter (fun (_, outcome) -> predicate outcome) outcomes)
  in
  {
    empty_answer with
    uses = List.length facts.F.uses;
    measured =
      count (function
        | Same | Elsewhere _ | Unresolved _ | Other_name _ | Misplaced _ -> true
        | Excluded _ -> false);
    same = count (function Same -> true | _ -> false);
    elsewhere =
      collect (function Elsewhere message -> Some message | _ -> None);
    unresolved =
      collect (function Unresolved message -> Some message | _ -> None);
    other_name =
      collect (function Other_name message -> Some message | _ -> None);
    misplaced =
      collect (function Misplaced message -> Some message | _ -> None);
    excluded_flag =
      collect (function
        | Excluded ("excluded-flag", message) -> Some message
        | _ -> None);
    excluded_temporary =
      collect (function
        | Excluded ("excluded-temporary", message) -> Some message
        | _ -> None);
    ledger =
      ledger_of ~document
        (List.map (fun (use, outcome) -> (use, class_of outcome)) outcomes);
  }

(* [with_decl ~byte use] is [use] with its compiler-side declaration moved to [byte]:
   a fact the compiler would not produce, used to prove the comparison answers a
   difference. *)
let with_decl ~(byte : int) (use : F.use) : F.use =
  { use with F.u_decl = S.make ~start_byte:byte ~end_byte:(byte + 1) }

(* [plant_first_measured ~resolve facts] moves the declaration of the first
   measured use of the check, in whichever file the use is written, to another
   byte: the negative control. The first use whose name the file writes inside
   the use's span is taken, the same test [classify] measures by, so the planted
   use is one the comparison measures; a check whose own file has no measured
   use still gets one in a file it requires. [None] when the check has no
   measured use at all. *)
let plant_first_measured ~(resolve : R.t) (facts : F.t) : F.t option =
  let measured (use : F.use) =
    match text_of resolve (I.normalise use.F.u_file) with
    | None -> false
    | Some text ->
        find_token text ~from:use.F.u_span.S.start_byte
          ~until:use.F.u_span.S.end_byte
          (last_segment use.F.u_name)
        <> None
  in
  let planted = ref false in
  let uses =
    List.map
      (fun (use : F.use) ->
        if (not !planted) && measured use then (
          planted := true;
          (* Byte 0 never starts a declared name, since a declaration opens with
             its kind or type; a declaration at 0 would still be moved. *)
          let byte = if use.F.u_decl.S.start_byte = 0 then 1 else 0 in
          with_decl ~byte use)
        else use)
      facts.F.uses
  in
  if !planted then Some { facts with F.uses } else None

(** [compare_text ?plant ~arch ~file text] is the whole comparison for one text: the
    check, the resolver over the same bytes, and every typed use. With [plant] the
    negative control is applied first. *)
let compare_text ?(plant = false) ~(arch : A.arch) ~(file : string)
    (text : string) : answer =
  let file = I.normalise file in
  let _bom, body = strip_bom text in
  let checked = A.check arch Arch.Linux ~file ~text:body ~idirs:[] in
  let errors = List.filter is_error checked.A.diagnostics in
  let index = LI.create body in
  let uses = List.length checked.A.facts.F.uses in
  let base =
    {
      empty_answer with
      accepted = errors = [];
      diagnostics = List.length errors;
      first_diagnostic =
        (match errors with
        | [] -> None
        | first :: _ -> Some (message_of_diagnostic index first));
      uses;
    }
  in
  if not base.accepted then
    {
      base with
      unchecked = uses;
      ledger =
        ledger_of ~document:file
          (List.map (fun use -> (use, "unchecked")) checked.A.facts.F.uses);
    }
  else
    let resolve = R.of_text ~idirs:[] ~read ~file body in
    let facts, planted =
      if plant then
        match plant_first_measured ~resolve checked.A.facts with
        | Some facts -> (facts, true)
        | None -> (checked.A.facts, false)
      else (checked.A.facts, false)
    in
    {
      (answer_of_facts ~document:file ~resolve facts) with
      accepted = true;
      planted;
    }

let compare_file ~(arch : A.arch) (path : string) =
  match read path with
  | None -> Error (Printf.sprintf "cannot read %s" path)
  | Some text -> Ok (compare_text ~arch ~file:path text)

(* ------------------------------------------------------------------ *)
(* The report *)

let status (answer : answer) =
  if
    answer.accepted && answer.elsewhere = [] && answer.unresolved = []
    && answer.other_name = [] && answer.misplaced = [] && answer.unchecked = 0
  then 0
  else 1

let say_answer (answer : answer) =
  Printf.eprintf "accepted: %s\n" (if answer.accepted then "yes" else "no");
  Printf.eprintf "errors: %d\n" answer.diagnostics;
  (match answer.first_diagnostic with
  | None -> ()
  | Some message -> Printf.eprintf "first-error: %s\n" message);
  Printf.eprintf "typed-uses: %d\n" answer.uses;
  Printf.eprintf "measured: %d\n" answer.measured;
  Printf.eprintf "same-declaration: %d\n" answer.same;
  Printf.eprintf "elsewhere: %d\n" (List.length answer.elsewhere);
  Printf.eprintf "unresolved: %d\n" (List.length answer.unresolved);
  Printf.eprintf "other-name: %d\n" (List.length answer.other_name);
  Printf.eprintf "misplaced: %d\n" (List.length answer.misplaced);
  Printf.eprintf "excluded: %d\n"
    (List.length answer.excluded_flag + List.length answer.excluded_temporary);
  Printf.eprintf "excluded-flag: %d\n" (List.length answer.excluded_flag);
  Printf.eprintf "excluded-temporary: %d\n"
    (List.length answer.excluded_temporary);
  Printf.eprintf "unchecked: %d\n" answer.unchecked;
  Printf.eprintf "planted: %s\n" (if answer.planted then "yes" else "no");
  flush stderr

let say_differences (answer : answer) =
  let line kind messages =
    List.iter
      (fun message -> Printf.eprintf "use-%s: %s\n" kind message)
      messages
  in
  line "elsewhere" answer.elsewhere;
  line "unresolved" answer.unresolved;
  line "other-name" answer.other_name;
  line "misplaced" answer.misplaced;
  line "excluded-flag" answer.excluded_flag;
  line "excluded-temporary" answer.excluded_temporary;
  flush stderr

(* ------------------------------------------------------------------ *)
(* Selftest *)

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

(* One local declares the same name as another, in a nested block: a resolver that
   answers the outer one on an inner use shows up as a difference. *)
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

(* A use the compiler cannot type: the check reports an error, so the file is not
   accepted and its uses are unchecked. *)
let untyped =
  String.concat "\n"
    [ "export fn main() -> reg u64 {"; "  y = 1;"; "  return y;"; "}"; "" ]

(* A global declared in a required file and used in the document. *)
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

(* A qualified use: the compiler keeps [A::x] while the source writes the last segment
   at the byte, so the comparison is by the segment. *)
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

(* A required file brought in by a namespace: its uses carry the required file's
   offsets, and the namespace they were typed in. *)
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

(* The flag groups of a multi-result instruction: [?{cf}] names the variable that
   receives the flag, and [CF=cf] names the flag and the variable. *)
let flags =
  String.concat "\n"
    [
      "export fn add1(reg u64 arg) -> reg u64 {";
      "  reg u64 z;";
      "  reg bool cf, zf;";
      "  reg bool CF, ZF;";
      "  z = arg;";
      "  ?{CF=cf}, z = #ADC(z,z,cf);";
      "  ?{cf}, z = #ADC(z,z,cf);";
      "  ?{CF, ZF}, z = #ADC(z,z,cf);";
      "  return z;";
      "}";
      "";
    ]

(* A required file read into two namespaces: its [N] is the namespace's own. *)
let sum =
  String.concat "\n"
    [
      "fn sum(reg u32[N] t) -> reg u32 {";
      "  reg u32 s;";
      "  inline int c;";
      "  s = 0;";
      "  for c = 0 to N {";
      "    s += t[c];";
      "  }";
      "  return s;";
      "}";
      "";
    ]

let namespaces_twice =
  String.concat "\n"
    [
      "namespace Short {";
      "  param int N = 1;";
      "  require \"sum.jinc\"";
      "}";
      "";
      "namespace Long {";
      "  param int N = 8;";
      "  require \"sum.jinc\"";
      "}";
      "";
      "export";
      "fn test(reg u32 a) -> reg u32 {";
      "  reg u32[Short::N] x;";
      "  reg u32[Long::N] y;";
      "  reg u32 r t;";
      "  inline int i;";
      "  for i = 0 to Short::N { x[i] = a; }";
      "  for i = 0 to Long::N { y[i] = a; }";
      "  r = 0;";
      "  t = Short::sum(x);";
      "  r += t;";
      "  t = Long::sum(y);";
      "  r += t;";
      "  return r;";
      "}";
      "";
    ]

(* A required file named twice, once bare and once with a [./]. *)
let twice_named =
  String.concat "\n"
    [
      "require \"lib.jinc\"";
      "require \"./lib.jinc\"";
      "";
      "export fn main() -> reg u64 {";
      "  reg u64 r;";
      "  r = n;";
      "  return r;";
      "}";
      "";
    ]

(* A top-level required file with a body: its uses are typed as part of the document,
   and they belong to the required file and the top-level namespace. *)
let required_body =
  String.concat "\n"
    [
      "fn g(reg u64 a) -> reg u64 {";
      "  reg u64 t;";
      "  t = a;";
      "  return t;";
      "}";
      "";
    ]

let requirer_of_body =
  String.concat "\n"
    [
      "require \"lib2.jinc\"";
      "";
      "export fn main(reg u64 p) -> reg u64 {";
      "  reg u64 r;";
      "  r = g(p);";
      "  return r;";
      "}";
      "";
    ]

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
  check "valid: every use is measured" (good.measured = good.uses);
  check "valid: every use is the compiler's declaration"
    (good.same = good.measured && good.elsewhere = [] && good.unresolved = []
   && good.other_name = [] && good.misplaced = []);
  check "valid: no use is excluded"
    (good.excluded_flag = [] && good.excluded_temporary = []);
  check "valid: the exit status is the passing one" (status good = 0);
  let valid_facts =
    (A.check arch Arch.Linux ~file:"valid.jazz" ~text:valid ~idirs:[]).A.facts
  in
  let resolve_valid = R.of_text ~idirs:[] ~read ~file:"valid.jazz" valid in
  let valid_document = I.normalise "valid.jazz" in
  (* A moved declaration is a difference, and fails the run. *)
  let moved =
    match valid_facts.F.uses with
    | [] -> empty_answer
    | first :: rest ->
        answer_of_facts ~document:valid_document ~resolve:resolve_valid
          { valid_facts with F.uses = with_decl ~byte:1 first :: rest }
  in
  check "a moved declaration is a difference"
    (moved.elsewhere <> [] || moved.unresolved <> []);
  check "a moved declaration fails the run"
    (status { moved with accepted = true } = 1);
  (* A flag the file never writes is excluded, not measured, and never fails a run. *)
  let flag_use =
    match valid_facts.F.uses with
    | [] -> empty_answer
    | first :: _ ->
        answer_of_facts ~document:valid_document ~resolve:resolve_valid
          {
            F.items = [];
            uses = [ { first with F.u_name = "_zf_" } ];
            calls = [];
          }
  in
  check "a made-up flag is excluded as a flag"
    (List.length flag_use.excluded_flag = 1
    && flag_use.excluded_temporary = []
    && flag_use.measured = 0);
  check "a made-up flag does not fail the run"
    (status { flag_use with accepted = true } = 0);
  (* A temporary the file never writes is excluded as a temporary. *)
  let temporary_use =
    match valid_facts.F.uses with
    | [] -> empty_answer
    | first :: _ ->
        answer_of_facts ~document:valid_document ~resolve:resolve_valid
          {
            F.items = [];
            uses = [ { first with F.u_name = "bytes" } ];
            calls = [];
          }
  in
  check "a made-up temporary is excluded as a temporary"
    (List.length temporary_use.excluded_temporary = 1
    && temporary_use.excluded_flag = []
    && temporary_use.measured = 0);
  check "a made-up temporary does not fail the run"
    (status { temporary_use with accepted = true } = 0);
  (* A real identifier whose span is off is a failure, not an exclusion: the name is
     in the file, so the compiler's span cannot be the whole story. *)
  let misplaced =
    match (valid_facts.F.uses, find_sub valid "return r;") with
    | first :: _, Some start ->
        answer_of_facts ~document:valid_document ~resolve:resolve_valid
          {
            F.items = [];
            uses =
              [
                {
                  first with
                  F.u_name = "s";
                  F.u_span = S.make ~start_byte:start ~end_byte:(start + 9);
                };
              ];
            calls = [];
          }
    | _ -> empty_answer
  in
  check "a real identifier excluded by span is misplaced"
    (List.length misplaced.misplaced = 1
    && misplaced.excluded_flag = []
    && misplaced.excluded_temporary = []);
  check "a misplaced use fails the run"
    (status { misplaced with accepted = true } = 1);
  let flagged = compare_text ~arch ~file:"flags.jazz" flags in
  check "flags: the check accepts it" flagged.accepted;
  check "flags: it holds typed uses" (flagged.measured > 0);
  check "flags: every flag group's variable is the compiler's declaration"
    (flagged.same = flagged.measured
    && flagged.elsewhere = [] && flagged.unresolved = []
    && flagged.other_name = [] && flagged.misplaced = []);
  let qualification = compare_text ~arch ~file:"qualified.jazz" qualified in
  check "qualified: the check accepts it" qualification.accepted;
  check "qualified: every use is the compiler's declaration"
    (qualification.same = qualification.measured
    && qualification.measured > 0
    && qualification.elsewhere = []
    && qualification.unresolved = []
    && qualification.other_name = []);
  let blocks = compare_text ~arch ~file:"shadowed.jazz" shadowed in
  check "shadowed: the check accepts it" blocks.accepted;
  check "shadowed: every use is its own block's declaration"
    (blocks.same = blocks.measured && blocks.measured > 0);
  let broken = compare_text ~arch ~file:"untyped.jazz" untyped in
  check "untyped: the check does not accept it" (not broken.accepted);
  check "untyped: no use is measured" (broken.measured = 0);
  check "untyped: its uses are counted as unchecked"
    (broken.unchecked = broken.uses);
  check "untyped: the first error is reported" (broken.first_diagnostic <> None);
  check "untyped: the run fails" (status broken = 1);
  let marked = compare_text ~arch ~file:"valid.jazz" ("\xef\xbb\xbf" ^ valid) in
  check "a marked file holds the same typed uses" (marked.uses = good.uses);
  check "a marked file resolves every use the same way"
    (marked.same = marked.measured && marked.accepted);
  (* The files a directory holds: the requirer and its library, the body a namespace
     requires, the namespaces that read one file twice, and a file named twice. *)
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
      write "sum.jinc" sum;
      write "twice.jazz" namespaces_twice;
      write "named.jazz" twice_named;
      let path = Filename.concat directory "main.jazz" in
      (match compare_file ~arch path with
      | Error message -> check ("required: it runs (" ^ message ^ ")") false
      | Ok required ->
          check "required: the check accepts it" required.accepted;
          check "required: it holds typed uses" (required.measured > 0);
          check "required: every use is the compiler's declaration"
            (required.same = required.measured
            && required.elsewhere = [] && required.unresolved = []
            && required.other_name = [] && required.misplaced = []);
          check "required: the exit status is the passing one"
            (status required = 0));
      let path = Filename.concat directory "namespaced.jazz" in
      (match compare_file ~arch path with
      | Error message -> check ("namespaced: it runs (" ^ message ^ ")") false
      | Ok namespaced ->
          check "namespaced: the check accepts it" namespaced.accepted;
          check "namespaced: it holds typed uses" (namespaced.measured > 0);
          check "namespaced: every use is the compiler's declaration"
            (namespaced.same = namespaced.measured
            && namespaced.elsewhere = [] && namespaced.unresolved = []
            && namespaced.other_name = [] && namespaced.misplaced = []);
          check "namespaced: the exit status is the passing one"
            (status namespaced = 0));
      (* A file read into two namespaces: every use is paired with the reading it was
         typed in, so each declaration of [N] is the right answer for its reading. *)
      let twice_path = Filename.concat directory "twice.jazz" in
      (match compare_file ~arch twice_path with
      | Error message -> check ("twice: it runs (" ^ message ^ ")") false
      | Ok twice ->
          check "twice: the check accepts it" twice.accepted;
          check "twice: every reading agrees with the compiler"
            (twice.same = twice.measured
            && twice.elsewhere = [] && twice.unresolved = []
            && twice.other_name = [] && twice.misplaced = []);
          check "twice: the file's uses are measured in both readings"
            (twice.measured > 0));
      (* One reading right, one wrong, must fail. The compiler's use of [N] in the
         Long reading is given the Short declaration, the one the Short reading
         answers. The Short reading's use still agrees, so exactly one use must be
         reported, and the run must fail. *)
      let twice_text = Option.get (read twice_path) in
      let twice_file = I.normalise twice_path in
      let twice_facts =
        (A.check arch Arch.Linux ~file:twice_file ~text:twice_text ~idirs:[])
          .A.facts
      in
      let twice_resolve =
        R.of_text ~idirs:[] ~read ~file:twice_file twice_text
      in
      let sum_file = I.normalise (Filename.concat directory "sum.jinc") in
      let sum_text = Option.get (read sum_file) in
      let n_byte =
        match find_sub sum_text "to N" with Some at -> at + 3 | None -> -1
      in
      let short_byte =
        match find_sub twice_text "param int N = 1;" with
        | Some at -> at + 10
        | None -> -1
      in
      let long_use (use : F.use) =
        String.equal (I.normalise use.F.u_file) sum_file
        && use.F.u_span.S.start_byte = n_byte
        && String.equal use.F.u_reading "Long"
      in
      check "twice: the Long reading has a use of N in sum.jinc"
        (List.exists long_use twice_facts.F.uses);
      let wrong_facts =
        {
          twice_facts with
          F.uses =
            List.map
              (fun (use : F.use) ->
                if long_use use then
                  {
                    use with
                    F.u_decl =
                      S.make ~start_byte:short_byte ~end_byte:(short_byte + 1);
                    F.u_dfile = twice_file;
                  }
                else use)
              twice_facts.F.uses;
        }
      in
      let wrong =
        answer_of_facts ~document:twice_file ~resolve:twice_resolve wrong_facts
      in
      check "twice: exactly the wrong reading is reported"
        (List.length wrong.elsewhere = 1);
      check "twice: the report names the Long namespace"
        (List.exists
           (fun message ->
             Option.is_some (find_sub message "in namespace Long"))
           wrong.elsewhere);
      check "twice: one reading wrong fails the run"
        (status { wrong with accepted = true } = 1);
      let right =
        answer_of_facts ~document:twice_file ~resolve:twice_resolve twice_facts
      in
      check "twice: the unmodified facts agree in both readings"
        (right.elsewhere = [] && right.unresolved = [] && right.misplaced = []);
      (* A top-level required file's uses are in the facts, each in its own file and
         reading, and the resolver agrees with each of them. *)
      write "lib2.jinc" required_body;
      write "req-body.jazz" requirer_of_body;
      (match compare_file ~arch (Filename.concat directory "req-body.jazz") with
      | Error message ->
          check ("required body: it runs (" ^ message ^ ")") false
      | Ok body_run ->
          let from_required line =
            Option.is_some (find_sub line "\trequired\t")
          in
          let required_lines = List.filter from_required body_run.ledger in
          check "required body: the check accepts it" body_run.accepted;
          check "required body: the required file's uses are in the facts"
            (List.length required_lines >= 2);
          check "required body: the required uses name the required file"
            (List.for_all
               (fun line -> Option.is_some (find_sub line "lib2.jinc"))
               required_lines);
          check
            "required body: every use is the compiler's declaration in its \
             reading"
            (body_run.measured > 0
            && body_run.same = body_run.measured
            && body_run.elsewhere = [] && body_run.unresolved = []
            && body_run.other_name = [] && body_run.misplaced = []));
      (* The negative control: a planted binding is reported and fails the run,
         in the document's own use when it has one and in a required file's use
         when it has none. *)
      let planted_valid =
        compare_text ~plant:true ~arch ~file:"valid.jazz" valid
      in
      check "control: a use is planted in the document" planted_valid.planted;
      check "control: the planted use is reported"
        (List.length planted_valid.elsewhere
         + List.length planted_valid.unresolved
        = 1);
      check "control: the planted use fails the run" (status planted_valid = 1);
      write "only-require.jazz" "require \"lib2.jinc\"\n";
      (match
         read (Filename.concat directory "only-require.jazz")
         |> Option.map
              (compare_text ~plant:true ~arch
                 ~file:(Filename.concat directory "only-require.jazz"))
       with
      | None -> check "control: the require-only file is read" false
      | Some only ->
          check "control: the require-only file is accepted" only.accepted;
          check "control: a use is planted in the required file" only.planted;
          check "control: the required file's planted use fails the run"
            (status only = 1));
      let named = compare_file ~arch (Filename.concat directory "named.jazz") in
      match named with
      | Error message -> check ("named: it runs (" ^ message ^ ")") false
      | Ok named ->
          check "named: the check accepts a file required twice" named.accepted;
          check "named: every use is the compiler's declaration"
            (named.same = named.measured
            && named.elsewhere = [] && named.unresolved = []
            && named.other_name = [] && named.misplaced = []));
  if !failures = 0 then print_endline "typed_uses: selftest ok"
  else (
    Printf.eprintf "typed_uses: %d selftest check(s) failed\n" !failures;
    exit 1)

(* ------------------------------------------------------------------ *)

let arch_of_name name =
  match A.arch_of_string name with
  | Some arch -> Ok arch
  | None ->
      Error (Printf.sprintf "%S is not an architecture of the adapter" name)

(* ------------------------------------------------------------------ *)
(* The corpus run *)

module H = Jide_harness.Harness

(* What one check left: its answer, or why it gave none. *)
type checked = {
  relative : string;
  arch_name : string;
  result : (answer, string) result;
}

(* The distinct typed uses of the run. A use is keyed by its architecture,
   file, byte, reading and occurrence, so a library two checks reach is
   counted once; two checks that judge one use differently are a conflict. *)
type distinct = {
  by_class : (string, int) Hashtbl.t;
  by_group : (string * string, int) Hashtbl.t;
  typed : int;
  conflicts : int;
  required_uses : int;
  required_files : int;
  only_files : int;
  only_uses : int;
}

let distinct ~root (checks : checked list) =
  let answers =
    List.filter_map
      (fun c -> match c.result with Ok a -> Some (c, a) | Error _ -> None)
      checks
  in
  let documents =
    List.map
      (fun (c, _) -> I.normalise (Filename.concat root c.relative))
      answers
  in
  let lines =
    List.concat_map
      (fun (c, a) ->
        List.map
          (fun line -> (c.arch_name, String.split_on_char '\t' line))
          a.ledger)
      answers
  in
  let reached = Hashtbl.create 16 in
  List.iter
    (function
      | _, _ :: "required" :: file :: _ -> Hashtbl.replace reached file ()
      | _ -> ())
    lines;
  let seen = Hashtbl.create 4096 in
  let by_class = Hashtbl.create 8 and by_group = Hashtbl.create 64 in
  let bump table key =
    Hashtbl.replace table key
      (1 + Option.value ~default:0 (Hashtbl.find_opt table key))
  in
  let typed = ref 0 and conflicts = ref 0 in
  let required_uses = ref 0 and only_uses = ref 0 in
  let only = Hashtbl.create 16 in
  List.iter
    (fun (arch, fields) ->
      match fields with
      | cls :: _origin :: file :: start :: reading :: index :: _ -> (
          let key = (arch, file, start, reading, index) in
          match Hashtbl.find_opt seen key with
          | Some known -> if known <> cls then incr conflicts
          | None ->
              Hashtbl.add seen key cls;
              incr typed;
              bump by_class cls;
              let relative =
                if String.length file > String.length root then
                  String.sub file
                    (String.length root + 1)
                    (String.length file - String.length root - 1)
                else file
              in
              let group =
                match String.split_on_char '/' relative with
                | _ :: second :: _ -> second
                | _ -> ""
              in
              bump by_group (group, "typed");
              bump by_group (group, cls);
              if Hashtbl.mem reached file then (
                incr required_uses;
                if not (List.mem file documents) then (
                  incr only_uses;
                  Hashtbl.replace only file ())))
      | _ -> ())
    lines;
  {
    by_class;
    by_group;
    typed = !typed;
    conflicts = !conflicts;
    required_uses = !required_uses;
    required_files = Hashtbl.length reached;
    only_files = Hashtbl.length only;
    only_uses = !only_uses;
  }

let corpus_usage =
  "usage: typed_uses --corpus <dir> [--checks <list>] [--jobs N] \
   [--negative-control]\n\n\
   Every .jazz under <dir>/success (a release root, its compiler or its tests \
   directory),\n\
   once per architecture its path names, or the checks of <list>."

let corpus_run ~negative (args : H.args) =
  H.name := "uses";
  let root = H.tests_root (Option.value ~default:"" (args.value "--corpus")) in
  let checks =
    match args.value "--checks" with
    | Some list -> H.read_checks list
    | None ->
        List.concat_map
          (fun relative ->
            List.map
              (fun arch -> { H.label = "accept"; path = relative; arch })
              (H.arches_of relative))
          (List.map (( ^ ) "success/")
             (H.files (Filename.concat root "success")))
  in
  if checks = [] then
    H.die "no checks: the corpus holds no .jazz under its success folder";
  List.iter
    (fun (c : H.check) ->
      if not (Sys.file_exists (Filename.concat root c.path)) then
        H.die "no such file under %s: %s" root c.path)
    checks;
  let results =
    H.parallel ~jobs:(H.jobs args)
      (fun (c : H.check) ->
        match arch_of_name c.arch with
        | Error message -> failwith message
        | Ok arch -> (
            let path = Filename.concat root c.path in
            match read path with
            | None -> failwith ("cannot read " ^ path)
            | Some text -> compare_text ~plant:negative ~arch ~file:path text))
      checks
  in
  let checked =
    List.map2
      (fun (c : H.check) result ->
        { relative = c.path; arch_name = c.arch; result })
      checks results
  in
  let d = distinct ~root:(I.normalise root) checked in
  let class_count cls =
    Option.value ~default:0 (Hashtbl.find_opt d.by_class cls)
  in
  let same = class_count "same" and elsewhere = class_count "elsewhere" in
  let unresolved = class_count "unresolved"
  and other = class_count "other-name" in
  let misplaced = class_count "misplaced"
  and unchecked = class_count "unchecked" in
  let flag = class_count "excluded-flag"
  and temporary = class_count "excluded-temporary" in
  let measured = same + elsewhere + unresolved + other + misplaced in
  let excluded = flag + temporary in
  (* Every check's own verdict, and the lines it adds to the report. *)
  let accepted = ref 0 and rejected = ref 0 and broken = ref 0 in
  let measurable = ref 0 and detected = ref 0 in
  let mismatches = ref [] and excluded_lines = ref [] and undetected = ref [] in
  let add list line = if not (List.mem line !list) then list := line :: !list in
  List.iter
    (fun c ->
      let where = Printf.sprintf "%s (%s)" c.relative c.arch_name in
      match c.result with
      | Error why ->
          incr broken;
          add mismatches
            (Printf.sprintf "%s: the comparison answered nothing (%s)" where why)
      | Ok a ->
          let differences =
            List.length a.elsewhere + List.length a.unresolved
            + List.length a.other_name + List.length a.misplaced
          in
          if a.accepted then (
            incr accepted;
            if a.measured > 0 then (
              incr measurable;
              if a.planted && differences > 0 then incr detected
              else
                add undetected
                  (Printf.sprintf "%s: planted %s, %d difference(s) reported"
                     where
                     (if a.planted then "yes" else "no")
                     differences)))
          else (
            incr rejected;
            add mismatches
              (Printf.sprintf "%s: the check does not accept it: %s" where
                 (Option.value ~default:"no error printed" a.first_diagnostic)));
          let lines kind list messages =
            List.iter
              (fun m -> add list (Printf.sprintf "%s %s %s" where kind m))
              messages
          in
          lines "elsewhere" mismatches a.elsewhere;
          lines "unresolved" mismatches a.unresolved;
          lines "other-name" mismatches a.other_name;
          lines "misplaced" mismatches a.misplaced;
          lines "excluded-flag" excluded_lines a.excluded_flag;
          lines "excluded-temporary" excluded_lines a.excluded_temporary)
    checked;
  let files =
    List.length
      (List.sort_uniq compare (List.map (fun c -> c.relative) checked))
  in
  if args.value "--checks" <> None then
    H.say "the check list names %d checks" (List.length checks);
  H.say "%d files, %d checks; the check accepts %d, refuses %d" files
    (List.length checks) !accepted !rejected;
  H.say
    "distinct typed variable uses per architecture: %d; measured: %d; the \
     compiler's declaration in its namespace: %d; elsewhere: %d; unresolved: \
     %d; other-name: %d; misplaced: %d; excluded: %d (flag %d, temporary %d); \
     unchecked: %d"
    d.typed measured same elsewhere unresolved other misplaced excluded flag
    temporary unchecked;
  H.say
    "uses of files a top-level require reaches: %d (in %d distinct files; %d \
     of them are not checked directly, with %d uses)"
    d.required_uses d.required_files d.only_files d.only_uses;
  if measured + excluded + unchecked <> d.typed then
    H.die
      "the counts do not add up: %d measured + %d excluded + %d unchecked = \
       %d, not %d"
      measured excluded unchecked
      (measured + excluded + unchecked)
      d.typed;
  if d.conflicts > 0 then
    if negative then
      H.say
        "negative control: %d use(s) are judged differently by two checks of \
         one architecture, since a planted use of a file is reached unplanted \
         from another check"
        d.conflicts
    else
      H.die
        "%d use(s) are judged differently by two checks of one architecture: \
         the comparison is not consistent"
        d.conflicts;
  if measured > 0 then
    H.say
      "the gate's measure: %d of %d measured uses are the compiler's \
       declaration in their own namespace (%.4f%%)"
      same measured
      (100. *. float same /. float measured);
  if d.typed > 0 then (
    H.say
      "by folder: typed, measured, same, elsewhere, unresolved, other-name, \
       misplaced, excluded, unchecked";
    let groups =
      List.sort_uniq compare
        (Hashtbl.fold (fun (group, _) _ acc -> group :: acc) d.by_group [])
    in
    List.iter
      (fun group ->
        let n cls =
          Option.value ~default:0 (Hashtbl.find_opt d.by_group (group, cls))
        in
        let g_measured =
          n "same" + n "elsewhere" + n "unresolved" + n "other-name"
          + n "misplaced"
        in
        H.say
          "  %s: %d typed, %d measured, %d same, %d elsewhere, %d unresolved, \
           %d other-name, %d misplaced, %d excluded, %d unchecked"
          group (n "typed") g_measured (n "same") (n "elsewhere")
          (n "unresolved") (n "other-name") (n "misplaced")
          (n "excluded-flag" + n "excluded-temporary")
          (n "unchecked"))
      groups);
  if !excluded_lines <> [] then (
    H.say
      "excluded uses: the name is not an identifier token anywhere in the \
       file, so the compiler made the use up; they are listed, not measured";
    List.iter (fun line -> H.say "%s" line) (List.rev !excluded_lines));
  if not negative then
    List.iter (fun line -> H.say "different %s" line) (List.rev !mismatches);
  if !broken > 0 then (
    H.say "%d check(s) gave no answer: the comparison is not working" !broken;
    2)
  else if d.typed = 0 || measured = 0 then (
    H.say
      "the corpus holds no measured typed variable use: nothing was measured";
    2)
  else if negative then
    if !measurable > 0 && !undetected = [] then (
      H.say
        "negative control: a wrong binding was planted in each of the %d \
         accepted checks that have a measured use, and the comparison reported \
         it in %d; the run fails, as it must"
        !measurable !detected;
      1)
    else (
      List.iter (fun line -> H.say "unreported %s" line) (List.rev !undetected);
      H.say
        "negative control broken: the planted wrong binding was reported in %d \
         of %d accepted checks with a measured use"
        !detected !measurable;
      2)
  else if
    !rejected = 0 && elsewhere = 0 && unresolved = 0 && other = 0
    && misplaced = 0 && unchecked = 0
  then (
    H.say
      "every measured typed use of every accepted file is the compiler's \
       declaration in its own namespace";
    0)
  else (
    H.say "the corpus does not agree";
    1)

let main () =
  let args = List.tl (Array.to_list Sys.argv) in
  match args with
  | [ "--selftest" ] -> check_selftest ()
  | _ when List.mem "--corpus" args ->
      (* The measurement, then the negative control over the same checks,
         which must fail the way a wrong binding fails; [--negative-control]
         runs the control alone. *)
      let args =
        H.parse_args ~usage:corpus_usage ~flags:[ "--negative-control" ]
          ~options:[ "--corpus"; "--checks"; "--jobs" ]
          args
      in
      if args.flag "--negative-control" then
        exit (corpus_run ~negative:true args);
      let measured = corpus_run ~negative:false args in
      if measured <> 0 then exit measured;
      let control = corpus_run ~negative:true args in
      if control <> 1 then (
        H.say
          "the negative control exited %d, not 1: the comparison cannot be \
           trusted"
          control;
        exit 1);
      exit 0
  | _ -> (
      let negative = List.mem "--negative-control" args in
      let positional = List.filter (fun a -> a <> "--negative-control") args in
      match positional with
      | [ "--help" ] | [ "-help" ] ->
          print_endline usage;
          print_endline corpus_usage;
          exit 0
      | path :: rest -> (
          let name =
            match rest with [] -> "x86-64" | [ name ] -> name | _ -> ""
          in
          match arch_of_name name with
          | Error message ->
              Printf.eprintf "typed_uses: %s\n%s\n" message usage;
              exit 2
          | Ok arch -> (
              match read path with
              | None ->
                  Printf.eprintf "typed_uses: cannot read %s\n" path;
                  exit 2
              | Some text ->
                  let answer =
                    compare_text ~plant:negative ~arch ~file:path text
                  in
                  List.iter print_endline answer.ledger;
                  say_answer answer;
                  say_differences answer;
                  exit (status answer)))
      | [] ->
          prerr_endline usage;
          exit 2)

let () = main ()
