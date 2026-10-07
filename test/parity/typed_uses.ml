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
    wrong one in the other fails on exactly the wrong use. The identity of a declaration
    is the design's (section 9): the normalised file and the byte where its name starts.

    {2 The classes}

    Every typed use of the checked file falls in exactly one class:

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
    use of the file is moved to byte 0. The run must then report that one use, which
    proves the comparison fails when a binding is wrong. The script that runs it expects
    the failure.

    Stdout carries nothing. Stderr carries the counts, then one line per difference and
    one per excluded use. A position is one-based, [line:column].

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

(** [answer_of_facts ~resolve facts] is every typed use of [facts] against [resolve],
    each in the reading it was typed in, with the classes counted. *)
let answer_of_facts ~(resolve : R.t) (facts : F.t) : answer =
  let lines_of = line_indexes resolve in
  let measured = ref 0 and same = ref 0 in
  let elsewhere = ref [] and unresolved = ref [] and other_name = ref [] in
  let misplaced = ref [] and excluded_flag = ref [] in
  let excluded_temporary = ref [] in
  let add cell message = cell := message :: !cell in
  (* Pass one: each use is measured, misplaced, excluded, or waits for its reading. *)
  let waiting = ref [] in
  List.iter
    (fun (use : F.use) ->
      let file = I.normalise use.F.u_file in
      let start = use.F.u_span.S.start_byte in
      let stop = use.F.u_span.S.end_byte in
      let name = last_segment use.F.u_name in
      match text_of resolve file with
      | None ->
          incr measured;
          add unresolved
            (Printf.sprintf "%s %s: the resolution never reached the file"
               (describe lines_of file start)
               use.F.u_name)
      | Some text -> (
          match find_token text ~from:start ~until:stop name with
          | Some byte -> waiting := (use, file, byte) :: !waiting
          | None -> (
              match
                find_token text ~from:0 ~until:(String.length text) name
              with
              | Some written_at ->
                  incr measured;
                  add misplaced
                    (Printf.sprintf
                       "%s %s: the compiler's span holds no identifier of the \
                        name, but the file writes it at %s"
                       (describe lines_of file start)
                       use.F.u_name
                       (describe lines_of file written_at))
              | None ->
                  let shown =
                    Printf.sprintf "%s %s"
                      (describe lines_of file start)
                      use.F.u_name
                  in
                  if is_flag_name name then add excluded_flag shown
                  else add excluded_temporary shown)))
    facts.F.uses;
  (* Pass two: each measured use against the answer of its own reading. *)
  List.iter
    (fun ((use : F.use), file, byte) ->
      incr measured;
      let shown = describe lines_of file byte in
      let reading = use.F.u_reading in
      if not (List.mem reading (R.readings resolve file)) then
        add unresolved
          (Printf.sprintf "%s %s: the resolver never walked the file in %s"
             shown use.F.u_name (reading_name reading))
      else
        match R.reading_answer resolve ~file ~byte ~reading with
        | None ->
            add unresolved
              (Printf.sprintf "%s %s: no occurrence at that byte" shown
                 use.F.u_name)
        | Some (occurrence, answer) -> (
            if
              not
                (String.equal
                   (last_segment occurrence.I.name)
                   (last_segment use.F.u_name))
            then
              add other_name
                (Printf.sprintf "%s %s: the occurrence at that byte is %s" shown
                   use.F.u_name occurrence.I.name)
            else
              match answer with
              | R.Resolved declaration when same_declaration use declaration ->
                  incr same
              | R.Resolved declaration ->
                  add elsewhere
                    (Printf.sprintf
                       "%s %s in %s: the compiler declares it at %s, the \
                        resolver answers %s"
                       shown use.F.u_name (reading_name reading)
                       (describe lines_of use.F.u_dfile
                          use.F.u_decl.S.start_byte)
                       (describe lines_of declaration.I.file
                          declaration.I.name_span.S.start_byte))
              | other ->
                  add unresolved
                    (Printf.sprintf "%s %s in %s: the resolver answers %s" shown
                       use.F.u_name (reading_name reading)
                       (name_of_answer other))))
    (List.rev !waiting);
  {
    empty_answer with
    uses = List.length facts.F.uses;
    measured = !measured;
    same = !same;
    elsewhere = List.rev !elsewhere;
    unresolved = List.rev !unresolved;
    other_name = List.rev !other_name;
    misplaced = List.rev !misplaced;
    excluded_flag = List.rev !excluded_flag;
    excluded_temporary = List.rev !excluded_temporary;
  }

(* [with_decl ~byte use] is [use] with its compiler-side declaration moved to [byte]:
   a fact the compiler would not produce, used to prove the comparison answers a
   difference. *)
let with_decl ~(byte : int) (use : F.use) : F.use =
  { use with F.u_decl = S.make ~start_byte:byte ~end_byte:(byte + 1) }

(* [plant_first_measured ~file ~body facts] moves the declaration of the first use the
   source writes in [file] to byte 0: the negative control. A planted use the
   comparison does not measure would not show the control working. *)
let plant_first_measured ~(file : string) ~(body : string) (facts : F.t) : F.t =
  let measured (use : F.use) =
    String.equal (I.normalise use.F.u_file) file
    && find_token body ~from:use.F.u_span.S.start_byte
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
          with_decl ~byte:0 use)
        else use)
      facts.F.uses
  in
  { facts with F.uses }

(** [compare_text ?plant ~arch ~file text] is the whole comparison for one text: the
    check, the resolver over the same bytes, and every typed use. With [plant] the
    negative control is applied first. *)
let compare_text ?(plant = false) ~(arch : A.arch) ~(file : string)
    (text : string) : answer =
  let file = I.normalise file in
  let _bom, body = strip_bom text in
  let checked = A.check arch Arch.Linux ~file ~text:body ~idirs:[] in
  let facts =
    if not plant then checked.A.facts
    else
      (* The first use the source writes in this file: a planted use the
         comparison does not measure would not show the control working. *)
      let measured (use : F.use) =
        String.equal (I.normalise use.F.u_file) file
        && spelled_in body ~start:use.F.u_span.S.start_byte
             ~stop:use.F.u_span.S.end_byte
             (last_segment use.F.u_name)
           <> None
      in
      let planted = ref false in
      let uses =
        List.map
          (fun (use : F.use) ->
            if (not !planted) && measured use then (
              planted := true;
              with_decl ~byte:0 use)
            else use)
          checked.A.facts.F.uses
      in
      { checked.A.facts with F.uses }
  in
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
  if not base.accepted then { base with unchecked = uses }
  else
    let resolve = R.of_text ~idirs:[] ~read ~file body in
    { (answer_of_facts ~resolve facts) with accepted = true }

let compare_file ~(arch : A.arch) (path : string) =
  match read path with
  | None -> Error (Printf.sprintf "cannot read %s" path)
  | Some text -> Ok (compare_text ~arch ~file:path text)

(* ------------------------------------------------------------------ *)
(* The report *)

let status (answer : answer) =
  if
    answer.accepted && answer.elsewhere = [] && answer.unresolved = []
    && answer.other_name = [] && answer.unchecked = 0
  then 0
  else 1

let say_answer (answer : answer) =
  Printf.eprintf "accepted: %s\n" (if answer.accepted then "yes" else "no");
  Printf.eprintf "errors: %d\n" answer.diagnostics;
  (match answer.first_diagnostic with
  | None -> ()
  | Some message -> Printf.eprintf "first-error: %s\n" message);
  Printf.eprintf "typed-uses: %d\n" answer.uses;
  Printf.eprintf "source-written: %d\n" answer.measured;
  Printf.eprintf "same-declaration: %d\n" answer.same;
  Printf.eprintf "elsewhere: %d\n" (List.length answer.elsewhere);
  Printf.eprintf "unresolved: %d\n" (List.length answer.unresolved);
  Printf.eprintf "other-name: %d\n" (List.length answer.other_name);
  Printf.eprintf "unlocated: %d\n" (List.length answer.unlocated);
  Printf.eprintf "unchecked: %d\n" answer.unchecked;
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
  line "unlocated" answer.unlocated;
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
   not accepted and its uses are not measured. *)
let untyped =
  String.concat "\n"
    [ "export fn main() -> reg u64 {"; "  y = 1;"; "  return y;"; "}"; "" ]

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

(* A qualified use: the compiler keeps [A::x] for it while the source writes
   the last segment at the byte, so the comparison is by the segment. *)
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
   offsets, so the file of each use is the required file's. *)
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

(* The flag groups of a multi-result instruction: [?{cf}] names the variable
   that receives the flag, and [CF=cf] names the flag and the variable. The
   resolver answered both as annotations before the flag groups were read as
   variable uses. *)
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

(* A required file read into two namespaces: the file's own [N] is the
   namespace's, so the two readings of one byte name different declarations. *)
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

(* A required file named twice, once bare and once with a [./]: one file, read
   once per namespace, as the compiler reads it. *)
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
   && good.other_name = []);
  check "valid: the exit status is the passing one" (status good = 0);
  (* The comparison is a comparison and not a constant: moving one use's
     compiler-side declaration to another byte makes it a difference. *)
  let one_use_moved =
    let checked =
      A.check arch Arch.Linux ~file:"valid.jazz" ~text:valid ~idirs:[]
    in
    match checked.A.facts.F.uses with
    | [] -> empty_answer
    | first :: rest ->
        let facts =
          {
            F.items = checked.A.facts.F.items;
            uses = with_decl ~byte:1 first :: rest;
          }
        in
        let resolve = R.of_text ~idirs:[] ~read ~file:"valid.jazz" valid in
        answer_of_facts ~resolve facts
  in
  check "a moved declaration is a difference"
    (one_use_moved.elsewhere <> [] || one_use_moved.unresolved <> []);
  check "a moved declaration fails the run"
    (status { one_use_moved with accepted = true } = 1);
  (* A use the compiler made up itself: its span is an instruction's and the
     bytes spell no such name, so it is not measured and never fails a run. *)
  let made_up =
    let checked =
      A.check arch Arch.Linux ~file:"valid.jazz" ~text:valid ~idirs:[]
    in
    match checked.A.facts.F.uses with
    | [] -> empty_answer
    | first :: _ ->
        let use = { first with F.u_name = "_zf_" } in
        let resolve = R.of_text ~idirs:[] ~read ~file:"valid.jazz" valid in
        answer_of_facts ~resolve { F.items = []; uses = [ use ] }
  in
  check "a made-up use is not measured" (made_up.measured = 0);
  check "a made-up use is listed as unlocated"
    (List.length made_up.unlocated = 1);
  check "a made-up use is not a difference"
    (made_up.unresolved = [] && made_up.elsewhere = []
   && made_up.other_name = []);
  check "a made-up use does not fail the run"
    (status { made_up with accepted = true } = 0);
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
  let flagged = compare_text ~arch ~file:"flags.jazz" flags in
  check "flags: the check accepts it" flagged.accepted;
  check "flags: it holds typed uses" (flagged.measured > 0);
  check "flags: every flag group's variable is the compiler's declaration"
    (flagged.same = flagged.measured
    && flagged.elsewhere = [] && flagged.unresolved = []
    && flagged.other_name = []);
  (* The files a directory holds: the requirer and its library, the body a
     namespace requires, the namespaces that read one file twice, and a file
     named twice. The compiler's answer is the same whether the test runs
     alone or among the rest. *)
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
            && required.other_name = [] && required.unlocated = []);
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
            && namespaced.other_name = []);
          check "namespaced: the exit status is the passing one"
            (status namespaced = 0));
      (* A file read into two namespaces: every reading of each byte of
         sum.jinc is compared with the compiler's use of that reading, so the
         two declarations of [N] are each the right answer for their reading. *)
      let path = Filename.concat directory "twice.jazz" in
      (match compare_file ~arch path with
      | Error message -> check ("twice: it runs (" ^ message ^ ")") false
      | Ok twice ->
          check "twice: the check accepts it" twice.accepted;
          check "twice: every reading agrees with the compiler"
            (twice.same = twice.measured
            && twice.elsewhere = [] && twice.unresolved = []
            && twice.other_name = []);
          check "twice: the file's uses are measured in both readings"
            (twice.measured > 0);
          let resolve =
            R.of_text ~idirs:[] ~read ~file:(I.normalise path)
              (Option.get (read path))
          in
          let sum_path = I.normalise (Filename.concat directory "sum.jinc") in
          let sum_text = Option.get (read sum_path) in
          let byte =
            let rec find index =
              if index + 1 >= String.length sum_text then -1
              else if String.sub sum_text index 1 = "N" then index
              else find (index + 1)
            in
            find 0
          in
          let _occurrence, readings =
            R.visit_answers resolve ~file:sum_path ~byte
          in
          check "twice: the file has one reading per namespace"
            (List.length readings = 2);
          check "twice: the readings name different declarations"
            (match readings with
            | [ R.Resolved first; R.Resolved second ] ->
                first.I.name_span.S.start_byte
                <> second.I.name_span.S.start_byte
            | _ -> false));
      (* Two requires of one file, spelled two ways: read once, as the
         compiler reads it, so its functions are declared once. *)
      let path = Filename.concat directory "named.jazz" in
      match compare_file ~arch path with
      | Error message -> check ("named: it runs (" ^ message ^ ")") false
      | Ok named ->
          check "named: the check accepts a file required twice" named.accepted;
          check "named: every use is the compiler's declaration"
            (named.same = named.measured
            && named.elsewhere = [] && named.unresolved = []
            && named.other_name = []));
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

let main () =
  let args = List.tl (Array.to_list Sys.argv) in
  let negative = List.mem "--negative-control" args in
  let positional = List.filter (fun a -> a <> "--negative-control") args in
  match positional with
  | [ "--selftest" ] -> check_selftest ()
  | [ "--help" ] | [ "-help" ] ->
      print_endline usage;
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
              let answer = compare_text ~plant:negative ~arch ~file:path text in
              say_answer answer;
              say_differences answer;
              exit (status answer)))
  | [] ->
      prerr_endline usage;
      exit 2

let () = main ()
