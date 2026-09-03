(* The four location features, hand-checked over small projects.

   Every expected range is computed from the text by this file, never read out
   of the index: [span_of_in text ~within ~name] finds the first place
   [within] sits in [text] and the first place [name] sits inside it. A test
   that passes therefore says the feature points at bytes a reader can see,
   not that it agrees with the walk it was built from.

   Four projects: a single file with a namespace, an alias and two locals of
   one name; an entry that requires a library; the same pair with a UTF-8
   byte-order mark on both files; and a file that does not parse. *)

open Jide_engine

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* ------------------------------------------------------------------ *)
(* Reading a text by the fragments a reader sees *)

let byte_of text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then None
    else if String.equal (String.sub text index fragment_length) fragment then
      Some index
    else from (index + 1)
  in
  if fragment_length = 0 then Some 0 else from 0

let byte_of_in text ~within ~name =
  match byte_of text within with
  | None -> None
  | Some start -> (
      let window = String.sub text start (String.length within) in
      match byte_of window name with
      | None -> None
      | Some offset -> Some (start + offset))

let span_of_in text ~within ~name =
  match byte_of_in text ~within ~name with
  | None -> None
  | Some start ->
      Some
        (Jide_lang.Span.make ~start_byte:start
           ~end_byte:(start + String.length name))

(* The byte a fragment starts at, and the range the name covers. Both fail the
   run when the fragment is not there, because a test whose text moved must
   not pass quietly. *)
let at text ~within ~name =
  match byte_of_in text ~within ~name with
  | Some byte -> byte
  | None ->
      incr failures;
      Printf.printf "FAIL the text has no %S inside %S\n%!" name within;
      -1

let expected text ~within ~name =
  match span_of_in text ~within ~name with
  | Some span -> span
  | None ->
      incr failures;
      Printf.printf "FAIL the text has no %S inside %S\n%!" name within;
      Jide_lang.Span.make ~start_byte:(-1) ~end_byte:(-1)

let uri_of path = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)

let is_at (location : Types.location) ~file ~span =
  String.equal location.uri (uri_of file) && location.span = span

let spans (locations : Types.location list) =
  List.map (fun (location : Types.location) -> location.span) locations

let files_of (locations : Types.location list) =
  List.map (fun (location : Types.location) -> location.uri) locations

(* The outline written as [name[children], name], so one string says both the
   names and the nesting. *)
let outline_text (symbols : Types.symbol list) =
  let rec one (symbol : Types.symbol) =
    match symbol.children with
    | [] -> symbol.name
    | children ->
        symbol.name ^ "[" ^ String.concat ", " (List.map one children) ^ "]"
  in
  String.concat ", " (List.map one symbols)

let kind_text (kind : Types.symbol_kind) =
  match kind with
  | Types.Function -> "function"
  | Parameter -> "parameter"
  | Variable -> "variable"
  | Global -> "global"
  | Namespace -> "namespace"
  | Type_alias -> "type alias"
  | Require -> "require"
  | Exec -> "exec"
  | File -> "file"
  | Unknown -> "unknown"

let hover_text (hover : Types.hover option) =
  match hover with
  | None -> ""
  | Some hover ->
      String.concat "\n"
        (List.map (fun (m : Types.markdown) -> m.value) hover.contents)

(* ------------------------------------------------------------------ *)
(* The projects *)

let names =
  "type word = u32[4];\n\n\
   fn double(reg u32 a) -> reg u32 {\n\
  \  reg u32 result;\n\
  \  result = a + a;\n\
  \  return result;\n\
   }\n\n\
   namespace box {\n\
  \  fn put(reg u64 v) -> reg u64 {\n\
  \    reg u64 stored;\n\
  \    stored = v;\n\
  \    return stored;\n\
  \  }\n\
   }\n\n\
   fn caller(reg u32 a) -> reg u32 {\n\
  \  reg word w;\n\
  \  reg u32 x;\n\
  \  x = double(a);\n\
  \  x = double(x);\n\
  \  x = box::put(a);\n\
  \  return x;\n\
   }\n\n\
   fn negate(reg u32 a) -> reg u32 {\n\
  \  reg u32 result;\n\
  \  result = a - a;\n\
  \  return result;\n\
   }\n"

let names_file = "/work/names.jazz"

let entry =
  "require \"lib.jinc\"\n\n\
   fn main(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = lib_f(a);\n\
  \  x = lib_f(x);\n\
  \  return x;\n\
   }\n"

let entry_file = "/work/entry.jazz"

let library =
  "fn lib_f(reg u32 a) -> reg u32 {\n\
  \  reg u32 tmp;\n\
  \  tmp = a;\n\
  \  return tmp;\n\
   }\n"

let library_file = "/work/lib.jinc"
let marked_entry = "\xEF\xBB\xBF" ^ entry
let marked_library = "\xEF\xBB\xBF" ^ library
let read_of files path = List.assoc_opt (Filename.basename path) files

let single file text =
  Jide_engine.Locate.of_text ~idirs:[] ~read:(read_of []) ~file text

let project ?facts ~files file text =
  match facts with
  | None ->
      Jide_engine.Locate.of_text ~idirs:[] ~read:(read_of files) ~file text
  | Some facts ->
      Jide_engine.Locate.of_text ~facts ~idirs:[] ~read:(read_of files) ~file
        text

(* The facts of a real check, which is the compiler's own answer. *)
let checked_facts ~file text =
  let arch = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64") in
  let call_conv =
    Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")
  in
  let result = Jide_jasmin.Adapter.check arch call_conv ~file ~text ~idirs:[] in
  result.Jide_jasmin.Adapter.facts

(* ------------------------------------------------------------------ *)
(* Project one: one file, a namespace, an alias, two locals of one name *)

let test_definition_of_a_function () =
  let locate = single names_file names in
  let call =
    Jide_engine.Locate.definition locate ~file:names_file
      ~byte:(at names ~within:"x = double(a);" ~name:"double")
  in
  let declared_at_its_name =
    Jide_engine.Locate.definition locate ~file:names_file
      ~byte:(at names ~within:"fn double(reg u32 a)" ~name:"double")
  in
  let on_the_parameter =
    Jide_engine.Locate.definition locate ~file:names_file
      ~byte:(at names ~within:"fn double(reg u32 a)" ~name:"a")
  in
  check "definition: a call points at the declaration's name"
    (match call with
    | Some location ->
        is_at location ~file:names_file
          ~span:(expected names ~within:"fn double(reg u32 a)" ~name:"double")
    | None -> false);
  check "definition: the declaration itself points at itself"
    (match declared_at_its_name with
    | Some location ->
        is_at location ~file:names_file
          ~span:(expected names ~within:"fn double(reg u32 a)" ~name:"double")
    | None -> false);
  check "definition: a cursor on a parameter is that parameter"
    (match on_the_parameter with
    | Some location ->
        is_at location ~file:names_file
          ~span:(expected names ~within:"fn double(reg u32 a)" ~name:"a")
    | None -> false);
  check "definition: nothing to go to on a keyword"
    (Jide_engine.Locate.definition locate ~file:names_file ~byte:0 = None);
  check "definition: a file the resolution never reached has nothing"
    (Jide_engine.Locate.definition locate ~file:"/work/other.jazz" ~byte:0
    = None)

let test_definition_of_a_type_alias () =
  let locate = single names_file names in
  let use =
    Jide_engine.Locate.definition locate ~file:names_file
      ~byte:(at names ~within:"reg word w;" ~name:"word")
  in
  check "alias: a use of the alias points at the alias"
    (match use with
    | Some location ->
        is_at location ~file:names_file
          ~span:(expected names ~within:"type word = u32[4];" ~name:"word")
    | None -> false)

let test_definition_through_a_namespace () =
  let locate = single names_file names in
  let put = expected names ~within:"fn put" ~name:"put" in
  let same = function
    | Some location -> is_at location ~file:names_file ~span:put
    | None -> false
  in
  check "namespace: the last segment of a qualified name is the declaration"
    (same
       (Jide_engine.Locate.definition locate ~file:names_file
          ~byte:(at names ~within:"x = box::put(a);" ~name:"put")));
  check "namespace: the whole qualified name is the declaration"
    (same
       (Jide_engine.Locate.definition locate ~file:names_file
          ~byte:(at names ~within:"x = box::put(a);" ~name:"box::put")));
  check "namespace: the first segment is the same declaration"
    (same
       (Jide_engine.Locate.definition locate ~file:names_file
          ~byte:(at names ~within:"x = box::put(a);" ~name:"box")))

