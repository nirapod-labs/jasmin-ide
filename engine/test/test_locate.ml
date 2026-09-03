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

let test_references_of_a_local () =
  let locate = single names_file names in
  let byte = at names ~within:"reg u32 result;" ~name:"result" in
  let all =
    Jide_engine.Locate.references locate ~file:names_file ~byte
      ~include_declaration:true
  in
  let without_the_declaration =
    Jide_engine.Locate.references locate ~file:names_file ~byte
      ~include_declaration:false
  in
  let declaration = expected names ~within:"reg u32 result;" ~name:"result" in
  let write = expected names ~within:"result = a + a;" ~name:"result" in
  let read = expected names ~within:"a + a;\n  return result;" ~name:"result" in
  check "references: the declaration and both of its uses, in source order"
    (spans all = [ declaration; write; read ]);
  check "references: without the declaration there are two"
    (spans without_the_declaration = [ write; read ]);
  check "references: every one names the file it is in"
    (files_of all = [ uri_of names_file; uri_of names_file; uri_of names_file ]);
  check "references: a keyword has none"
    (Jide_engine.Locate.references locate ~file:names_file ~byte:0
       ~include_declaration:true
    = [])

(* Two variables of one name are two declarations, and neither answers for
   the other. *)
let test_two_declarations_of_one_name () =
  let locate = single names_file names in
  let in_double =
    Jide_engine.Locate.references locate ~file:names_file
      ~byte:(at names ~within:"reg u32 result;" ~name:"result")
      ~include_declaration:true
  in
  let negate_declaration =
    expected names
      ~within:"fn negate(reg u32 a) -> reg u32 {\n  reg u32 result;"
      ~name:"result"
  in
  let in_negate =
    Jide_engine.Locate.references locate ~file:names_file
      ~byte:
        (at names ~within:"fn negate(reg u32 a) -> reg u32 {\n  reg u32 result;"
           ~name:"result")
      ~include_declaration:true
  in
  let negate_spans =
    [
      negate_declaration;
      expected names ~within:"result = a - a;" ~name:"result";
      expected names ~within:"a - a;\n  return result;" ~name:"result";
    ]
  in
  check "identity: the local in the second function has its own three"
    (spans in_negate = negate_spans);
  check "identity: the two locals share no reference"
    (List.for_all
       (fun span -> not (List.mem span negate_spans))
       (spans in_double));
  check "identity: a use of the second local points at the second declaration"
    (match
       Jide_engine.Locate.definition locate ~file:names_file
         ~byte:(at names ~within:"result = a - a;" ~name:"result")
     with
    | Some location -> is_at location ~file:names_file ~span:negate_declaration
    | None -> false)

let test_the_outline_nests_namespaces () =
  let locate = single names_file names in
  let symbols = Jide_engine.Locate.symbols locate ~file:names_file in
  check "outline: the items in source order, the namespace's inside it"
    (String.equal (outline_text symbols)
       "word, double, box[put], caller, negate");
  check "outline: each item's kind"
    (List.map kind_text (List.map (fun (s : Types.symbol) -> s.kind) symbols)
    = [ "type alias"; "function"; "namespace"; "function"; "function" ]);
  check "outline: the namespace holds one function"
    (match List.find_opt (fun (s : Types.symbol) -> s.name = "box") symbols with
    | Some box -> (
        match box.children with
        | [ put ] -> String.equal put.name "put" && put.kind = Types.Function
        | _ -> false)
    | None -> false);
  check
    "outline: a function carries the signature the index cut from the source"
    (match
       List.find_opt (fun (s : Types.symbol) -> s.name = "double") symbols
     with
    | Some double -> double.detail = Some "fn double(reg u32 a) -> reg u32"
    | None -> false);
  check "outline: the name range is the name and the item range holds it"
    (match
       List.find_opt (fun (s : Types.symbol) -> s.name = "caller") symbols
     with
    | Some caller ->
        caller.name_span = expected names ~within:"fn caller" ~name:"caller"
        && caller.span.start_byte <= caller.name_span.start_byte
        && caller.span.end_byte >= caller.name_span.end_byte
    | None -> false);
  check "outline: a file the resolution never reached has none"
    (Jide_engine.Locate.symbols locate ~file:"/work/other.jazz" = [])

let test_hover_from_the_index () =
  let locate = single names_file names in
  let at_call =
    Jide_engine.Locate.hover locate ~file:names_file
      ~byte:(at names ~within:"x = double(a);" ~name:"double")
  in
  let at_local =
    Jide_engine.Locate.hover locate ~file:names_file
      ~byte:(at names ~within:"result = a + a;" ~name:"result")
  in
  let at_alias =
    Jide_engine.Locate.hover locate ~file:names_file
      ~byte:(at names ~within:"reg word w;" ~name:"word")
  in
  check "hover: a function's signature, in a fenced block"
    (String.equal (hover_text at_call)
       "```jasmin\nfn double(reg u32 a) -> reg u32\n```");
  check "hover: the range is the occurrence under the cursor"
    (match at_call with
    | Some hover ->
        hover.span = expected names ~within:"x = double(a);" ~name:"double"
    | None -> false);
  check "hover: a local's storage and type, with its name"
    (String.equal (hover_text at_local) "```jasmin\nreg u32 result\n```");
  check "hover: an alias as it was written"
    (String.equal (hover_text at_alias) "```jasmin\ntype word = u32[4]\n```");
  check "hover: a keyword says nothing"
    (Jide_engine.Locate.hover locate ~file:names_file ~byte:0 = None);
  check "hover: a namespace names itself"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover locate ~file:names_file
             ~byte:(at names ~within:"namespace box {" ~name:"box")))
       "```jasmin\nnamespace box\n```");
  check "hover: a function inside a namespace"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover locate ~file:names_file
             ~byte:(at names ~within:"fn put" ~name:"put")))
       "```jasmin\nfn put(reg u64 v) -> reg u64\n```")

