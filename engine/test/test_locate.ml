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

(* The byte-level answers carry the path the index holds, so a test compares
   paths and never an identifier: the identifier is the seam's business and is
   checked in the seam's own test below. *)
let same_file (left : string) (right : string) =
  String.equal
    (Jide_engine.Index.normalise left)
    (Jide_engine.Index.normalise right)

let is_at (location : Types.location) ~file ~span =
  same_file location.file file && location.span = span

let spans (locations : Types.location list) =
  List.map (fun (location : Types.location) -> location.span) locations

let files_of (locations : Types.location list) =
  List.map (fun (location : Types.location) -> location.file) locations

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
    Jide_engine.Locate.definition_at locate ~file:names_file
      ~byte:(at names ~within:"x = double(a);" ~name:"double")
  in
  let declared_at_its_name =
    Jide_engine.Locate.definition_at locate ~file:names_file
      ~byte:(at names ~within:"fn double(reg u32 a)" ~name:"double")
  in
  let on_the_parameter =
    Jide_engine.Locate.definition_at locate ~file:names_file
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
    (Jide_engine.Locate.definition_at locate ~file:names_file ~byte:0 = None);
  check "definition: a file the resolution never reached has nothing"
    (Jide_engine.Locate.definition_at locate ~file:"/work/other.jazz" ~byte:0
    = None)

let test_definition_of_a_type_alias () =
  let locate = single names_file names in
  let use =
    Jide_engine.Locate.definition_at locate ~file:names_file
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
       (Jide_engine.Locate.definition_at locate ~file:names_file
          ~byte:(at names ~within:"x = box::put(a);" ~name:"put")));
  check "namespace: the whole qualified name is the declaration"
    (same
       (Jide_engine.Locate.definition_at locate ~file:names_file
          ~byte:(at names ~within:"x = box::put(a);" ~name:"box::put")));
  check "namespace: the first segment is the same declaration"
    (same
       (Jide_engine.Locate.definition_at locate ~file:names_file
          ~byte:(at names ~within:"x = box::put(a);" ~name:"box")))

let test_references_of_a_local () =
  let locate = single names_file names in
  let byte = at names ~within:"reg u32 result;" ~name:"result" in
  let all =
    Jide_engine.Locate.references_at locate ~file:names_file ~byte
      ~include_declaration:true
  in
  let without_the_declaration =
    Jide_engine.Locate.references_at locate ~file:names_file ~byte
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
    (files_of all = [ names_file; names_file; names_file ]);
  check "references: a keyword has none"
    (Jide_engine.Locate.references_at locate ~file:names_file ~byte:0
       ~include_declaration:true
    = [])

(* Two variables of one name are two declarations, and neither answers for
   the other. *)
let test_two_declarations_of_one_name () =
  let locate = single names_file names in
  let in_double =
    Jide_engine.Locate.references_at locate ~file:names_file
      ~byte:(at names ~within:"reg u32 result;" ~name:"result")
      ~include_declaration:true
  in
  let negate_declaration =
    expected names
      ~within:"fn negate(reg u32 a) -> reg u32 {\n  reg u32 result;"
      ~name:"result"
  in
  let in_negate =
    Jide_engine.Locate.references_at locate ~file:names_file
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
       Jide_engine.Locate.definition_at locate ~file:names_file
         ~byte:(at names ~within:"result = a - a;" ~name:"result")
     with
    | Some location -> is_at location ~file:names_file ~span:negate_declaration
    | None -> false)

let test_the_outline_nests_namespaces () =
  let locate = single names_file names in
  let symbols = Jide_engine.Locate.outline locate ~file:names_file in
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
    (Jide_engine.Locate.outline locate ~file:"/work/other.jazz" = [])

let test_hover_from_the_index () =
  let locate = single names_file names in
  let at_call =
    Jide_engine.Locate.hover_at locate ~file:names_file
      ~byte:(at names ~within:"x = double(a);" ~name:"double")
  in
  let at_local =
    Jide_engine.Locate.hover_at locate ~file:names_file
      ~byte:(at names ~within:"result = a + a;" ~name:"result")
  in
  let at_alias =
    Jide_engine.Locate.hover_at locate ~file:names_file
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
    (Jide_engine.Locate.hover_at locate ~file:names_file ~byte:0 = None);
  check "hover: a namespace names itself"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at locate ~file:names_file
             ~byte:(at names ~within:"namespace box {" ~name:"box")))
       "```jasmin\nnamespace box\n```");
  check "hover: a function inside a namespace"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at locate ~file:names_file
             ~byte:(at names ~within:"fn put" ~name:"put")))
       "```jasmin\nfn put(reg u64 v) -> reg u64\n```")

let test_hover_on_a_primitive_and_an_annotation () =
  let text =
    "#[returnaddress=stack]\n\
     fn f(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = #add(a, a);\n\
    \  return x;\n\
     }\n"
  in
  let file = "/work/prim.jazz" in
  let locate = single file text in
  let primitive =
    Jide_engine.Locate.hover_at locate ~file
      ~byte:(at text ~within:"x = #add(a, a);" ~name:"add")
  in
  let annotation =
    Jide_engine.Locate.hover_at locate ~file
      ~byte:(at text ~within:"#[returnaddress=stack]" ~name:"returnaddress")
  in
  check "hover: a primitive names itself"
    (String.equal (hover_text primitive) "```jasmin\n#add\na primitive\n```");
  check "hover: an annotation label names itself and its value"
    (String.equal (hover_text annotation)
       "```jasmin\n#[returnaddress=\"stack\"]\n```");
  check "definition: a primitive is not a declaration"
    (Jide_engine.Locate.definition_at locate ~file
       ~byte:(at text ~within:"x = #add(a, a);" ~name:"add")
    = None)

(* pretyping has no forward declaration, so a name used above its declaration
   is not a declaration at all: nothing to go to and nothing to say. *)
let test_a_name_used_before_its_declaration () =
  let text =
    "fn f(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = below(a);\n\
    \  return x;\n\
     }\n\
     fn below(reg u32 a) -> reg u32 { return a; }\n"
  in
  let file = "/work/forward.jazz" in
  let locate = single file text in
  let byte = at text ~within:"x = below(a);" ~name:"below" in
  check "forward: a call above the declaration has no definition"
    (Jide_engine.Locate.definition_at locate ~file ~byte = None);
  check "forward: and no hover"
    (Jide_engine.Locate.hover_at locate ~file ~byte = None);
  check "forward: and no reference"
    (Jide_engine.Locate.references_at locate ~file ~byte
       ~include_declaration:true
    = []);
  check "forward: the declaration itself is still a definition"
    (match
       Jide_engine.Locate.definition_at locate ~file
         ~byte:(at text ~within:"fn below" ~name:"below")
     with
    | Some location ->
        is_at location ~file
          ~span:(expected text ~within:"fn below" ~name:"below")
    | None -> false);
  check "of_file: the bytes a reader gives are the ones indexed"
    (match
       Jide_engine.Locate.of_file
         ~read:(read_of [ (Filename.basename file, text) ])
         ~idirs:[] file
     with
    | Some from_file ->
        String.equal (Jide_engine.Locate.entry from_file) file
        && Jide_engine.Locate.outline from_file ~file
           |> outline_text = "f, below"
    | None -> false)

(* ------------------------------------------------------------------ *)
(* Project two: a require across files *)

let linked ?facts () =
  project ?facts
    ~files:[ (Filename.basename library_file, library) ]
    entry_file entry

let test_definition_across_a_require () =
  let locate = linked () in
  let call =
    Jide_engine.Locate.definition_at locate ~file:entry_file
      ~byte:(at entry ~within:"x = lib_f(a);" ~name:"lib_f")
  in
  let declaration =
    Jide_engine.Locate.definition_at locate ~file:library_file
      ~byte:(at library ~within:"fn lib_f" ~name:"lib_f")
  in
  check "require: a call in the entry points into the library's file"
    (match call with
    | Some location ->
        is_at location ~file:library_file
          ~span:(expected library ~within:"fn lib_f" ~name:"lib_f")
    | None -> false);
  check "require: the declaration in the library points at itself"
    (match declaration with
    | Some location ->
        is_at location ~file:library_file
          ~span:(expected library ~within:"fn lib_f" ~name:"lib_f")
    | None -> false);
  check "require: the snapshot holds both files, the entry first"
    (List.map
       (fun (index : Jide_engine.Index.t) -> index.file)
       (Jide_engine.Locate.files locate)
    = [ entry_file; library_file ]);
  check "require: a file the resolution never read has no index"
    (Jide_engine.Locate.file locate "/work/absent.jinc" = None);
  check "require: reading the library on its own is the same declaration"
    (Jide_engine.Locate.declaration_at locate ~file:library_file
       ~byte:(at library ~within:"fn lib_f" ~name:"lib_f")
    <> None)

let test_references_across_a_require () =
  let locate = linked () in
  let from_the_library =
    Jide_engine.Locate.references_at locate ~file:library_file
      ~byte:(at library ~within:"fn lib_f" ~name:"lib_f")
      ~include_declaration:true
  in
  let without_the_declaration =
    Jide_engine.Locate.references_at locate ~file:entry_file
      ~byte:(at entry ~within:"x = lib_f(a);" ~name:"lib_f")
      ~include_declaration:false
  in
  check "references: the entry's calls and the library's declaration"
    (spans from_the_library
    = [
        expected entry ~within:"x = lib_f(a);" ~name:"lib_f";
        expected entry ~within:"x = lib_f(x);" ~name:"lib_f";
        expected library ~within:"fn lib_f" ~name:"lib_f";
      ]);
  check "references: each one names the file it is in"
    (files_of from_the_library = [ entry_file; entry_file; library_file ]);
  check "references: without the declaration only the entry's two"
    (spans without_the_declaration
    = [
        expected entry ~within:"x = lib_f(a);" ~name:"lib_f";
        expected entry ~within:"x = lib_f(x);" ~name:"lib_f";
      ])

let test_hover_prefers_the_typed_facts () =
  let facts = checked_facts ~file:library_file library in
  let with_facts = linked ~facts:[ (library_file, facts) ] () in
  let without_facts = linked () in
  let call_byte = at entry ~within:"x = lib_f(a);" ~name:"lib_f" in
  let checked =
    Jide_engine.Locate.hover_at with_facts ~file:entry_file ~byte:call_byte
  in
  let indexed =
    Jide_engine.Locate.hover_at without_facts ~file:entry_file ~byte:call_byte
  in
  check "facts: the compiler's signature wins where it has one"
    (String.equal (hover_text checked) "```jasmin\nfn lib_f(u32) -> u32\n```");
  check "facts: the index's own rendering when there are none"
    (String.equal (hover_text indexed)
       "```jasmin\nfn lib_f(reg u32 a) -> reg u32\n```");
  check "facts: the snapshot keeps the facts it was given"
    (match Jide_engine.Locate.facts with_facts library_file with
    | Some kept -> List.length kept.Jide_jasmin.Facts.items = 1
    | None -> false);
  check "facts: a variable hovers from the compiler's kind and type"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at with_facts ~file:library_file
             ~byte:(at library ~within:"tmp = a;" ~name:"tmp")))
       "```jasmin\nreg u32 tmp\n```");

  (* A snapshot with its facts replaced is a new value; the one it came from
     keeps what it had. *)
  let replaced =
    Jide_engine.Locate.with_facts with_facts ~file:library_file
      Jide_jasmin.Facts.empty
  in
  check "facts: replacing them changes the new snapshot only"
    (Jide_engine.Locate.facts replaced library_file
     = Some Jide_jasmin.Facts.empty
    && Jide_engine.Locate.facts with_facts library_file
       <> Some Jide_jasmin.Facts.empty);
  check "facts: with the facts gone the index answers again"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at replaced ~file:entry_file ~byte:call_byte))
       "```jasmin\nfn lib_f(reg u32 a) -> reg u32\n```")

let test_the_outline_of_a_required_file () =
  let locate = linked () in
  check "outline: the library's one function"
    (String.equal
       (outline_text (Jide_engine.Locate.outline locate ~file:library_file))
       "lib_f");
  check "outline: the entry's require, the file it names, then the function"
    (String.equal
       (outline_text (Jide_engine.Locate.outline locate ~file:entry_file))
       "require[lib.jinc], main")

(* ------------------------------------------------------------------ *)
(* Project three: the same pair with a byte-order mark on both files *)

let test_a_byte_order_mark_changes_no_answer () =
  let locate =
    project
      ~files:[ (Filename.basename library_file, marked_library) ]
      entry_file marked_entry
  in
  let call =
    Jide_engine.Locate.definition_at locate ~file:entry_file
      ~byte:(at marked_entry ~within:"x = lib_f(a);" ~name:"lib_f")
  in
  check "bom: a definition across the require lands after the mark's bytes"
    (match call with
    | Some location ->
        is_at location ~file:library_file
          ~span:(expected marked_library ~within:"fn lib_f" ~name:"lib_f")
    | None -> false);
  check "bom: the required file's declaration sits three bytes further"
    (expected marked_library ~within:"fn lib_f" ~name:"lib_f"
    = Jide_lang.Span.shift (expected library ~within:"fn lib_f" ~name:"lib_f") 3
    );
  check "bom: a local in the marked entry points inside it"
    (match
       Jide_engine.Locate.definition_at locate ~file:entry_file
         ~byte:(at marked_entry ~within:"x = lib_f(a);" ~name:"x")
     with
    | Some location ->
        is_at location ~file:entry_file
          ~span:(expected marked_entry ~within:"reg u32 x;" ~name:"x")
    | None -> false);
  check "bom: hover names the local"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at locate ~file:entry_file
             ~byte:(at marked_entry ~within:"x = lib_f(a);" ~name:"x")))
       "```jasmin\nreg u32 x\n```");
  check "bom: references keep the marked ranges"
    (spans
       (Jide_engine.Locate.references_at locate ~file:library_file
          ~byte:(at marked_library ~within:"fn lib_f" ~name:"lib_f")
          ~include_declaration:true)
    = [
        expected marked_entry ~within:"x = lib_f(a);" ~name:"lib_f";
        expected marked_entry ~within:"x = lib_f(x);" ~name:"lib_f";
        expected marked_library ~within:"fn lib_f" ~name:"lib_f";
      ]);
  check "bom: the outline is shifted with the mark"
    (match
       List.find_opt
         (fun (s : Types.symbol) -> String.equal s.name "main")
         (Jide_engine.Locate.outline locate ~file:entry_file)
     with
    | Some main ->
        main.name_span = expected marked_entry ~within:"fn main" ~name:"main"
    | None -> false);
  check "bom: a byte inside the mark holds nothing"
    (Jide_engine.Locate.definition_at locate ~file:entry_file ~byte:1 = None
    && Jide_engine.Locate.hover_at locate ~file:entry_file ~byte:2 = None)

(* ------------------------------------------------------------------ *)
(* Project four: a file that does not parse *)

let test_a_file_that_does_not_parse () =
  let broken = "fn broken( {\n" in
  let file = "/work/broken.jazz" in
  let locate = single file broken in
  check "broken: no outline" (Jide_engine.Locate.outline locate ~file = []);
  check "broken: nothing to go to anywhere in it"
    (List.for_all
       (fun byte -> Jide_engine.Locate.definition_at locate ~file ~byte = None)
       [ 0; 1; 3; 5; 10 ]);
  check "broken: no hover"
    (List.for_all
       (fun byte -> Jide_engine.Locate.hover_at locate ~file ~byte = None)
       [ 0; 1; 3; 5; 10 ]);
  check "broken: no references"
    (Jide_engine.Locate.references_at locate ~file ~byte:3
       ~include_declaration:true
    = []);
  check "broken: of_file with nothing to read is None"
    (Jide_engine.Locate.of_file ~idirs:[] ~read:(fun _ -> None) file = None)

(* ------------------------------------------------------------------ *)
(* A qualified name of three segments, the way pretyping resolves it:
   [A::B::x] is a namespace holding a namespace holding a name.

   The use is written where the grammar allows one. A qualified name is a
   [var], and a [var] may stand anywhere an expression may and as the target
   of an assignment; a [return] takes its own list of variables rather than
   an expression, so a call cannot be written there. The assignment below is
   therefore the shortest legal spelling of a three-segment qualified use,
   not a weaker one: the same [PECall] over the same [outer::inner::deep]. *)

let nested =
  "namespace outer {\n\
  \  namespace inner {\n\
  \    fn deep(reg u32 a) -> reg u32 { return a; }\n\
  \  }\n\
   }\n\n\
   fn user(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = outer::inner::deep(a);\n\
  \  return x;\n\
   }\n"

let nested_file = "/work/nested.jazz"

let test_a_three_segment_qualified_name () =
  let locate = single nested_file nested in
  let deep = expected nested ~within:"fn deep" ~name:"deep" in
  let same = function
    | Some location -> is_at location ~file:nested_file ~span:deep
    | None -> false
  in
  check "qualified: the whole A::B::x name is the declaration"
    (same
       (Jide_engine.Locate.definition_at locate ~file:nested_file
          ~byte:
            (at nested ~within:"x = outer::inner::deep(a);"
               ~name:"outer::inner::deep")));
  check "qualified: the last segment is the same declaration"
    (same
       (Jide_engine.Locate.definition_at locate ~file:nested_file
          ~byte:(at nested ~within:"outer::inner::deep(a)" ~name:"deep")));
  check "qualified: the outermost segment reaches it too"
    (same
       (Jide_engine.Locate.definition_at locate ~file:nested_file
          ~byte:(at nested ~within:"x = outer::inner::deep(a);" ~name:"outer")));
  check "qualified: an intermediate segment reaches it"
    (same
       (Jide_engine.Locate.definition_at locate ~file:nested_file
          ~byte:(at nested ~within:"outer::inner::deep(a)" ~name:"inner")));
  check "qualified: the outline nests both namespaces"
    (String.equal
       (outline_text (Jide_engine.Locate.outline locate ~file:nested_file))
       "outer[inner[deep]], user")

(* ------------------------------------------------------------------ *)
(* A position that holds no name is the empty answer, never an error. *)

let test_a_position_in_whitespace_and_in_a_comment () =
  let blank = "\n\n   \t\n" in
  let blank_file = "/work/blank.jazz" in
  let locate = single blank_file blank in
  check "whitespace: every byte of a blank file is empty"
    (List.for_all
       (fun byte ->
         Jide_engine.Locate.definition_at locate ~file:blank_file ~byte = None
         && Jide_engine.Locate.hover_at locate ~file:blank_file ~byte = None
         && Jide_engine.Locate.references_at locate ~file:blank_file ~byte
              ~include_declaration:true
            = [])
       (List.init (String.length blank) (fun index -> index)));
  check "whitespace: an empty file has no outline either"
    (Jide_engine.Locate.outline locate ~file:blank_file = []);

  let text =
    "// a comment names nothing: no words are here but these\n\
     fn f(reg u32 a) -> reg u32 {\n\
    \  /* nor here, not even a name like a or f */\n\
    \  return a;\n\
     }\n"
  in
  let file = "/work/commented.jazz" in
  let locate = single file text in
  let in_line_comment = at text ~within:"// a comment" ~name:"comment" in
  let in_block_comment = at text ~within:"/* nor here" ~name:"nor" in
  check "comment: a word in a line comment names nothing"
    (Jide_engine.Locate.definition_at locate ~file ~byte:in_line_comment = None
    && Jide_engine.Locate.hover_at locate ~file ~byte:in_line_comment = None
    && Jide_engine.Locate.references_at locate ~file ~byte:in_line_comment
         ~include_declaration:true
       = []);
  check "comment: a word in a block comment names nothing"
    (Jide_engine.Locate.definition_at locate ~file ~byte:in_block_comment = None
    && Jide_engine.Locate.hover_at locate ~file ~byte:in_block_comment = None);
  check "comment: a name the comment quotes is not an occurrence"
    (Jide_engine.Locate.definition_at locate ~file
       ~byte:(at text ~within:"not even a name like a or f" ~name:"f")
    = None);
  (* The code beside the comments still answers, so the test above is about
     the comment and not about a file that answers nothing. *)
  check "comment: the code beside them still answers"
    (match
       Jide_engine.Locate.definition_at locate ~file
         ~byte:(at text ~within:"  return a;" ~name:"a")
     with
    | Some location ->
        is_at location ~file
          ~span:(expected text ~within:"fn f(reg u32 a)" ~name:"a")
    | None -> false)

(* A file the snapshot never reached is empty on every question. *)
let test_an_unopened_document () =
  let locate = single names_file names in
  let absent = "/work/never-opened.jazz" in
  check "unopened: it has no outline"
    (Jide_engine.Locate.outline locate ~file:absent = []);
  check "unopened: it has no definition"
    (Jide_engine.Locate.definition_at locate ~file:absent ~byte:0 = None);
  check "unopened: it has no hover"
    (Jide_engine.Locate.hover_at locate ~file:absent ~byte:0 = None);
  check "unopened: it has no references"
    (Jide_engine.Locate.references_at locate ~file:absent ~byte:0
       ~include_declaration:true
    = []);
  check "unopened: the resolution holds no index for it"
    (Jide_engine.Locate.file locate absent = None);
  check "unopened: the file's own questions still answer"
    (Jide_engine.Locate.outline locate ~file:names_file <> [])

(* ------------------------------------------------------------------ *)
(* Rename-adjacent: two declarations of one name in different blocks of one
   function. They are two identities and must not be conflated, which is what
   rename will rely on. *)

let shadowed =
  "fn blocks(reg u32 a) -> reg u32 {\n\
  \  reg u32 total;\n\
  \  total = a;\n\
  \  if a {\n\
  \    reg u32 total;\n\
  \    total = a;\n\
  \    total = total;\n\
  \  }\n\
  \  return total;\n\
   }\n"

let shadowed_file = "/work/blocks.jazz"

let test_two_blocks_of_one_name () =
  let locate = single shadowed_file shadowed in
  let outer_declaration =
    expected shadowed ~within:"  reg u32 total;\n  total = a;" ~name:"total"
  in
  let inner_declaration =
    expected shadowed ~within:"    reg u32 total;\n    total = a;" ~name:"total"
  in
  let outer =
    Jide_engine.Locate.references_at locate ~file:shadowed_file
      ~byte:(at shadowed ~within:"reg u32 total;\n  total = a;" ~name:"total")
      ~include_declaration:true
  in
  let inner =
    Jide_engine.Locate.references_at locate ~file:shadowed_file
      ~byte:
        (at shadowed ~within:"    reg u32 total;\n    total = a;" ~name:"total")
      ~include_declaration:true
  in
  check "blocks: the outer name has its declaration, its write and its return"
    (spans outer
    = [
        outer_declaration;
        expected shadowed ~within:"total = a;\n  if a" ~name:"total";
        expected shadowed ~within:"\n  return total;" ~name:"total";
      ]);
  check "blocks: the inner name has its own declaration and its three uses"
    (spans inner
    = [
        inner_declaration;
        (* The statement that initialises it in the inner block. *)
        expected shadowed ~within:"\n    total = a;\n    total = total;"
          ~name:"total";
        (* The self-assignment [total = total;] writes the name once and reads
           it once, so it is two occurrences at two ranges, not one: the window
           below starts inside the line before it and finds the name the
           statement writes, and the last one starts at the [=] and finds the
           name it reads. *)
        expected shadowed ~within:"= a;\n    total = total;" ~name:"total";
        expected shadowed ~within:"= total;" ~name:"total";
      ]);
  check "blocks: the outer declaration quotes the outer name here"
    (inner_declaration <> outer_declaration);
  check "blocks: no range is in both answers"
    (List.for_all (fun span -> not (List.mem span (spans inner))) (spans outer));
  check "blocks: a use in the inner block points at the inner declaration"
    (match
       Jide_engine.Locate.definition_at locate ~file:shadowed_file
         ~byte:
           (at shadowed ~within:"total = a;\n    total = total;" ~name:"total")
     with
    | Some location ->
        is_at location ~file:shadowed_file ~span:inner_declaration
    | None -> false);
  check "blocks: a use after the block points at the outer declaration"
    (match
       Jide_engine.Locate.definition_at locate ~file:shadowed_file
         ~byte:(at shadowed ~within:"\n  return total;" ~name:"total")
     with
    | Some location ->
        is_at location ~file:shadowed_file ~span:outer_declaration
    | None -> false)

(* ------------------------------------------------------------------ *)
(* Project five: the controller's seam, where a path becomes the protocol's
   identifier

   Everything above asks the byte-level questions, whose file is the path the
   index holds. The controller asks the four seam questions instead, and what
   it prints is an identifier: the string the client opened the document with
   when the answer is in that document, and a [file:] identifier otherwise.
   The seam is also where a span becomes a range, so an answer that lost the
   path would lose its range with it: these two tests say the identifier is
   the document's own and every range is a declaration's real one. *)

let seam_engine ~uri text =
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.open_document engine ~uri ~version:1 ~text;
  ignore (Jide_engine.Engine.check engine ~uri);
  engine

let position line character = Jide_lang.Position.make ~line ~character

(* One seam location written as [uri line:character-line:character], so a
   failed comparison prints the answers instead of only saying they differ. *)
let location_text (answer : Jide_engine.Locate.location) =
  Printf.sprintf "%s %d:%d-%d:%d" answer.uri answer.range.start_.line
    answer.range.start_.character answer.range.end_.line
    answer.range.end_.character

let locations_text (answers : Jide_engine.Locate.location list) =
  "[" ^ String.concat "; " (List.map location_text answers) ^ "]"

let test_the_seam_answers_in_the_documents_own_identifier () =
  let text =
    "fn f(reg u32 a) -> reg u32 {\n  reg u32 x;\n  x = a;\n  return x;\n}\n"
  in
  let file = "/tmp/locate-seam.jazz" in
  let uri = uri_of file in
  let engine = seam_engine ~uri text in
  let answers =
    Jide_engine.Locate.definition engine ~uri ~position:(position 2 2)
  in
  (* The declaration writes [x] at line 1, characters 10 to 11, and the
     position asked about is the use of it at line 2, character 2. *)
  check
    (Printf.sprintf
       "seam: a definition names the document the client opened, got %s"
       (locations_text answers))
    (match answers with
    | [ answer ] -> String.equal answer.uri uri
    | _ -> false);
  check
    (Printf.sprintf "seam: and its range is the declaration's, got %s"
       (locations_text answers))
    (List.map location_text answers = [ uri ^ " 1:10-1:11" ]);
  check "seam: a position in whitespace is still the empty answer"
    (Jide_engine.Locate.definition engine ~uri ~position:(position 0 0) = [])

let test_the_seam_keeps_two_declarations_apart () =
  let file = "/tmp/locate-blocks.jazz" in
  let uri = uri_of file in
  let engine = seam_engine ~uri shadowed in
  let inner =
    Jide_engine.Locate.references engine ~uri ~position:(position 5 4)
      ~include_declaration:true
  in
  let outer =
    Jide_engine.Locate.references engine ~uri ~position:(position 8 9)
      ~include_declaration:true
  in
  (* [shadowed] declares [total] at line 1, characters 10 to 15, and again at
     line 4, characters 12 to 17. The two answers below are asked about a use
     of each: the write in the inner block, and the read after it. *)
  let inner_text =
    [
      uri ^ " 4:12-4:17"; uri ^ " 5:4-5:9"; uri ^ " 6:4-6:9"; uri ^ " 6:12-6:17";
    ]
  in
  let outer_text =
    [ uri ^ " 1:10-1:15"; uri ^ " 2:2-2:7"; uri ^ " 8:9-8:14" ]
  in
  check
    (Printf.sprintf "seam: the inner name is its own four, got %s"
       (locations_text inner))
    (List.map location_text inner = inner_text);
  check
    (Printf.sprintf "seam: the outer name is its own three, got %s"
       (locations_text outer))
    (List.map location_text outer = outer_text);
  check "seam: no range is in both answers"
    (List.for_all (fun text -> not (List.mem text outer_text)) inner_text);
  check "seam: every reference names the document the client opened"
    (List.for_all
       (fun (answer : Jide_engine.Locate.location) ->
         String.equal answer.uri uri)
       (inner @ outer))

let test_hover_shows_the_annotation_strings () =
  let text =
    "#[ct=\"public -> public\"]\n\
     #[sct=\"secret\"]\n\
     #[returnaddress=stack]\n\
     fn f(reg u64 a) -> reg u64 {\n\
    \  stack u64 s;\n\
    \  s = a;\n\
    \  return s;\n\
     }\n"
  in
  let file = "/work/contracts.jazz" in
  let locate = single file text in
  let on_the_contract =
    hover_text
      (Jide_engine.Locate.hover_at locate ~file
         ~byte:(at text ~within:"#[ct=" ~name:"ct"))
  in
  check "hover: a quoted contract string is shown"
    (String.equal on_the_contract "```jasmin\n#[ct=\"public -> public\"]\n```");
  check "hover: a second contract string is shown too"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at locate ~file
             ~byte:(at text ~within:"#[sct=" ~name:"sct")))
       "```jasmin\n#[sct=\"secret\"]\n```");
  check "hover: the keyword form is unchanged"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at locate ~file
             ~byte:(at text ~within:"#[returnaddress=" ~name:"returnaddress")))
       "```jasmin\n#[returnaddress=\"stack\"]\n```");

  (* At the declaration the strings are part of what it says, below the
     signature the index cut. The window starts at the name and the
     parenthesis after it, so the [f] it finds is the declared name's and not
     the [f] of the [fn] keyword. *)
  check "hover: a function's declaration shows its contracts below its type"
    (String.equal
       (hover_text
          (Jide_engine.Locate.hover_at locate ~file
             ~byte:(at text ~within:"f(reg u64 a)" ~name:"f")))
       "```jasmin\n\
        fn f(reg u64 a) -> reg u64\n\
        #[ct=\"public -> public\"]\n\
        #[sct=\"secret\"]\n\
        #[returnaddress=\"stack\"]\n\
        ```")

(* A value is the user's text, and hover is markdown: a backtick would end
   the fence, a control byte has no place in it, and a value longer than a
   line of prose is not shown. The label is shown alone instead. *)
let test_hover_refuses_an_unsafe_annotation_string () =
  let with_backtick =
    "#[ct=\"a`b\"]\nfn f(reg u32 a) -> reg u32 { return a; }\n"
  in
  let with_control =
    "#[ct=\"a\001b\"]\nfn f(reg u32 a) -> reg u32 { return a; }\n"
  in
  let long = String.make 200 'x' in
  let with_long =
    "#[ct=\"" ^ long ^ "\"]\nfn f(reg u32 a) -> reg u32 { return a; }\n"
  in
  let hover_of text =
    let file = "/work/unsafe.jazz" in
    let locate = single file text in
    hover_text
      (Jide_engine.Locate.hover_at locate ~file
         ~byte:(at text ~within:"#[ct=" ~name:"ct"))
  in
  check "hover: a backtick in the value is not shown"
    (String.equal (hover_of with_backtick) "```jasmin\n#[ct]\n```");
  check "hover: a control byte in the value is not shown"
    (String.equal (hover_of with_control) "```jasmin\n#[ct]\n```");
  check "hover: a value too long to read is not shown"
    (String.equal (hover_of with_long) "```jasmin\n#[ct]\n```");
  check "hover: the safe value beside them is still shown"
    (String.equal
       (hover_of "#[ct=\"ok\"]\nfn f(reg u32 a) -> reg u32 { return a; }\n")
       "```jasmin\n#[ct=\"ok\"]\n```")

(* ------------------------------------------------------------------ *)
(* The outline's shapes: the item range, the selection range, the kind *)

(* The item range is the whole item, and an item begins where the item before
   it ended: [Locate]'s [symbol.range] is "the whole item, which is what an
   outline collapses" (locate.mli), and [Index.item_span] is "the range of the
   item it makes" (index.mli), never "the definition text". The bytes in front
   of a definition are therefore the item's own leading trivia, and a doc
   comment written above a declaration falls inside its range, which is what a
   collapse should hide with it. *)
let test_the_outline_carries_both_ranges () =
  let locate = single names_file names in
  let symbols = Jide_engine.Locate.outline locate ~file:names_file in
  let negate =
    List.find_opt
      (fun (s : Types.symbol) -> String.equal s.name "negate")
      symbols
  in
  let definition =
    "fn negate(reg u32 a) -> reg u32 {\n\
    \  reg u32 result;\n\
    \  result = a - a;\n\
    \  return result;\n\
     }"
  in
  (* The blank line that separates [caller] from [negate], which the parser's
     location for the second item takes in. *)
  let leading = "\n\n" in
  let whole_item =
    expected names
      ~within:("return x;\n}\n\n" ^ definition)
      ~name:(leading ^ definition)
  in
  check "outline: the item range is the definition and the trivia above it"
    (match negate with
    | Some symbol ->
        symbol.span = whole_item
        && String.equal
             (Jide_engine.Index.text_of_span names symbol.span)
             (leading ^ definition)
    | None -> false);
  check "outline: the item range ends where the definition ends"
    (match negate with
    | Some symbol ->
        symbol.span.end_byte
        = symbol.span.start_byte + String.length leading
          + String.length definition
    | None -> false);
  (* The same rule measured once on a declaration that carries a doc comment,
     so the test above is about a rule and not about one blank line. *)
  let documented =
    "fn a(reg u32 p) -> reg u32 {\n\
    \  return p;\n\
     }\n\n\
     // the doc of b\n\
     // second line\n\
     fn b(reg u32 q) -> reg u32 {\n\
    \  return q;\n\
     }\n"
  in
  let documented_file = "/work/documented.jazz" in
  check "outline: a doc comment above a declaration is inside its range"
    (match
       List.find_opt
         (fun (s : Types.symbol) -> String.equal s.name "b")
         (Jide_engine.Locate.outline
            (single documented_file documented)
            ~file:documented_file)
     with
    | Some symbol ->
        String.equal
          (Jide_engine.Index.text_of_span documented symbol.span)
          "\n\n\
           // the doc of b\n\
           // second line\n\
           fn b(reg u32 q) -> reg u32 {\n\
          \  return q;\n\
           }"
    | None -> false);
  check "outline: the selection range is the name inside it"
    (match negate with
    | Some symbol ->
        symbol.name_span = expected names ~within:"fn negate" ~name:"negate"
        && Jide_engine.Index.text_of_span names symbol.name_span = "negate"
        && symbol.span.start_byte <= symbol.name_span.start_byte
        && symbol.span.end_byte >= symbol.name_span.end_byte
    | None -> false);
  check "outline: every kind is one of the protocol's numbers"
    (List.for_all
       (fun (symbol : Types.symbol) ->
         List.mem symbol.kind
           [
             Types.Function;
             Types.Parameter;
             Types.Variable;
             Types.Global;
             Types.Namespace;
             Types.Type_alias;
             Types.Require;
             Types.Exec;
             Types.File;
             Types.Unknown;
           ])
       symbols);
  check "outline: an empty file has an empty outline"
    (Jide_engine.Locate.outline
       (single "/work/empty.jazz" "")
       ~file:"/work/empty.jazz"
    = [])

(* ------------------------------------------------------------------ *)
(* The run: every test this file defines, in the order they are written.
   The list is the count the line below prints, so a test that is written
   and not listed here is the one thing this arrangement cannot see; it is
   added to and read from in one place for that reason. *)

let tests =
  [
    ("the definition of a function", test_definition_of_a_function);
    ("the definition of a type alias", test_definition_of_a_type_alias);
    ("the definition through a namespace", test_definition_through_a_namespace);
    ("the references of a local", test_references_of_a_local);
    ("two declarations of one name", test_two_declarations_of_one_name);
    ("the outline nests namespaces", test_the_outline_nests_namespaces);
    ("hover from the index", test_hover_from_the_index);
    ( "hover on a primitive and an annotation",
      test_hover_on_a_primitive_and_an_annotation );
    ( "a name used before its declaration",
      test_a_name_used_before_its_declaration );
    ("the definition across a require", test_definition_across_a_require);
    ("the references across a require", test_references_across_a_require);
    ("hover prefers the typed facts", test_hover_prefers_the_typed_facts);
    ("the outline of a required file", test_the_outline_of_a_required_file);
    ( "a byte-order mark changes no answer",
      test_a_byte_order_mark_changes_no_answer );
    ("a file that does not parse", test_a_file_that_does_not_parse);
    ("a three-segment qualified name", test_a_three_segment_qualified_name);
    ( "a position in whitespace and in a comment",
      test_a_position_in_whitespace_and_in_a_comment );
    ("an unopened document", test_an_unopened_document);
    ("two blocks of one name", test_two_blocks_of_one_name);
    ( "the seam answers in the document's own identifier",
      test_the_seam_answers_in_the_documents_own_identifier );
    ( "the seam keeps two declarations apart",
      test_the_seam_keeps_two_declarations_apart );
    ( "hover shows the annotation strings",
      test_hover_shows_the_annotation_strings );
    ( "hover refuses an unsafe annotation string",
      test_hover_refuses_an_unsafe_annotation_string );
    ("the outline carries both ranges", test_the_outline_carries_both_ranges);
  ]

let () =
  List.iter
    (fun (name, run) ->
      let before = !failures in
      run ();
      if !failures > before then
        Printf.printf "%s: %d assertion(s) failed\n%!" name (!failures - before))
    tests;
  let run = List.length tests in
  if !failures = 0 then Printf.printf "all %d locate tests passed\n" run
  else (
    Printf.printf "%d assertion(s) failed in %d locate tests\n" !failures run;
    exit 1)
