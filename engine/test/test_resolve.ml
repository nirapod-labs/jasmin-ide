let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* The tests say where a name is by quoting the source around it, never by a
   byte offset written by hand: [byte_of_in text ~within ~name] is where
   [name] sits inside the first place [within] sits in the text. *)
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

let resolve ?(idirs = []) ?(files = []) file text =
  let read path = List.assoc_opt (Filename.basename path) files in
  Jide_engine.Resolve.of_text ~idirs ~read ~file text

let answer_of (resolved : Jide_engine.Resolve.t) ~file ~text ~within ~name =
  match byte_of_in text ~within ~name with
  | None -> None
  | Some byte -> (
      match Jide_engine.Resolve.answer_at resolved ~file ~byte with
      | Some (_, answer) -> Some answer
      | None -> None)

let is_resolved_to (resolved : Jide_engine.Resolve.t) ~file ~text ~within ~name
    ~declaration =
  match answer_of resolved ~file ~text ~within ~name with
  | Some (Jide_engine.Resolve.Resolved found) -> (
      match declaration with
      | Some expected -> Jide_engine.Index.equal_declaration found expected
      | None -> false)
  | _ -> false

let is_unresolved (resolved : Jide_engine.Resolve.t) ~file ~text ~within ~name =
  match answer_of resolved ~file ~text ~within ~name with
  | Some Jide_engine.Resolve.Unresolved -> true
  | _ -> false

let index_of (resolved : Jide_engine.Resolve.t) path =
  match Jide_engine.Resolve.file resolved path with
  | Some index -> index
  | None -> Jide_engine.Index.of_text ~file:path "" |> fst

let declaration_named index name kind =
  List.find_opt
    (fun (declaration : Jide_engine.Index.declaration) ->
      String.equal declaration.name name && declaration.kind = kind)
    index.Jide_engine.Index.declarations

let variables =
  "fn f(reg u32 a) -> reg u32 {\n  reg u32 b;\n  b = a;\n  return b;\n}\n"

let test_a_name_resolves_to_its_declaration () =
  let file = "/work/variables.jazz" in
  let resolved = resolve file variables in
  let index = index_of resolved file in
  let b = declaration_named index "b" Jide_engine.Index.Variable in
  let a = declaration_named index "a" Jide_engine.Index.Parameter in
  check "variables: the file is the entry"
    (String.equal (Jide_engine.Resolve.entry resolved) file);
  check "variables: only the entry was read"
    (List.map
       (fun (index : Jide_engine.Index.t) -> index.file)
       (Jide_engine.Resolve.files resolved)
    = [ file ]);
  check "variables: a read of b is the declared b"
    (is_resolved_to resolved ~file ~text:variables ~within:"b = a;" ~name:"b"
       ~declaration:b);
  check "variables: a read of a is the declared a"
    (is_resolved_to resolved ~file ~text:variables ~within:"= a;" ~name:"a"
       ~declaration:a);
  check "variables: the return is the declared b"
    (is_resolved_to resolved ~file ~text:variables ~within:"return b;" ~name:"b"
       ~declaration:b);
  check "variables: the declaration itself resolves to itself"
    (is_resolved_to resolved ~file ~text:variables ~within:"reg u32 b;"
       ~name:"b" ~declaration:b)

(* A document may begin with a UTF-8 byte-order mark. It is not part of the
   program, so a document that carries one resolves to exactly what the same
   text resolves to without it, and the mark itself names nothing. *)
let test_a_byte_order_mark_resolves () =
  let file = "/work/bom.jazz" in
  let text = "\xEF\xBB\xBF" ^ variables in
  let resolved = resolve file text in
  let index = index_of resolved file in
  let b = declaration_named index "b" Jide_engine.Index.Variable in
  let a = declaration_named index "a" Jide_engine.Index.Parameter in
  check "bom: the file is the entry"
    (String.equal (Jide_engine.Resolve.entry resolved) file);
  check "bom: a read of b is the declared b"
    (is_resolved_to resolved ~file ~text ~within:"b = a;" ~name:"b"
       ~declaration:b);
  check "bom: a read of a is the declared a"
    (is_resolved_to resolved ~file ~text ~within:"= a;" ~name:"a" ~declaration:a);
  check "bom: the return is the declared b"
    (is_resolved_to resolved ~file ~text ~within:"return b;" ~name:"b"
       ~declaration:b);
  check "bom: the declaration itself resolves to itself"
    (is_resolved_to resolved ~file ~text ~within:"reg u32 b;" ~name:"b"
       ~declaration:b);
  check "bom: no occurrence of the document is left unresolved"
    (List.for_all
       (fun (_, answer) -> answer <> Jide_engine.Resolve.Unresolved)
       (Jide_engine.Resolve.answers resolved file));
  check "bom: a byte inside the mark holds no occurrence"
    (Jide_engine.Resolve.answer_at resolved ~file ~byte:1 = None)

let order =
  "fn order(reg u32 a) -> reg u32 {\n\
  \  reg u32 early;\n\
  \  early = later;\n\
  \  reg u32 later;\n\
  \  later = a;\n\
  \  if (a < 1) {\n\
  \    reg u32 inner;\n\
  \    inner = a;\n\
  \    later = inner;\n\
  \  }\n\
  \  later = inner;\n\
  \  return early;\n\
   }\n"

let test_define_before_use_and_block_scope () =
  let file = "/work/order.jazz" in
  let resolved = resolve file order in
  let index = index_of resolved file in
  let later = declaration_named index "later" Jide_engine.Index.Variable in
  let inner = declaration_named index "inner" Jide_engine.Index.Variable in
  let early = declaration_named index "early" Jide_engine.Index.Variable in
  check "order: a use before the declaration is unresolved"
    (is_unresolved resolved ~file ~text:order ~within:"early = later;"
       ~name:"later");
  check "order: the same name after its declaration resolves"
    (is_resolved_to resolved ~file ~text:order ~within:"later = a;"
       ~name:"later" ~declaration:later);
  check "order: a name declared in a block resolves inside it"
    (is_resolved_to resolved ~file ~text:order ~within:"inner = a;"
       ~name:"inner" ~declaration:inner);
  check "order: the declaration inside the block resolves there"
    (is_resolved_to resolved ~file ~text:order ~within:"reg u32 inner;"
       ~name:"inner" ~declaration:inner);
  check "order: the same name after the block is unresolved"
    (is_unresolved resolved ~file ~text:order ~within:"}\n  later = inner;"
       ~name:"inner");
  check "order: a name outside every block still resolves"
    (is_resolved_to resolved ~file ~text:order ~within:"return early;"
       ~name:"early" ~declaration:early)

let calls =
  "fn first(reg u32 a) -> reg u32 { return a; }\n\
   fn caller(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = first(a);\n\
  \  x = second(a);\n\
  \  return x;\n\
   }\n\
   fn second(reg u32 a) -> reg u32 { return a; }\n\
   fn recursive(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = recursive(a);\n\
  \  return x;\n\
   }\n"

let test_calls_follow_the_same_rule () =
  let file = "/work/calls.jazz" in
  let resolved = resolve file calls in
  let index = index_of resolved file in
  let first = declaration_named index "first" Jide_engine.Index.Function in
  check "calls: a call to a function defined above resolves"
    (is_resolved_to resolved ~file ~text:calls ~within:"x = first(a);"
       ~name:"first" ~declaration:first);
  check "calls: a call to a function defined below is unresolved"
    (is_unresolved resolved ~file ~text:calls ~within:"x = second(a);"
       ~name:"second");
  check "calls: a function cannot call itself"
    (is_unresolved resolved ~file ~text:calls ~within:"x = recursive(a);"
       ~name:"recursive")

let loop_without_a_declaration =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 acc;\n\
  \  acc = a;\n\
  \  for i = 0 to 3 {\n\
  \    acc = acc + i;\n\
  \  }\n\
  \  return acc;\n\
   }\n"

let loop_with_one =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 acc;\n\
  \  reg u32 i;\n\
  \  acc = a;\n\
  \  i = a;\n\
  \  for i = 0 to 3 {\n\
  \    acc = acc + i;\n\
  \  }\n\
  \  return acc;\n\
   }\n"

(* A [for] variable is not a declaration: it must already exist. *)
let test_a_for_variable_must_exist () =
  let file = "/work/loop.jazz" in
  let missing = resolve file loop_without_a_declaration in
  check "for: a variable nothing declares is unresolved"
    (is_unresolved missing ~file ~text:loop_without_a_declaration
       ~within:"for i =" ~name:"i");
  check "for: and so is its use in the body"
    (is_unresolved missing ~file ~text:loop_without_a_declaration
       ~within:"acc = acc + i;" ~name:"i");
  let resolved = resolve file loop_with_one in
  let index = index_of resolved file in
  let i = declaration_named index "i" Jide_engine.Index.Variable in
  check "for: a variable already declared resolves"
    (is_resolved_to resolved ~file ~text:loop_with_one ~within:"for i ="
       ~name:"i" ~declaration:i);
  check "for: and its use in the body too"
    (is_resolved_to resolved ~file ~text:loop_with_one ~within:"acc = acc + i;"
       ~name:"i" ~declaration:i)

let namespaces =
  "namespace A {\n\
  \  fn f(reg u32 a) -> reg u32 { return a; }\n\
   }\n\
   fn use(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = A::f(a);\n\
  \  x = f(a);\n\
  \  return x;\n\
   }\n"

let test_namespaces () =
  let file = "/work/namespaces.jazz" in
  let resolved = resolve file namespaces in
  let index = index_of resolved file in
  let f = declaration_named index "f" Jide_engine.Index.Function in
  let namespace = declaration_named index "A" Jide_engine.Index.Namespace in
  check "namespaces: the qualified call resolves"
    (is_resolved_to resolved ~file ~text:namespaces ~within:"A::f(a);"
       ~name:"A::f" ~declaration:f);
  check "namespaces: the unqualified name is not there"
    (is_unresolved resolved ~file ~text:namespaces ~within:"x = f(a);" ~name:"f");
  check "namespaces: the namespace is a declaration"
    (is_resolved_to resolved ~file ~text:namespaces ~within:"namespace A {"
       ~name:"A" ~declaration:namespace)

let nested_namespaces =
  "namespace Outer {\n\
  \  fn outer_fn(reg u32 a) -> reg u32 { return a; }\n\
  \  namespace Inner {\n\
  \    fn inner_fn(reg u32 a) -> reg u32 {\n\
  \      reg u32 x;\n\
  \      x = outer_fn(a);\n\
  \      return x;\n\
  \    }\n\
  \  }\n\
   }\n"

let test_a_nested_namespace_finds_the_outer_one () =
  let file = "/work/nested.jazz" in
  let resolved = resolve file nested_namespaces in
  let index = index_of resolved file in
  let outer_fn =
    declaration_named index "outer_fn" Jide_engine.Index.Function
  in
  check "nested: a name from the enclosing namespace resolves"
    (is_resolved_to resolved ~file ~text:nested_namespaces
       ~within:"x = outer_fn(a);" ~name:"outer_fn" ~declaration:outer_fn)

let aliases =
  "type word = u32;\n\
   param int ROUNDS = 10;\n\
   u64 counter = 0;\n\
   fn f(reg word a) -> reg u32 {\n\
  \  reg word b;\n\
  \  b = a;\n\
  \  b = counter;\n\
  \  b = ROUNDS;\n\
  \  return b;\n\
   }\n"

let test_aliases_globals_and_parameters () =
  let file = "/work/aliases.jazz" in
  let resolved = resolve file aliases in
  let index = index_of resolved file in
  let word = declaration_named index "word" Jide_engine.Index.Type_alias in
  let counter = declaration_named index "counter" Jide_engine.Index.Global in
  let rounds = declaration_named index "ROUNDS" Jide_engine.Index.Parameter in
  check "aliases: a type use in a parameter list resolves"
    (is_resolved_to resolved ~file ~text:aliases ~within:"reg word a"
       ~name:"word" ~declaration:word);
  check "aliases: a type use in a declaration resolves"
    (is_resolved_to resolved ~file ~text:aliases ~within:"reg word b"
       ~name:"word" ~declaration:word);
  check "aliases: a global resolves"
    (is_resolved_to resolved ~file ~text:aliases ~within:"b = counter;"
       ~name:"counter" ~declaration:counter);
  check "aliases: a parameter resolves"
    (is_resolved_to resolved ~file ~text:aliases ~within:"b = ROUNDS;"
       ~name:"ROUNDS" ~declaration:rounds)

let entry =
  "require \"lib.jinc\"\n\
   require \"lib.jinc\"\n\
   fn main(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = lib_f(a);\n\
  \  return x;\n\
   }\n"

let library =
  "fn lib_f(reg u32 a) -> reg u32 { return a; }\ntype lib_word = u32;\n"

let test_a_required_file () =
  let file = "/work/entry.jazz" in
  let library_file = "/work/lib.jinc" in
  let resolved = resolve ~files:[ ("lib.jinc", library) ] file entry in
  let index = index_of resolved library_file in
  let lib_f = declaration_named index "lib_f" Jide_engine.Index.Function in
  check "require: the files reached, the entry first"
    (List.map
       (fun (index : Jide_engine.Index.t) -> index.file)
       (Jide_engine.Resolve.files resolved)
    = [ file; library_file ]);
  check "require: the call resolves into the required file"
    (is_resolved_to resolved ~file ~text:entry ~within:"x = lib_f(a);"
       ~name:"lib_f" ~declaration:lib_f);
  check "require: the answer names the file the declaration lives in"
    (match
       answer_of resolved ~file ~text:entry ~within:"x = lib_f(a);"
         ~name:"lib_f"
     with
    | Some (Jide_engine.Resolve.Resolved declaration) ->
        String.equal declaration.Jide_engine.Index.file library_file
    | _ -> false);
  check "require: the same file listed twice is read once"
    (List.filter
       (fun (_namespace, path) -> String.equal path library_file)
       (Jide_engine.Resolve.loaded resolved)
    |> List.length = 1);
  check "require: the entry is in its own namespace"
    (List.exists
       (fun (namespace, path) ->
         String.equal namespace "" && String.equal path file)
       (Jide_engine.Resolve.loaded resolved));
  check "require: both files were reached"
    (List.length (Jide_engine.Resolve.files resolved) = 2);
  (* The same entry with nothing to read the library from. *)
  let missing = resolve file entry in
  check "require: a file no read gives is not reached"
    (List.length (Jide_engine.Resolve.files missing) = 1);
  check "require: the call into it is unresolved"
    (is_unresolved missing ~file ~text:entry ~within:"x = lib_f(a);"
       ~name:"lib_f")

let from_require =
  "from LIB require \"deep.jinc\"\n\
   #[returnaddress=stack]\n\
   fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = #set0();\n\
  \  return x;\n\
   }\n"

let test_intrinsics_annotations_and_include_names () =
  let file = "/work/from.jazz" in
  let resolved =
    resolve
      ~idirs:[ ("LIB", "/work/lib") ]
      ~files:
        [ ("deep.jinc", "fn deep_f(reg u32 a) -> reg u32 { return a; }\n") ]
      file from_require
  in
  check "intrinsic: a # name is not a declaration"
    (match
       answer_of resolved ~file ~text:from_require ~within:"#set0()"
         ~name:"set0"
     with
    | Some Jide_engine.Resolve.Intrinsic -> true
    | _ -> false);
  check "annotation: a label is not a symbol"
    (match
       answer_of resolved ~file ~text:from_require
         ~within:"#[returnaddress=stack]" ~name:"returnaddress"
     with
    | Some Jide_engine.Resolve.Annotation -> true
    | _ -> false);
  check "include path: the from name is a configuration name"
    (match
       answer_of resolved ~file ~text:from_require ~within:"from LIB require"
         ~name:"LIB"
     with
    | Some Jide_engine.Resolve.Include_path -> true
    | _ -> false);
  check "include path: the named directory is where the file is read"
    (List.exists
       (fun (index : Jide_engine.Index.t) ->
         String.equal index.file "/work/lib/deep.jinc")
       (Jide_engine.Resolve.files resolved))

let test_answers_and_unknown_bytes () =
  let file = "/work/variables.jazz" in
  let resolved = resolve file variables in
  check "answers: one answer per occurrence"
    (List.length (Jide_engine.Resolve.answers resolved file) = 6);
  check "answers: every one is in source order"
    (let starts =
       List.map
         (fun ((occurrence : Jide_engine.Index.occurrence), _) ->
           occurrence.span.Jide_lang.Span.start_byte)
         (Jide_engine.Resolve.answers resolved file)
     in
     let rec ordered = function
       | [] | [ _ ] -> true
       | left :: (right :: _ as rest) -> left <= right && ordered rest
     in
     ordered starts);
  check "answers: a byte between two names holds no occurrence"
    (match byte_of variables "  reg u32 b;" with
    | Some byte -> Jide_engine.Resolve.answer_at resolved ~file ~byte = None
    | None -> false);
  check "answers: a file the resolution did not reach has none"
    (Jide_engine.Resolve.answers resolved "/work/absent.jazz" = []);
  check "answers: of_file is None when the file cannot be read"
    (let read _path = None in
     Jide_engine.Resolve.of_file ~idirs:[] ~read file = None)

let test_the_entry_path_is_normalised () =
  let resolved = resolve "./work/../work/variables.jazz" variables in
  check "normalise: the entry is written one way"
    (String.equal (Jide_engine.Resolve.entry resolved) "work/variables.jazz");
  check "normalise: the file is found by either spelling"
    (Jide_engine.Resolve.file resolved "./work/variables.jazz" <> None)

let test_identity_across_files () =
  let file = "/work/entry.jazz" in
  let library_file = "/work/lib.jinc" in
  let resolved = resolve ~files:[ ("lib.jinc", library) ] file entry in
  let index = index_of resolved library_file in
  let lib_f = declaration_named index "lib_f" Jide_engine.Index.Function in
  let name_start =
    match lib_f with
    | Some declaration -> declaration.name_span.Jide_lang.Span.start_byte
    | None -> -1
  in
  check "identity: the declaration sits where it is written"
    (name_start
    = match byte_of library "fn lib_f" with Some byte -> byte + 3 | None -> -2);
  check "identity: that declaration belongs to the library, not the entry"
    (match lib_f with
    | Some declaration -> String.equal declaration.file library_file
    | None -> false)

(* A flag group [?{...}] names a variable: a label with no attribute is the
   variable that receives the flag, and an attribute is the variable of a
   labelled flag. The label of a labelled flag names the flag, not a symbol. *)
let flag_groups =
  "fn f(reg u64 x) -> reg u64 {\n\
  \  reg u64 z;\n\
  \  reg bool cf zf CF;\n\
  \  z = x;\n\
  \  ?{CF=cf}, z = #ADC(z,z,cf);\n\
  \  ?{cf}, z = #ADC(z,z,cf);\n\
  \  ?{CF, zf}, z = #ADC(z,z,cf);\n\
  \  return z;\n\
   }\n"

let test_flag_groups_name_variables () =
  let file = "/work/flags.jazz" in
  let resolved = resolve file flag_groups in
  let index = index_of resolved file in
  let cf = declaration_named index "cf" Jide_engine.Index.Variable in
  let zf = declaration_named index "zf" Jide_engine.Index.Variable in
  let big = declaration_named index "CF" Jide_engine.Index.Variable in
  check "flags: a label with no attribute is the variable it names"
    (is_resolved_to resolved ~file ~text:flag_groups ~within:"?{cf}" ~name:"cf"
       ~declaration:cf);
  check "flags: the attribute of a labelled flag is the variable"
    (is_resolved_to resolved ~file ~text:flag_groups ~within:"?{CF=cf}"
       ~name:"cf" ~declaration:cf);
  check "flags: a label with no attribute in a later group is resolved too"
    (is_resolved_to resolved ~file ~text:flag_groups ~within:"?{CF, zf}"
       ~name:"zf" ~declaration:zf);
  check "flags: a label with no attribute names its variable, CF"
    (is_resolved_to resolved ~file ~text:flag_groups ~within:"?{CF, zf}"
       ~name:"CF" ~declaration:big);
  check "flags: the label of a labelled flag is not a symbol"
    (match
       answer_of resolved ~file ~text:flag_groups ~within:"?{CF=cf}" ~name:"CF"
     with
    | Some Jide_engine.Resolve.Annotation -> true
    | _ -> false)

(* A file a namespace reads is read once per namespace. Each reading names a
   byte by the declaration its own namespace holds, so the answers of the two
   readings differ, and the first reading is the one the single answer gives. *)
let twice_entry =
  "namespace Short {\n\
  \  param int N = 1;\n\
  \  require \"sum.jinc\"\n\
   }\n\
   namespace Long {\n\
  \  param int N = 8;\n\
  \  require \"sum.jinc\"\n\
   }\n"

let twice_library =
  "fn sum(reg u32[N] t) -> reg u32 {\n  reg u32 s;\n  s = N;\n  return s;\n}\n"

let test_a_file_read_twice_answers_per_reading () =
  let file = "/work/twice.jazz" in
  let library_file = "/work/sum.jinc" in
  let resolved =
    resolve ~files:[ ("sum.jinc", twice_library) ] file twice_entry
  in
  let short = byte_of_in twice_entry ~within:"param int N = 1;" ~name:"N" in
  let long = byte_of_in twice_entry ~within:"param int N = 8;" ~name:"N" in
  let byte = byte_of_in twice_library ~within:"s = N;" ~name:"N" in
  (match (short, long, byte) with
  | Some short, Some long, Some byte ->
      let _occurrence, readings =
        Jide_engine.Resolve.visit_answers resolved ~file:library_file ~byte
      in
      check "twice: one answer per reading of the file"
        (List.length readings = 2);
      check "twice: the readings name the two namespaces' declarations"
        (match readings with
        | [
         Jide_engine.Resolve.Resolved first; Jide_engine.Resolve.Resolved second;
        ] ->
            first.Jide_engine.Index.name_span.start_byte = short
            && second.Jide_engine.Index.name_span.start_byte = long
        | _ -> false);
      check "twice: the single answer is the first reading's"
        (match
           Jide_engine.Resolve.answer_at resolved ~file:library_file ~byte
         with
        | Some (_, Jide_engine.Resolve.Resolved first) ->
            first.Jide_engine.Index.name_span.start_byte = short
        | _ -> false)
  | _ -> check "twice: the fixture has the three names" false);
  let unread = resolve file twice_entry in
  let _occurrence, none =
    Jide_engine.Resolve.visit_answers unread ~file:library_file ~byte:0
  in
  check "twice: a file the resolution never reached has no reading" (none = [])

let () =
  test_a_name_resolves_to_its_declaration ();
  test_a_byte_order_mark_resolves ();
  test_define_before_use_and_block_scope ();
  test_calls_follow_the_same_rule ();
  test_a_for_variable_must_exist ();
  test_namespaces ();
  test_a_nested_namespace_finds_the_outer_one ();
  test_aliases_globals_and_parameters ();
  test_a_required_file ();
  test_intrinsics_annotations_and_include_names ();
  test_answers_and_unknown_bytes ();
  test_flag_groups_name_variables ();
  test_a_file_read_twice_answers_per_reading ();
  test_the_entry_path_is_normalised ();
  test_identity_across_files ();
  if !failures = 0 then print_endline "all resolve tests passed"
  else (
    Printf.printf "%d resolve test(s) failed\n" !failures;
    exit 1)
