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
