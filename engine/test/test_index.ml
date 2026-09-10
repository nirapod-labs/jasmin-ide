let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then false
    else if String.equal (String.sub text index fragment_length) fragment then
      true
    else from (index + 1)
  in
  fragment_length = 0 || from 0

(* The first place a fragment sits in the text. The tests say where a name is
   by quoting the source around it, never by a byte offset written by hand. *)
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

let kind_text (kind : Jide_engine.Index.kind) =
  match kind with
  | Function -> "function"
  | Parameter -> "parameter"
  | Variable -> "variable"
  | Global -> "global"
  | Namespace -> "namespace"
  | Type_alias -> "alias"
  | Exec -> "exec"
  | Require -> "require"
  | Intrinsic -> "intrinsic"
  | Annotation -> "annotation"
  | Include -> "include"
  | Unknown -> "unknown"

let role_text (role : Jide_engine.Index.role) =
  match role with
  | Call -> "a call"
  | Var -> "a variable"
  | Type -> "a type"
  | Prim -> "a primitive"
  | Annot -> "an annotation"
  | Include_path -> "an include path"
  | Declared kind -> "declares a " ^ kind_text kind

let of_text = Jide_engine.Index.of_text
let text_of = Jide_engine.Index.text_of_span

let declaration_named (index : Jide_engine.Index.t) name kind =
  List.find_opt
    (fun (declaration : Jide_engine.Index.declaration) ->
      String.equal declaration.name name && declaration.kind = kind)
    index.declarations

let function_text =
  "fn f(reg u32 a) -> reg u32 {\n  reg u32 b;\n  b = a;\n  return b;\n}\n"

(* Every identifier the walk meets, in source order, with what it does. *)
let test_the_occurrences_in_source_order () =
  let index, diagnostics = of_text ~file:"/work/f.jazz" function_text in
  check "occurrences: the file parses" (diagnostics = []);
  check "occurrences: the names in source order"
    (List.map
       (fun (occurrence : Jide_engine.Index.occurrence) -> occurrence.name)
       index.Jide_engine.Index.occurrences
    = [ "f"; "a"; "b"; "b"; "a"; "b" ]);
  check "occurrences: what each one does"
    (List.map
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         role_text occurrence.role)
       index.Jide_engine.Index.occurrences
    = [
        "declares a function";
        "declares a parameter";
        "declares a variable";
        "a variable";
        "a variable";
        "a variable";
      ]);
  check "occurrences: every range covers the name it names"
    (List.for_all
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal (text_of function_text occurrence.span) occurrence.name)
       index.Jide_engine.Index.occurrences);
  check "occurrences: an unqualified name is its own last segment"
    (List.for_all
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         Jide_lang.Span.compare occurrence.span occurrence.last = 0)
       index.Jide_engine.Index.occurrences)

(* A declaration carries the name range and the range of the item it makes,
   which is what hover and the outline need. *)
let test_the_declarations_carry_two_ranges () =
  let index, _diagnostics = of_text ~file:"/work/f.jazz" function_text in
  check "declarations: one per name introduced"
    (List.map
       (fun (declaration : Jide_engine.Index.declaration) ->
         (declaration.name, kind_text declaration.kind))
       index.Jide_engine.Index.declarations
    = [ ("f", "function"); ("a", "parameter"); ("b", "variable") ]);
  let function_ = declaration_named index "f" Jide_engine.Index.Function in
  check "declarations: the function's name range"
    (match function_ with
    | Some declaration -> (
        String.equal (text_of function_text declaration.name_span) "f"
        && declaration.name_span.Jide_lang.Span.start_byte
           =
           match byte_of function_text "fn f" with
           | Some byte -> byte + 3
           | None -> -1)
    | None -> false);
  check "declarations: the function's item range is the whole definition"
    (match function_ with
    | Some declaration ->
        String.equal
          (text_of function_text declaration.item_span)
          (String.sub function_text 0 (String.length function_text - 1))
    | None -> false);
  check "declarations: a variable's item range is its declaration"
    (match declaration_named index "b" Jide_engine.Index.Variable with
    | Some declaration ->
        String.equal (text_of function_text declaration.item_span) "reg u32 b;"
    | None -> false);

  (* The detail is text taken from the file, never invented. *)
  check "declarations: a function's detail is its signature"
    (match function_ with
    | Some declaration -> declaration.detail = Some "fn f(reg u32 a) -> reg u32"
    | None -> false);
  check "declarations: a variable's detail is its type"
    (match declaration_named index "b" Jide_engine.Index.Variable with
    | Some declaration -> declaration.detail = Some "reg u32"
    | None -> false);
  check "declarations: every detail is a substring of the file"
    (List.for_all
       (fun (declaration : Jide_engine.Index.declaration) ->
         match declaration.detail with
         | None -> false
         | Some detail -> contains function_text detail)
       index.Jide_engine.Index.declarations)

let require_text =
  "require \"lib.jinc\"\nfn main(reg u32 a) -> reg u32 { return a; }\n"

let test_the_require_edges () =
  let index, diagnostics = of_text ~file:"/work/main.jazz" require_text in
  check "require: the file parses" (diagnostics = []);
  check "require: one edge, as written"
    (List.map
       (fun (require : Jide_engine.Index.require) -> require.written)
       index.Jide_engine.Index.requires
    = [ "lib.jinc" ]);
  check "require: the range covers the string literal with its quotes"
    (match index.Jide_engine.Index.requires with
    | [ require ] ->
        String.equal (text_of require_text require.written_span) "\"lib.jinc\""
    | _ -> false);
  check "require: the item range is the statement"
    (match index.Jide_engine.Index.requires with
    | [ require ] ->
        String.equal
          (text_of require_text require.item_span)
          "require \"lib.jinc\""
    | _ -> false);
  check "require: no include name"
    (match index.Jide_engine.Index.requires with
    | [ require ] -> require.from = None
    | _ -> false);
  check "require: the outline nests the file under the statement"
    (match index.Jide_engine.Index.outline with
    | [ statement; function_ ] ->
        statement.Jide_engine.Index.item_kind = Jide_engine.Index.Require
        && statement.Jide_engine.Index.item_name = None
        && List.map
             (fun (child : Jide_engine.Index.item) -> child.item_name)
             statement.Jide_engine.Index.item_children
           = [ Some "lib.jinc" ]
        && function_.Jide_engine.Index.item_name = Some "main"
    | _ -> false)

let test_a_named_require () =
  let text = "from LIB require \"deep.jinc\"\n" in
  let index, diagnostics = of_text ~file:"/work/main.jazz" text in
  check "named require: the file parses" (diagnostics = []);
  check "named require: the name and its range"
    (match index.Jide_engine.Index.requires with
    | [ require ] -> (
        match require.from with
        | Some (name, span) ->
            String.equal name "LIB" && String.equal (text_of text span) "LIB"
        | None -> false)
    | _ -> false);
  check "named require: the name is an occurrence"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "LIB"
         && occurrence.role = Jide_engine.Index.Include_path)
       index.Jide_engine.Index.occurrences)

let namespace_text =
  "namespace A {\n  fn f(reg u32 a) -> reg u32 { return a; }\n}\n"

let test_the_outline_nests_namespaces () =
  let index, diagnostics = of_text ~file:"/work/ns.jazz" namespace_text in
  check "namespace: the file parses" (diagnostics = []);
  check "namespace: one node, with its function inside"
    (match index.Jide_engine.Index.outline with
    | [ namespace ] ->
        namespace.Jide_engine.Index.item_kind = Jide_engine.Index.Namespace
        && namespace.Jide_engine.Index.item_name = Some "A"
        && List.map
             (fun (child : Jide_engine.Index.item) ->
               (kind_text child.item_kind, child.item_name))
             namespace.Jide_engine.Index.item_children
           = [ ("function", Some "f") ]
    | _ -> false);
  check "namespace: it is also a declaration"
    (match declaration_named index "A" Jide_engine.Index.Namespace with
    | Some declaration ->
        String.equal
          (text_of namespace_text declaration.item_span)
          (String.sub namespace_text 0 (String.length namespace_text - 1))
    | None -> false)

let qualified_text =
  "namespace A {\n\
  \  fn f(reg u32 a) -> reg u32 { return a; }\n\
   }\n\
   fn use(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = A::f(a);\n\
  \  return x;\n\
   }\n"

(* [A::B::x] has one range and the last segment has its own. *)
let test_the_last_segment_of_a_qualified_name () =
  let index, diagnostics = of_text ~file:"/work/q.jazz" qualified_text in
  check "qualified: the file parses" (diagnostics = []);
  let qualified =
    List.find_opt
      (fun (occurrence : Jide_engine.Index.occurrence) ->
        String.equal occurrence.name "A::f")
      index.Jide_engine.Index.occurrences
  in
  check "qualified: the whole name is the range"
    (match qualified with
    | Some occurrence ->
        String.equal (text_of qualified_text occurrence.span) "A::f"
    | None -> false);
  check "qualified: the last segment is the name after the last ::"
    (match qualified with
    | Some occurrence ->
        String.equal (text_of qualified_text occurrence.last) "f"
    | None -> false);
  check "qualified: an unqualified name keeps the whole range"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "x"
         && Jide_lang.Span.compare occurrence.span occurrence.last = 0)
       index.Jide_engine.Index.occurrences)

let test_find_occurrence_and_find_declaration () =
  let index, _diagnostics = of_text ~file:"/work/f.jazz" function_text in
  let assignment =
    match byte_of function_text "b = a;" with Some byte -> byte | None -> -1
  in
  check "find: the occurrence holding a byte of a use"
    (match Jide_engine.Index.find_occurrence index ~byte:assignment with
    | Some occurrence ->
        String.equal occurrence.name "b"
        && occurrence.role = Jide_engine.Index.Var
    | None -> false);
  check "find: the end of a range is excluded"
    (Jide_engine.Index.find_occurrence index ~byte:(assignment + 1) = None);
  check "find: the declaration whose name begins there"
    (match byte_of function_text "reg u32 b;" with
    | None -> false
    | Some start -> (
        match
          Jide_engine.Index.find_declaration index ~name_start:(start + 8)
        with
        | Some declaration ->
            String.equal declaration.name "b"
            && declaration.kind = Jide_engine.Index.Variable
        | None -> false));
  check "find: no declaration at a use"
    (Jide_engine.Index.find_declaration index ~name_start:assignment = None)

(* The identity of a symbol is the file and the byte of its name. *)
let test_equal_declaration () =
  let left, _ = of_text ~file:"/work/f.jazz" function_text in
  let right, _ = of_text ~file:"/work/f.jazz" function_text in
  let other, _ = of_text ~file:"/work/g.jazz" function_text in
  check "identity: the same file and name agree"
    (match
       ( declaration_named left "b" Jide_engine.Index.Variable,
         declaration_named right "b" Jide_engine.Index.Variable )
     with
    | Some left, Some right -> Jide_engine.Index.equal_declaration left right
    | _ -> false);
  check "identity: another file is another symbol"
    (match
       ( declaration_named left "b" Jide_engine.Index.Variable,
         declaration_named other "b" Jide_engine.Index.Variable )
     with
    | Some left, Some other ->
        not (Jide_engine.Index.equal_declaration left other)
    | _ -> false);
  check "identity: the name does not enter it, the range does"
    (match
       ( declaration_named left "f" Jide_engine.Index.Function,
         declaration_named left "a" Jide_engine.Index.Parameter )
     with
    | Some function_, Some parameter ->
        not (Jide_engine.Index.equal_declaration function_ parameter)
    | _ -> false)

(* A byte-order mark is not part of the program: the parser is given the same
   text with the three mark bytes replaced by three spaces, so every range,
   the tree's included, still indexes the text as the caller holds it. *)
let test_a_byte_order_mark_shifts_nothing () =
  let text = "\xEF\xBB\xBF" ^ function_text in
  let index, diagnostics = of_text ~file:"/work/bom.jazz" text in
  let plain, _ = of_text ~file:"/work/plain.jazz" function_text in
  let shift = String.length text - String.length function_text in
  check "bom: the file parses" (diagnostics = []);
  check "bom: the ranges index the caller's text"
    (List.for_all
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal (text_of text occurrence.span) occurrence.name)
       index.Jide_engine.Index.occurrences);
  check "bom: the item starts after the mark"
    (match
       ( byte_of text "fn f",
         declaration_named index "f" Jide_engine.Index.Function )
     with
    | Some start, Some declaration ->
        declaration.name_span.Jide_lang.Span.start_byte = start + 3
    | _ -> false);
  check "bom: the outline names the item three bytes on"
    (match
       (plain.Jide_engine.Index.outline, index.Jide_engine.Index.outline)
     with
    | [ plain_item ], [ item ] -> (
        match
          ( plain_item.Jide_engine.Index.item_name_span,
            item.Jide_engine.Index.item_name_span )
        with
        | Some plain_span, Some span ->
            span.Jide_lang.Span.start_byte
            = plain_span.Jide_lang.Span.start_byte + shift
            && span.Jide_lang.Span.end_byte
               = plain_span.Jide_lang.Span.end_byte + shift
        | _ -> false)
    | _ -> false);
  check "bom: the outline's item ends three bytes on"
    (match
       (plain.Jide_engine.Index.outline, index.Jide_engine.Index.outline)
     with
    | [ plain_item ], [ item ] ->
        item.Jide_engine.Index.item_span.Jide_lang.Span.end_byte
        = plain_item.Jide_engine.Index.item_span.Jide_lang.Span.end_byte + shift
    | _ -> false);
  check "bom: the detail is cut from the bytes it names"
    (match
       ( declaration_named plain "f" Jide_engine.Index.Function,
         declaration_named index "f" Jide_engine.Index.Function )
     with
    | Some plain_declaration, Some declaration ->
        declaration.detail = plain_declaration.detail
    | _ -> false)

let test_a_file_that_does_not_parse () =
  let index, diagnostics = of_text ~file:"/work/broken.jazz" "fn f( {\n" in
  check "broken: the parser says so" (diagnostics <> []);
  check "broken: no occurrences" (index.Jide_engine.Index.occurrences = []);
  check "broken: no declarations" (index.Jide_engine.Index.declarations = []);
  check "broken: no outline" (index.Jide_engine.Index.outline = [])

let test_an_annotation_carries_its_value () =
  let text =
    "#[mmx]\n#[returnaddress=stack]\nfn f(reg u32 a) -> reg u32 { return a; }\n"
  in
  let index, diagnostics = of_text ~file:"/work/annot.jazz" text in
  check "annotation: the file parses" (diagnostics = []);
  check "annotation: a label is an occurrence over the label only"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "returnaddress"
         && occurrence.role = Jide_engine.Index.Annot
         && String.equal (text_of text occurrence.span) "returnaddress")
       index.Jide_engine.Index.occurrences);
  check "annotation: a keyword value is kept with the label"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "returnaddress"
         && occurrence.value = Some "stack")
       index.Jide_engine.Index.occurrences);
  check "annotation: a label with no value carries none"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "mmx" && occurrence.value = None)
       index.Jide_engine.Index.occurrences)

(* A quoted string is a [pexpr_noarr] in the grammar, so it arrives as
   [PAexpr (PEstring _)] and not as a keyword's [PAstring]. Both spellings are
   a string value, and hover needs the quoted ones: [#[ct]] and [#[sct]] carry
   the contract of a function. *)
let test_a_quoted_annotation_carries_its_string () =
  let text =
    "#[ct=\"public -> public\"]\n\
     #[sct=\"secret\"]\n\
     #[returnaddress=stack]\n\
     #[stacksize=64]\n\
     #[mmx]\n\
     fn f(reg u32 a) -> reg u32 { return a; }\n"
  in
  let index, diagnostics = of_text ~file:"/work/quoted.jazz" text in
  let value_of label =
    List.find_map
      (fun (occurrence : Jide_engine.Index.occurrence) ->
        if
          occurrence.role = Jide_engine.Index.Annot
          && String.equal occurrence.name label
        then Some occurrence.value
        else None)
      index.Jide_engine.Index.occurrences
  in
  check "quoted annotation: the file parses" (diagnostics = []);
  check "quoted annotation: ct carries the string it was given"
    (value_of "ct" = Some (Some "public -> public"));
  check "quoted annotation: sct carries its string too"
    (value_of "sct" = Some (Some "secret"));
  check "quoted annotation: a keyword value still carries its word"
    (value_of "returnaddress" = Some (Some "stack"));
  check "quoted annotation: an integer value has no string"
    (value_of "stacksize" = Some None);
  check "quoted annotation: a label with no value has none"
    (value_of "mmx" = Some None);
  check "quoted annotation: the label's range is the label"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "ct"
         && String.equal (text_of text occurrence.span) "ct")
       index.Jide_engine.Index.occurrences);

  (* The value is the lexer's, with escapes resolved, because that is what
     the compiler's own annotation string holds: hover and a check of the
     same file must not disagree. *)
  let escaped, _ =
    of_text ~file:"/work/escaped.jazz"
      "#[ct=\"a\\tb\"]\nfn f(reg u32 a) -> reg u32 { return a; }\n"
  in
  check "quoted annotation: an escape is resolved, as the compiler resolves it"
    (List.exists
       (fun (occurrence : Jide_engine.Index.occurrence) ->
         String.equal occurrence.name "ct" && occurrence.value = Some "a\tb")
       escaped.Jide_engine.Index.occurrences)

(* One spelling per path, and the same one as Context's. *)
let test_normalise_agrees_with_the_context () =
  let paths =
    [
      "./a/./b/../c.jinc";
      "/a//b/../c.jinc";
      "/../a.jinc";
      "a.jinc";
      "/work/lib/a.jinc";
    ]
  in
  check "normalise: the engine and the resolver write one spelling"
    (List.for_all
       (fun path ->
         String.equal
           (Jide_engine.Index.normalise path)
           (Jide_engine.Context.normalise path))
       paths);
  check "normalise: a relative path"
    (String.equal (Jide_engine.Index.normalise "./a/./b/../c.jinc") "a/c.jinc");
  check "normalise: an absolute path"
    (String.equal (Jide_engine.Index.normalise "/a//b/../c.jinc") "/a/c.jinc")

let () =
  test_the_occurrences_in_source_order ();
  test_the_declarations_carry_two_ranges ();
  test_the_require_edges ();
  test_a_named_require ();
  test_the_outline_nests_namespaces ();
  test_the_last_segment_of_a_qualified_name ();
  test_find_occurrence_and_find_declaration ();
  test_equal_declaration ();
  test_a_byte_order_mark_shifts_nothing ();
  test_a_file_that_does_not_parse ();
  test_an_annotation_carries_its_value ();
  test_a_quoted_annotation_carries_its_string ();
  test_normalise_agrees_with_the_context ();
  if !failures = 0 then print_endline "all index tests passed"
  else (
    Printf.printf "%d index test(s) failed\n" !failures;
    exit 1)
