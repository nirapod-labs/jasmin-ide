let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* The tests say where a byte is by quoting the source around it, never by a
   number written by hand: [byte_of_in text ~within ~name] is where [name]
   starts inside the first place [within] sits in the text. *)
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

(* A byte the tests must have: a missing one is a broken test, not a refusal. *)
let byte text ~within ~name =
  match byte_of_in text ~within ~name with
  | Some byte -> byte
  | None ->
      incr failures;
      Printf.printf "FAIL test bug: %s is not inside %s\n%!" name within;
      0

let resolve ?(idirs = []) ?(files = []) file text =
  let read path = List.assoc_opt (Filename.basename path) files in
  Jide_engine.Resolve.of_text ~idirs ~read ~file text

let index_of (resolved : Jide_engine.Resolve.t) path =
  match Jide_engine.Resolve.file resolved path with
  | Some index -> index
  | None -> Jide_engine.Index.of_text ~file:path "" |> fst

(* No file is broken in the tests that do not ask about a broken file. *)
let nothing_broken _ = false

(* The text the edits make: every replacement applied at its own range. This
   is what an editor does with a workspace edit, and it is what lets the tests
   check the result as text rather than as ranges. *)
let apply (text : string) (edits : (string * Jide_engine.Rename.edit list) list)
    =
  let all =
    List.concat_map
      (fun (_path, edits) ->
        List.map
          (fun (edit : Jide_engine.Rename.edit) ->
            ( edit.Jide_engine.Rename.span.Jide_lang.Span.start_byte,
              edit.Jide_engine.Rename.span.Jide_lang.Span.end_byte,
              edit.Jide_engine.Rename.new_text ))
          edits)
      edits
  in
  (* Back to front, so an earlier range's offsets stay valid. *)
  let sorted =
    List.sort (fun (left, _, _) (right, _, _) -> compare right left) all
  in
  List.fold_left
    (fun text (start, stop, new_text) ->
      let start = max 0 (min start (String.length text)) in
      let stop = max start (min stop (String.length text)) in
      String.sub text 0 start ^ new_text
      ^ String.sub text stop (String.length text - stop))
    text sorted

let edits_in edits path =
  match List.assoc_opt path edits with Some edits -> edits | None -> []

let edit_count (edits : (string * Jide_engine.Rename.edit list) list) =
  List.fold_left (fun total (_path, edits) -> total + List.length edits) 0 edits

let renamed ?(files = []) ?(project = []) ?(broken = nothing_broken)
    ?(idirs = []) ~file ~text ~within ~name ~new_name () =
  let resolved = resolve ~idirs ~files file text in
  let index = index_of resolved file in
  let project =
    List.map (fun (path, text) -> resolve ~idirs ~files path text) project
  in
  Jide_engine.Rename.rename ~files:project ~index ~resolve:resolved
    ~byte:(byte text ~within ~name) ~new_name ~broken

let refusal = function
  | Ok edits -> Printf.sprintf "renamed (%d edits)" (edit_count edits)
  | Error reason -> reason

(* ------------------------------------------------------------------ *)

(* A variable renamed across its uses: every occurrence of that one
   declaration, and nothing else. *)
let variables =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 count;\n\
  \  count = a;\n\
  \  count = count + a;\n\
  \  return count;\n\
   }\n"

let test_a_variable_renamed_across_its_uses () =
  let file = "/work/variables.jazz" in
  let result =
    renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
      ~new_name:"total" ()
  in
  match result with
  | Error reason -> check ("variables: the rename is refused: " ^ reason) false
  | Ok edits ->
      let renamed_text = apply variables edits in
      check "variables: every use of the variable is rewritten"
        (String.equal renamed_text
           "fn f(reg u32 a) -> reg u32 {\n\
           \  reg u32 total;\n\
           \  total = a;\n\
           \  total = total + a;\n\
           \  return total;\n\
            }\n");
      check "variables: the parameter a keeps its name"
        (match byte_of renamed_text "fn f(reg u32 a) -> reg u32 {" with
        | Some _ -> true
        | None -> false);
      check "variables: five occurrences are rewritten" (edit_count edits = 5);
      check "variables: every edit is in the document"
        (List.map fst edits = [ file ]);
      check "variables: the document's own name is never rewritten"
        (not (String.equal (apply variables edits) variables))

(* An annotation label is fixed by the compiler: no declaration introduces it,
   so there is nothing a rename could rewrite. *)
let annotated =
  "#[returnaddress=stack]\nfn f(reg u32 x) -> reg u32 { return x; }\n"

let test_an_annotation_label_is_refused () =
  let file = "/work/annotated.jazz" in
  let result =
    renamed ~file ~text:annotated ~within:"#[returnaddress=stack]"
      ~name:"returnaddress" ~new_name:"other" ()
  in
  check
    ("annotation: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.equal reason
          "an annotation label is fixed by the compiler and cannot be renamed"
    | Ok _ -> false);
  check "annotation: prepareRename answers nothing"
    (let resolved = resolve file annotated in
     Jide_engine.Rename.prepare ~index:(index_of resolved file)
       ~resolve:resolved
       ~byte:
         (byte annotated ~within:"#[returnaddress=stack]" ~name:"returnaddress")
     = None)

(* The [from NAME] of a require names an entry of the project's include paths,
   not a symbol of the program. *)
let named_require =
  "from LIB require \"deep.jinc\"\nfn f(reg u32 x) -> reg u32 { return x; }\n"

let test_an_include_path_is_refused () =
  let file = "/work/from.jazz" in
  let files =
    [ ("deep.jinc", "fn deep_f(reg u32 a) -> reg u32 { return a; }\n") ]
  in
  let idirs = [ ("LIB", "/work/lib") ] in
  let result =
    renamed ~files ~idirs ~file ~text:named_require ~within:"from LIB require"
      ~name:"LIB" ~new_name:"OTHER" ()
  in
  check
    ("include path: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.equal reason
          "an include path names a configuration entry, not a symbol"
    | Ok _ -> false);
  check "include path: prepareRename answers nothing"
    (let resolved = resolve ~idirs ~files file named_require in
     Jide_engine.Rename.prepare ~index:(index_of resolved file)
       ~resolve:resolved
       ~byte:(byte named_require ~within:"from LIB require" ~name:"LIB")
     = None)

(* A qualified name carries one range for the whole name. The rename rewrites
   its last segment, which is the part that names the symbol; the namespace
   part is not a symbol a rename can take. *)
let namespaced =
  "namespace A {\n\
  \  fn mul(reg u32 x) -> reg u32 { return x; }\n\
   }\n\
   fn use(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = A::mul(x);\n\
  \  return r;\n\
   }\n"

let test_a_qualified_name_renames_its_last_segment () =
  let file = "/work/namespaced.jazz" in
  match
    renamed ~file ~text:namespaced ~within:"fn mul" ~name:"mul"
      ~new_name:"times" ()
  with
  | Error reason -> check ("qualified: the rename is refused: " ^ reason) false
  | Ok edits ->
      check "qualified: the declaration and the qualified call are rewritten"
        (String.equal (apply namespaced edits)
           "namespace A {\n\
           \  fn times(reg u32 x) -> reg u32 { return x; }\n\
            }\n\
            fn use(reg u32 x) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = A::times(x);\n\
           \  return r;\n\
            }\n");
      check "qualified: two occurrences are rewritten" (edit_count edits = 2)

(* A namespace's own name is refused: a qualified name has one range for the
   whole of it, so there is no range for the [A] a rename would rewrite. *)
let test_a_namespace_name_is_refused () =
  let file = "/work/namespaced.jazz" in
  let result =
    renamed ~file ~text:namespaced ~within:"namespace A" ~name:"A" ~new_name:"B"
      ()
  in
  check
    ("namespace name: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.equal reason
          "a namespace's own name has no range of its own inside a qualified \
           name"
    | Ok _ -> false);
  check "namespace name: prepareRename answers nothing"
    (let resolved = resolve file namespaced in
     Jide_engine.Rename.prepare ~index:(index_of resolved file)
       ~resolve:resolved
       ~byte:(byte namespaced ~within:"namespace A" ~name:"A")
     = None)

(* The cursor on the [A] of [A::mul] is on the namespace, not on the function:
   renaming from there would rewrite a symbol the client never pointed at. *)
let test_a_namespace_prefix_is_refused () =
  let file = "/work/namespaced.jazz" in
  let result =
    renamed ~file ~text:namespaced ~within:"r = A::mul(x);" ~name:"A"
      ~new_name:"other" ()
  in
  check
    ("namespace prefix: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.equal reason
          "the position holds the namespace part of a qualified name, not the \
           name it qualifies"
    | Ok _ -> false);
  check "namespace prefix: prepareRename answers nothing"
    (let resolved = resolve file namespaced in
     Jide_engine.Rename.prepare ~index:(index_of resolved file)
       ~resolve:resolved
       ~byte:(byte namespaced ~within:"r = A::mul(x);" ~name:"A")
     = None)

(* A qualified name written with spaces around [::] has no range for its last
   segment, so no rename can rewrite it: one such occurrence refuses the whole
   rename, rather than replacing the qualified name with the bare new one. *)
let spaced_qualified =
  "namespace A {\n\
  \  fn mul(reg u32 x) -> reg u32 { return x; }\n\
   }\n\
   fn use(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = A :: mul(x);\n\
  \  return r;\n\
   }\n"

let test_a_spaced_qualified_name_is_refused () =
  let file = "/work/spaced.jazz" in
  let at_position =
    renamed ~file ~text:spaced_qualified ~within:"r = A :: mul(x);" ~name:"mul"
      ~new_name:"times" ()
  in
  check
    ("spaced qualified: refused at the position, saying " ^ refusal at_position)
    (match at_position with
    | Error reason ->
        String.equal reason
          "a qualified name written with spaces around :: has no range for its \
           last segment"
    | Ok _ -> false);
  let from_declaration =
    renamed ~file ~text:spaced_qualified ~within:"fn mul" ~name:"mul"
      ~new_name:"times" ()
  in
  check
    ("spaced qualified: refused from the declaration too, saying "
   ^ refusal from_declaration)
    (match from_declaration with
    | Error reason ->
        String.equal reason
          "a qualified name written with spaces around :: has no range for its \
           last segment"
    | Ok _ -> false)

(* A function renamed and its call sites: a function is declared after its
   body, so a call below it resolves and a call above it does not. The rename
   rewrites the declaration and the calls that resolve to it, and leaves a
   same-named call that resolves to nothing alone. *)
let calls =
  "fn add(reg u32 x) -> reg u32 { return x; }\n\
   fn use(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = add(x);\n\
  \  return r;\n\
   }\n"

let test_a_function_renamed_with_its_call_sites () =
  let file = "/work/calls.jazz" in
  match
    renamed ~file ~text:calls ~within:"fn add" ~name:"add" ~new_name:"plus" ()
  with
  | Error reason -> check ("calls: the rename is refused: " ^ reason) false
  | Ok edits ->
      check "calls: the declaration and the call are both rewritten"
        (String.equal (apply calls edits)
           "fn plus(reg u32 x) -> reg u32 { return x; }\n\
            fn use(reg u32 x) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = plus(x);\n\
           \  return r;\n\
            }\n");
      check "calls: two occurrences are rewritten" (edit_count edits = 2)

(* A rename at a call site rewrites the declaration too: the identity is the
   declaration, wherever the client asked from. *)
let test_a_rename_from_a_call_rewrites_the_declaration () =
  let file = "/work/calls.jazz" in
  match
    renamed ~file ~text:calls ~within:"r = add(x);" ~name:"add" ~new_name:"plus"
      ()
  with
  | Error reason -> check ("from a call: refused: " ^ reason) false
  | Ok edits ->
      check "from a call: the declaration is rewritten as well"
        (String.equal (apply calls edits)
           "fn plus(reg u32 x) -> reg u32 { return x; }\n\
            fn use(reg u32 x) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = plus(x);\n\
           \  return r;\n\
            }\n")

(* A keyword is not a name: the lexer has a token for it, so no rename. *)
let test_a_rename_is_refused_on_a_keyword () =
  let file = "/work/variables.jazz" in
  let result =
    renamed ~file ~text:variables ~within:"reg u32 count;" ~name:"reg"
      ~new_name:"other" ()
  in
  check
    ("keyword: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.equal reason "a keyword is not a name a rename can change"
    | Ok _ -> false);
  check "keyword: prepareRename answers nothing at the keyword"
    (let resolved = resolve file variables in
     Jide_engine.Rename.prepare ~index:(index_of resolved file)
       ~resolve:resolved
       ~byte:(byte variables ~within:"reg u32 count;" ~name:"reg")
     = None)

(* A keyword is refused even as the new name, and the reason names it. *)
let test_a_keyword_is_not_a_valid_new_name () =
  let file = "/work/variables.jazz" in
  match
    renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
      ~new_name:"fn" ()
  with
  | Ok _ -> check "new name: a keyword is accepted as a name" false
  | Error reason ->
      check "new name: a keyword is refused, saying what it is"
        (String.equal reason "fn is not a valid Jasmin identifier")

(* An intrinsic is not a declaration: [#spill(x)] is a primitive of the
   compiler, not a name the program introduces. *)
let intrinsics = "fn f(reg u32 x) -> reg u32 {\n  #spill(x);\n  return x;\n}\n"

let test_a_rename_is_refused_on_an_intrinsic () =
  let file = "/work/intrinsics.jazz" in
  let result =
    renamed ~file ~text:intrinsics ~within:"#spill(x);" ~name:"spill"
      ~new_name:"other" ()
  in
  check
    ("intrinsic: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.equal reason
          "an intrinsic is fixed by the compiler and cannot be renamed"
    | Ok _ -> false);
  check "intrinsic: prepareRename answers nothing"
    (let resolved = resolve file intrinsics in
     Jide_engine.Rename.prepare ~index:(index_of resolved file)
       ~resolve:resolved
       ~byte:(byte intrinsics ~within:"#spill(x);" ~name:"spill")
     = None)

(* The new name must lex as one identifier: not empty, not starting with a
   digit, not holding a character the lexer has no rule for, not qualified. *)
let test_an_invalid_identifier_is_refused () =
  let file = "/work/variables.jazz" in
  let refused_for name =
    match
      renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
        ~new_name:name ()
    with
    | Ok _ -> Printf.sprintf "accepted %S" name
    | Error reason -> reason
  in
  check "invalid: an empty name is refused"
    (match
       renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
         ~new_name:"" ()
     with
    | Error reason -> String.equal reason " is not a valid Jasmin identifier"
    | Ok _ -> false);
  check "invalid: a name starting with a digit is refused"
    (String.equal (refused_for "1count")
       "1count is not a valid Jasmin identifier");
  check "invalid: a name with a space is refused"
    (String.equal (refused_for "two words")
       "two words is not a valid Jasmin identifier");
  check "invalid: a name with a hyphen is refused"
    (String.equal (refused_for "two-words")
       "two-words is not a valid Jasmin identifier");
  check "invalid: a qualified name is refused"
    (String.equal (refused_for "A::b") "A::b is not a valid Jasmin identifier");
  check "invalid: a valid name is accepted"
    (String.starts_with ~prefix:"accepted" (refused_for "total"))

(* The rule that keeps a rename safe: a name that a different declaration
   already holds where the rename would write it is refused. Here [count] is
   renamed to [other], but [other] is a live declaration in the same block at
   the byte where [count] is used. *)
let collision =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 count;\n\
  \  reg u32 other;\n\
  \  count = a;\n\
  \  other = a;\n\
  \  return count;\n\
   }\n"

let test_a_collision_with_a_visible_declaration_is_refused () =
  let file = "/work/collision.jazz" in
  let result =
    renamed ~file ~text:collision ~within:"count = a;" ~name:"count"
      ~new_name:"other" ()
  in
  check
    ("collision: refused, saying " ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"other already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/collision.jazz)" reason
    | Ok _ -> false)

(* A collision is not a collision when the other declaration is not in scope
   where the renamed name would be written: [other] is declared in a block
   that ends before [count] is used, so [count] may become [other]. *)
let no_collision =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  if (a < 1) {\n\
  \    reg u32 other;\n\
  \    other = a;\n\
  \  }\n\
  \  reg u32 count;\n\
  \  count = a;\n\
  \  return count;\n\
   }\n"

let test_a_name_out_of_scope_is_not_a_collision () =
  let file = "/work/no-collision.jazz" in
  match
    renamed ~file ~text:no_collision ~within:"count = a;" ~name:"count"
      ~new_name:"other" ()
  with
  | Error reason ->
      check ("no collision: refused, though it is safe: " ^ reason) false
  | Ok edits ->
      check "no collision: the rename goes through"
        (String.equal (apply no_collision edits)
           "fn f(reg u32 a) -> reg u32 {\n\
           \  if (a < 1) {\n\
           \    reg u32 other;\n\
           \    other = a;\n\
           \  }\n\
           \  reg u32 other;\n\
           \  other = a;\n\
           \  return other;\n\
            }\n")

(* Rename and rename back restore the text exactly. This is the text-level
   statement the gate asks for: the two edits composed are the identity. *)
let test_rename_and_rename_back_restores_the_text () =
  let file = "/work/variables.jazz" in
  match
    renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
      ~new_name:"total" ()
  with
  | Error reason ->
      check ("idempotence: the rename is refused: " ^ reason) false
  | Ok forward -> (
      let renamed_text = apply variables forward in
      let resolved = resolve file renamed_text in
      let back =
        Jide_engine.Rename.rename ~files:[] ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte renamed_text ~within:"total = a;" ~name:"total")
          ~new_name:"count" ~broken:nothing_broken
      in
      match back with
      | Error reason ->
          check ("idempotence: renaming back is refused: " ^ reason) false
      | Ok backward ->
          check "idempotence: renaming back restores the text exactly"
            (String.equal (apply renamed_text backward) variables))

(* Two same-named declarations in different blocks are two symbols. A rename
   of one leaves the other's occurrences as they are, and the same name in the
   other block keeps its own name. *)
let two_blocks =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 value;\n\
  \  value = a;\n\
  \  if (a < 1) {\n\
  \    reg u32 value;\n\
  \    value = a;\n\
  \    a = value;\n\
  \  }\n\
  \  return value;\n\
   }\n"

let test_two_same_named_declarations_are_not_conflated () =
  let file = "/work/two-blocks.jazz" in
  (* The inner declaration: renaming it must not touch the outer one. *)
  let inner =
    match
      renamed ~file ~text:two_blocks
        ~within:"reg u32 value;\n    value = a;\n    a = value;" ~name:"value"
        ~new_name:"inner" ()
    with
    | Error reason -> Error reason
    | Ok edits -> Ok (apply two_blocks edits)
  in
  (match inner with
  | Error reason ->
      check ("two blocks: the inner rename is refused: " ^ reason) false
  | Ok text ->
      check "two blocks: the inner declaration and its uses are the inner one"
        (String.equal text
           "fn f(reg u32 a) -> reg u32 {\n\
           \  reg u32 value;\n\
           \  value = a;\n\
           \  if (a < 1) {\n\
           \    reg u32 inner;\n\
           \    inner = a;\n\
           \    a = inner;\n\
           \  }\n\
           \  return value;\n\
            }\n"));
  (* And the outer one, which must leave the inner block alone. *)
  match
    renamed ~file ~text:two_blocks ~within:"reg u32 value;\n  value = a;"
      ~name:"value" ~new_name:"outer" ()
  with
  | Error reason ->
      check ("two blocks: the outer rename is refused: " ^ reason) false
  | Ok edits ->
      check "two blocks: the outer rename leaves the inner block alone"
        (String.equal (apply two_blocks edits)
           "fn f(reg u32 a) -> reg u32 {\n\
           \  reg u32 outer;\n\
           \  outer = a;\n\
           \  if (a < 1) {\n\
           \    reg u32 value;\n\
           \    value = a;\n\
           \    a = value;\n\
           \  }\n\
           \  return outer;\n\
            }\n")

(* A function name and a variable name are different name spaces: renaming a
   variable to a function's name is not a collision. *)
let separate_spaces =
  "fn helper(reg u32 a) -> reg u32 { return a; }\n\
   fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = helper(a);\n\
  \  return x;\n\
   }\n"

let test_a_function_name_is_another_name_space () =
  let file = "/work/spaces.jazz" in
  match
    renamed ~file ~text:separate_spaces ~within:"x = helper(a);" ~name:"x"
      ~new_name:"helper" ()
  with
  | Error reason ->
      check ("name spaces: a variable took a function's name: " ^ reason) false
  | Ok edits ->
      check "name spaces: a variable may take a function's name"
        (String.equal
           (apply separate_spaces edits)
           "fn helper(reg u32 a) -> reg u32 { return a; }\n\
            fn f(reg u32 a) -> reg u32 {\n\
           \  reg u32 helper;\n\
           \  helper = helper(a);\n\
           \  return helper;\n\
            }\n")

(* The require closure rule from design section 10: while any file in the
   document's require closure has recovered syntax errors, no rename. *)
let requiring =
  "require \"lib.jinc\"\n\
   fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 x;\n\
  \  x = a;\n\
  \  return x;\n\
   }\n"

let library = "fn lib_fn(reg u32 a) -> reg u32 { return a; }\n"

let test_a_broken_file_in_the_require_closure_refuses () =
  let file = "/work/main.jazz" in
  let library_file = "/work/lib.jinc" in
  let files = [ ("lib.jinc", library) ] in
  let resolved = resolve ~files file requiring in
  let index = index_of resolved file in
  check "require: the resolution reached the required file"
    (match Jide_engine.Resolve.file resolved library_file with
    | Some _ -> true
    | None -> false);
  let result =
    Jide_engine.Rename.rename ~files:[] ~index ~resolve:resolved
      ~byte:(byte requiring ~within:"x = a;" ~name:"x") ~new_name:"y"
      ~broken:(fun path ->
        String.equal (Jide_engine.Index.normalise path) library_file)
  in
  check
    ("require: refused while a required file is broken, saying "
   ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with
          ~prefix:
            "a file the document requires has recovered syntax errors, so no \
             rename is offered:"
          reason
        && String.ends_with ~suffix:library_file reason
    | Ok _ -> false);
  (* The same rename is allowed once nothing is broken. *)
  check "require: allowed once no required file is broken"
    (match
       Jide_engine.Rename.rename ~files:[] ~index ~resolve:resolved
         ~byte:(byte requiring ~within:"x = a;" ~name:"x")
         ~new_name:"y" ~broken:nothing_broken
     with
    | Ok edits -> edit_count edits = 3
    | Error _ -> false)

(* A rename follows the identity into the files the document requires: the
   edits are grouped by file, and the required file's declaration is one of
   them. *)
let entry_calling_a_required =
  "require \"lib.jinc\"\n\
   fn use(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = lib_fn(a);\n\
  \  return r;\n\
   }\n"

let library_with_one_function =
  "fn lib_fn(reg u32 a) -> reg u32 { return a; }\n"

let test_a_rename_reaches_a_required_file () =
  let file = "/work/entry.jazz" in
  let library = "/work/lib.jinc" in
  let files = [ ("lib.jinc", library_with_one_function) ] in
  match
    renamed ~files ~file ~text:entry_calling_a_required ~within:"r = lib_fn(a);"
      ~name:"lib_fn" ~new_name:"lib_call" ()
  with
  | Error reason -> check ("required: the rename is refused: " ^ reason) false
  | Ok edits ->
      check "required: the entry and the required file are both edited"
        (List.map fst edits = [ file; library ]);
      check "required: the call in the entry is rewritten"
        (String.equal
           (apply entry_calling_a_required [ (file, edits_in edits file) ])
           "require \"lib.jinc\"\n\
            fn use(reg u32 a) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = lib_call(a);\n\
           \  return r;\n\
            }\n");
      check "required: the declaration in the required file is rewritten"
        (String.equal
           (apply library_with_one_function
              [ (library, edits_in edits library) ])
           "fn lib_call(reg u32 a) -> reg u32 { return a; }\n");
      check "required: two edits, one per file" (edit_count edits = 2)

(* A project file the document does not itself reach is searched through the
   caller's list of resolutions: a rename of a workspace symbol is not limited
   to the files the open document read. *)
let declaring_document = "fn shared(reg u32 a) -> reg u32 { return a; }\n"

let calling_project_file =
  "require \"doc.jazz\"\n\
   fn call(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = shared(a);\n\
  \  return r;\n\
   }\n"

let test_a_rename_reaches_a_project_file () =
  let file = "/work/doc.jazz" in
  let other = "/work/other.jazz" in
  let files =
    [ ("doc.jazz", declaring_document); ("other.jazz", calling_project_file) ]
  in
  match
    renamed ~files
      ~project:[ (other, calling_project_file) ]
      ~file ~text:declaring_document ~within:"fn shared" ~name:"shared"
      ~new_name:"common" ()
  with
  | Error reason -> check ("project: the rename is refused: " ^ reason) false
  | Ok edits ->
      check "project: the document and the project file are both edited"
        (List.map fst edits = [ file; other ]);
      check "project: the project file's call is rewritten"
        (String.equal
           (apply calling_project_file [ (other, edits_in edits other) ])
           "require \"doc.jazz\"\n\
            fn call(reg u32 a) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = common(a);\n\
           \  return r;\n\
            }\n");
      check "project: two edits, one per file" (edit_count edits = 2)

(* Nothing to rename: a position on no name, a name with no declaration. *)
let test_positions_that_hold_no_name_are_refused () =
  let file = "/work/variables.jazz" in
  let resolved = resolve file variables in
  let index = index_of resolved file in
  let at text ~within ~name =
    Jide_engine.Rename.prepare ~index ~resolve:resolved
      ~byte:(byte text ~within ~name)
  in
  check "no name: a byte inside the return keyword holds no name"
    (at variables ~within:"return count;" ~name:"return" = None);
  check "no name: a byte on a name with no declaration answers nothing"
    (let unresolved = "fn f(reg u32 a) -> reg u32 {\n  return nothing;\n}\n" in
     let resolved = resolve file unresolved in
     let index = index_of resolved file in
     Jide_engine.Rename.prepare ~index ~resolve:resolved
       ~byte:(byte unresolved ~within:"return nothing;" ~name:"nothing")
     = None);
  check "no name: a name is prepared with its span and its kind"
    (match at variables ~within:"reg u32 count;" ~name:"count" with
    | Some (span, kind) ->
        kind = Jide_engine.Index.Variable
        && span.Jide_lang.Span.start_byte
           = byte variables ~within:"reg u32 count;" ~name:"count"
        && span.Jide_lang.Span.end_byte
           = span.Jide_lang.Span.start_byte + String.length "count"
    | None -> false)

(* A rename to the name the symbol already has changes nothing: every
   occurrence is replaced by itself. *)
let test_a_rename_to_the_same_name_changes_nothing () =
  let file = "/work/variables.jazz" in
  match
    renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
      ~new_name:"count" ()
  with
  | Error reason -> check ("same name: refused: " ^ reason) false
  | Ok edits ->
      check "same name: the text is unchanged"
        (String.equal (apply variables edits) variables);
      check "same name: every occurrence is still an edit" (edit_count edits = 5)

(* The gate's statement, measured: the renamed program gets the same verdict.
   Both texts are checked by the compiler, and the renamed one is as clean as
   the one it came from. *)
let verdict_program =
  "fn add(reg u64 a, reg u64 b) -> reg u64 {\n\
  \  reg u64 c;\n\
  \  c = a + b;\n\
  \  return c;\n\
   }\n\
   fn use(reg u64 x) -> reg u64 {\n\
  \  reg u64 r;\n\
  \  r = add(x, x);\n\
  \  return r;\n\
   }\n"

let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let errors_of text =
  let result =
    Jide_jasmin.Adapter.check x86_64 linux ~file:"/work/verdict.jazz" ~text
      ~idirs:[]
  in
  List.filter
    (fun (diagnostic : Jide_lang.Diagnostic.t) ->
      diagnostic.Jide_lang.Diagnostic.severity = Jide_lang.Diagnostic.Error)
    result.Jide_jasmin.Adapter.diagnostics

let test_the_renamed_program_keeps_its_verdict () =
  let file = "/work/verdict.jazz" in
  match
    renamed ~file ~text:verdict_program ~within:"fn add" ~name:"add"
      ~new_name:"plus" ()
  with
  | Error reason -> check ("verdict: the rename is refused: " ^ reason) false
  | Ok edits ->
      let renamed_text = apply verdict_program edits in
      check "verdict: the program before the rename has no error"
        (errors_of verdict_program = []);
      check "verdict: the program after the rename has no error"
        (errors_of renamed_text = [])

let () =
  test_a_variable_renamed_across_its_uses ();
  test_a_function_renamed_with_its_call_sites ();
  test_a_rename_from_a_call_rewrites_the_declaration ();
  test_a_rename_is_refused_on_a_keyword ();
  test_a_keyword_is_not_a_valid_new_name ();
  test_a_rename_is_refused_on_an_intrinsic ();
  test_an_annotation_label_is_refused ();
  test_an_include_path_is_refused ();
  test_a_qualified_name_renames_its_last_segment ();
  test_a_namespace_name_is_refused ();
  test_a_namespace_prefix_is_refused ();
  test_a_spaced_qualified_name_is_refused ();
  test_an_invalid_identifier_is_refused ();
  test_a_collision_with_a_visible_declaration_is_refused ();
  test_a_name_out_of_scope_is_not_a_collision ();
  test_rename_and_rename_back_restores_the_text ();
  test_two_same_named_declarations_are_not_conflated ();
  test_a_function_name_is_another_name_space ();
  test_a_broken_file_in_the_require_closure_refuses ();
  test_a_rename_reaches_a_required_file ();
  test_a_rename_reaches_a_project_file ();
  test_positions_that_hold_no_name_are_refused ();
  test_a_rename_to_the_same_name_changes_nothing ();
  test_the_renamed_program_keeps_its_verdict ();
  if !failures = 0 then print_endline "all rename tests passed"
  else (
    Printf.printf "%d rename test(s) failed\n" !failures;
    exit 1)
