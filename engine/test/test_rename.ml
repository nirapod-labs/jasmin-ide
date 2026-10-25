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

(* prepareRename answers a range and a kind, or the reason it offers nothing. *)
let refused
    (result : (Jide_lang.Span.t * Jide_engine.Index.kind, string) result) =
  match result with Error _ -> true | Ok _ -> false

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
     refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:
            (byte annotated ~within:"#[returnaddress=stack]"
               ~name:"returnaddress")
          ~broken:nothing_broken))

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
     refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte named_require ~within:"from LIB require" ~name:"LIB")
          ~broken:nothing_broken))

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

(* A declaration may carry a qualified name too: [inline fn ble::e] is one
   function, and the compiler accepts it beside a call written [ble::e()]. The
   last segment of the declaration is the range a rename rewrites, as it is for
   a use. *)
let qualified_declaration =
  "inline fn ble::e () -> reg u32 {\n\
  \  reg u32 x = 0;\n\
  \  return x;\n\
   }\n\
   export fn out() -> reg u32 {\n\
  \  reg u32 x = ble::e();\n\
  \  return x;\n\
   }\n"

let test_a_qualified_declaration_renames_its_last_segment () =
  let file = "/work/qualified-declaration.jazz" in
  match
    renamed ~file ~text:qualified_declaration ~within:"::e ()" ~name:"e"
      ~new_name:"times" ()
  with
  | Error reason ->
      check ("qualified declaration: the rename is refused: " ^ reason) false
  | Ok edits ->
      check "qualified declaration: the declaration and the call are rewritten"
        (String.equal
           (apply qualified_declaration edits)
           "inline fn ble::times () -> reg u32 {\n\
           \  reg u32 x = 0;\n\
           \  return x;\n\
            }\n\
            export fn out() -> reg u32 {\n\
           \  reg u32 x = ble::times();\n\
           \  return x;\n\
            }\n");
      check "qualified declaration: two occurrences are rewritten"
        (edit_count edits = 2)

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
     refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte namespaced ~within:"namespace A" ~name:"A")
          ~broken:nothing_broken))

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
     refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte namespaced ~within:"r = A::mul(x);" ~name:"A")
          ~broken:nothing_broken))

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
     refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte variables ~within:"reg u32 count;" ~name:"reg")
          ~broken:nothing_broken))

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
     refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte intrinsics ~within:"#spill(x);" ~name:"spill")
          ~broken:nothing_broken))

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

(* The rule has two ends. The scope the resolver built at an occurrence is the
   scope before it, because the walk is define-before-use, so a name declared
   later in the same block is invisible there. Renaming [count] to [total]
   would declare [total] twice in one block all the same, and the compiler
   reports it. *)
let later_variable =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 count;\n\
  \  count = a;\n\
  \  reg u32 total;\n\
  \  total = a;\n\
  \  return total;\n\
   }\n"

let test_a_later_declaration_in_the_same_block_is_refused () =
  let file = "/work/later-variable.jazz" in
  let result =
    renamed ~file ~text:later_variable ~within:"count = a;" ~name:"count"
      ~new_name:"total" ()
  in
  check
    ("later declaration: a variable declared later in the same block, saying "
   ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"total already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/later-variable.jazz)" reason
    | Ok edits ->
        (* This is what the one-ended rule answered: a second [total] in one
           block, which the compiler warns about. *)
        Printf.printf "later declaration: accepted, %d edit(s):\n%s\n%!"
          (edit_count edits)
          (apply later_variable edits);
        false)

(* A function's scope is its file, so a function declared after it is in that
   scope: renaming [foo] to [bar] here declares [bar] twice, which the
   compiler reports as an error rather than a warning. *)
let later_function =
  "fn foo(reg u32 a) -> reg u32 { return a; }\n\
   fn bar(reg u32 a) -> reg u32 { return a; }\n"

let test_a_later_function_in_the_same_file_is_refused () =
  let file = "/work/later-function.jazz" in
  let result =
    renamed ~file ~text:later_function ~within:"fn foo" ~name:"foo"
      ~new_name:"bar" ()
  in
  check
    ("later declaration: a function declared later in the file, saying "
   ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"bar already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/later-function.jazz)" reason
    | Ok edits ->
        Printf.printf "later declaration: accepted, %d edit(s):\n%s\n%!"
          (edit_count edits)
          (apply later_function edits);
        false)

(* The same rule reaches a declaration written later inside a nested block:
   the block's own declaration shadows nothing, so declaring the renamed name
   there as well is a duplicate in the block that holds both. *)
let later_nested =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 count;\n\
  \  count = a;\n\
  \  if (a < 1) {\n\
  \    reg u32 total;\n\
  \    total = a;\n\
  \  }\n\
  \  return count;\n\
   }\n"

let test_a_later_declaration_in_a_nested_block_is_refused () =
  let file = "/work/later-nested.jazz" in
  let result =
    renamed ~file ~text:later_nested ~within:"count = a;" ~name:"count"
      ~new_name:"total" ()
  in
  check
    ("later declaration: a variable declared later in a nested block, saying "
   ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"total already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/later-nested.jazz)" reason
    | Ok _ -> false)

(* The scope a declaration belongs to is not the scope it is visible from. A
   function written [foo] in a namespace reaches the parent as [A::foo], so a
   top-level [bar] declared after the namespace is a different name in the
   compiler's map: giving [A::foo] the name [bar] declares no duplicate. *)
let namespaced_sibling =
  "namespace A {\n\
  \  fn foo(reg u32 a) -> reg u32 { return a; }\n\
   }\n\
   fn bar(reg u32 a) -> reg u32 { return a; }\n"

let test_a_name_in_another_namespace_is_not_a_collision () =
  let file = "/work/namespaced-sibling.jazz" in
  match
    renamed ~file ~text:namespaced_sibling ~within:"fn foo" ~name:"foo"
      ~new_name:"bar" ()
  with
  | Error reason ->
      check ("namespaces: a name in another scope was refused: " ^ reason) false
  | Ok edits ->
      check "namespaces: a name in another scope is not a collision"
        (String.equal
           (apply namespaced_sibling edits)
           "namespace A {\n\
           \  fn bar(reg u32 a) -> reg u32 { return a; }\n\
            }\n\
            fn bar(reg u32 a) -> reg u32 { return a; }\n")

(* The same statement for a variable, in both directions. The frame a
   declaration lands in is not the scope it is visible from: a variable
   written inside a namespace is pushed into that namespace's own frame and
   merged into its parent as [A::total], so a top-level [count] renamed to
   [total] is a different name in the frame the merge fills, and the compiler
   accepts the renamed program. This is the direction the function test above
   does not cover: there the declaration inside the namespace is the one
   renamed, and a function is pushed by its unqualified name, which is why
   that direction is refused. *)
let namespace_variable_outside =
  "u32 count = 0;\n\
   fn use(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = a;\n\
  \  r += count;\n\
  \  return r;\n\
   }\n\
   namespace A {\n\
  \  u32 total = 0;\n\
   }\n"

let namespace_variable_inside =
  "namespace A {\n\
  \  u32 count = 0;\n\
  \  fn use(reg u32 a) -> reg u32 {\n\
  \    reg u32 r;\n\
  \    r = a;\n\
  \    r += count;\n\
  \    return r;\n\
  \  }\n\
   }\n\
   u32 total = 0;\n"

let test_a_variable_in_another_namespace_is_not_a_collision () =
  let file = "/work/namespace-variable.jazz" in
  (match
     renamed ~file ~text:namespace_variable_outside ~within:"u32 count = 0;"
       ~name:"count" ~new_name:"total" ()
   with
  | Error reason ->
      check
        ("namespaces: a top-level variable renamed beside a namespace's \
          variable was refused: " ^ reason)
        false
  | Ok edits ->
      check
        "namespaces: a top-level variable takes the name of a variable in a \
         namespace"
        (String.equal
           (apply namespace_variable_outside edits)
           "u32 total = 0;\n\
            fn use(reg u32 a) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = a;\n\
           \  r += total;\n\
           \  return r;\n\
            }\n\
            namespace A {\n\
           \  u32 total = 0;\n\
            }\n"));
  (* And the mirror: the declaration inside the namespace is the one renamed,
     and the top-level declaration of the new name is not in its frame. *)
  let file = "/work/namespace-target.jazz" in
  match
    renamed ~file ~text:namespace_variable_inside ~within:"u32 count = 0;"
      ~name:"count" ~new_name:"total" ()
  with
  | Error reason ->
      check
        ("namespaces: a variable in a namespace renamed beside a top-level \
          variable was refused: " ^ reason)
        false
  | Ok edits ->
      check
        "namespaces: a variable in a namespace takes the name of a top-level \
         variable"
        (String.equal
           (apply namespace_variable_inside edits)
           "namespace A {\n\
           \  u32 total = 0;\n\
           \  fn use(reg u32 a) -> reg u32 {\n\
           \    reg u32 r;\n\
           \    r = a;\n\
           \    r += total;\n\
           \    return r;\n\
           \  }\n\
            }\n\
            u32 total = 0;\n")

(* The mirror of the test above for a function is a refusal, and the rule has
   to keep it: the compiler pushes a function by its unqualified name and
   looks through every enclosing frame, so [A::bar] and a top-level [bar] are
   one name to it and the renamed program is a hard error. *)
let function_in_a_namespace =
  "fn foo(reg u32 a) -> reg u32 { return a; }\n\
   namespace A {\n\
  \  fn bar(reg u32 a) -> reg u32 { return a; }\n\
   }\n"

let test_a_function_in_another_namespace_is_a_collision () =
  let file = "/work/namespace-function.jazz" in
  let result =
    renamed ~file ~text:function_in_a_namespace ~within:"fn foo" ~name:"foo"
      ~new_name:"bar" ()
  in
  check
    ("namespaces: a function renamed to a namespace function's name, saying "
   ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"bar already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/namespace-function.jazz)" reason
    | Ok _ -> false)

(* A namespace's own name is in none of the compiler's three name spaces: a
   function or a variable called [A] may sit beside [namespace A], so taking
   the name [A] is not a collision with the namespace. *)
let namespace_named_like_a_value =
  "namespace A {\n\
  \  fn f(reg u32 x) -> reg u32 { return x; }\n\
   }\n\
   fn g(reg u32 x) -> reg u32 { return x; }\n"

(* The variable's version of the same program, with the namespace written
   after the value: that is the order in which the old rule saw the namespace
   during the rule's second end, because the value is then already in scope
   where the namespace's name is written. The compiler accepts both orders
   (probed with jasminc 2026.09.0). The use is an assignment, not
   [return x + g;]: the grammar's return takes a tuple of variables
   ([RETURN vs=tuple(var) SEMICOLON]), so the expression would not parse and
   the occurrence would not exist for either the rule or the test to see. *)
let namespace_named_like_a_value_variable =
  "u32 g = 0;\n\
   fn use(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = x;\n\
  \  r += g;\n\
  \  return r;\n\
   }\n\
   namespace A {\n\
  \  fn f(reg u32 x) -> reg u32 { return x; }\n\
   }\n"

let test_a_namespace_name_is_not_a_value_collision () =
  let file = "/work/namespace-name.jazz" in
  (match
     renamed ~file ~text:namespace_named_like_a_value ~within:"fn g" ~name:"g"
       ~new_name:"A" ()
   with
  | Error reason ->
      check
        ("namespaces: a function renamed to a namespace's own name was \
          refused: " ^ reason)
        false
  | Ok edits ->
      check "namespaces: a function may take a namespace's own name"
        (String.equal
           (apply namespace_named_like_a_value edits)
           "namespace A {\n\
           \  fn f(reg u32 x) -> reg u32 { return x; }\n\
            }\n\
            fn A(reg u32 x) -> reg u32 { return x; }\n"));
  let file = "/work/namespace-name-variable.jazz" in
  match
    renamed ~file ~text:namespace_named_like_a_value_variable
      ~within:"u32 g = 0;" ~name:"g" ~new_name:"A" ()
  with
  | Error reason ->
      check
        ("namespaces: a variable renamed to a namespace's own name was \
          refused: " ^ reason)
        false
  | Ok edits ->
      check "namespaces: a variable may take a namespace's own name"
        (String.equal
           (apply namespace_named_like_a_value_variable edits)
           "u32 A = 0;\n\
            fn use(reg u32 x) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = x;\n\
           \  r += A;\n\
           \  return r;\n\
            }\n\
            namespace A {\n\
           \  fn f(reg u32 x) -> reg u32 { return x; }\n\
            }\n")

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

(* The require closure rule: while any file in the
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
  let library_is_broken path =
    String.equal (Jide_engine.Index.normalise path) library_file
  in
  let at = byte requiring ~within:"x = a;" ~name:"x" in
  let result =
    Jide_engine.Rename.rename ~files:[] ~index ~resolve:resolved ~byte:at
      ~new_name:"y" ~broken:library_is_broken
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
  (* prepareRename refuses for the same reason and with the same message: a
     client is never offered a name the following rename would not write. *)
  check
    ("require: prepareRename refuses with the rename's own message, saying "
    ^
    match
      Jide_engine.Rename.prepare ~index ~resolve:resolved ~byte:at
        ~broken:library_is_broken
    with
    | Error reason -> reason
    | Ok _ -> "an offered range")
    (match
       ( result,
         Jide_engine.Rename.prepare ~index ~resolve:resolved ~byte:at
           ~broken:library_is_broken )
     with
    | Error rename_reason, Error prepare_reason ->
        String.equal prepare_reason rename_reason
    | _ -> false);
  (* The same rename is allowed once nothing is broken, and prepareRename
     offers it again. *)
  check "require: allowed once no required file is broken"
    (match
       Jide_engine.Rename.rename ~files:[] ~index ~resolve:resolved ~byte:at
         ~new_name:"y" ~broken:nothing_broken
     with
    | Ok edits -> edit_count edits = 3
    | Error _ -> false);
  check "require: prepareRename offers the name once nothing is broken"
    (match
       Jide_engine.Rename.prepare ~index ~resolve:resolved ~byte:at
         ~broken:nothing_broken
     with
    | Ok (span, kind) ->
        kind = Jide_engine.Index.Variable
        && span.Jide_lang.Span.start_byte = at
        && span.Jide_lang.Span.end_byte = at + String.length "x"
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

(* The other end of the collision rule is asked in every file the search
   found, not only the document: a global declared in a required file lands in
   the same frame as a top-level declaration of the document, so renaming the
   document's global to its name would declare the frame's name twice. *)
let cross_file_document =
  "u32 count = 0;\n\
   require \"lib.jinc\"\n\
   fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = a;\n\
  \  r += count;\n\
  \  return r;\n\
   }\n"

let cross_file_library = "u32 total = 1;\n"

let test_a_later_declaration_in_a_required_file_is_refused () =
  let file = "/work/cross-file.jazz" in
  let result =
    renamed
      ~files:[ ("lib.jinc", cross_file_library) ]
      ~file ~text:cross_file_document ~within:"u32 count = 0;" ~name:"count"
      ~new_name:"total" ()
  in
  check
    ("later declaration: a declaration in a required file, saying "
   ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"total already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/lib.jinc)" reason
    | Ok edits ->
        Printf.printf "later declaration: accepted, %d edit(s):\n%s\n%!"
          (edit_count edits)
          (apply cross_file_document edits);
        false)

(* A file that two requires bring in has one text and one meaning at each
   site: [sum.jinc] reads the parameter [N] of whichever namespace requires it.
   Renaming the [N] of one namespace would edit the file for the other as well,
   so the rename is refused with the reason, and so is prepareRename. *)
let shared_library =
  "/* Parameters:\n\
  \  param int N;\n\
   */\n\
   fn sum(reg u32[N] t) -> reg u32 {\n\
  \  reg u32 s;\n\
  \  inline int c;\n\
  \  s = 0;\n\
  \  for c = 0 to N {\n\
  \    s += t[c];\n\
  \  }\n\
  \  return s;\n\
   }\n"

let shared_document =
  "namespace Short {\n\
  \  param int N = 1;\n\
  \  require \"sum.jinc\"\n\
   }\n\
   namespace Long {\n\
  \  param int N = 8;\n\
  \  require \"sum.jinc\"\n\
   }\n"

let test_a_name_in_a_file_two_requires_bring_in_is_refused () =
  let file = "/work/shared.jazz" in
  let within = "param int N = 1;" in
  let files = [ ("sum.jinc", shared_library) ] in
  let result =
    renamed ~files ~file ~text:shared_document ~within ~name:"N" ~new_name:"M"
      ()
  in
  check
    ("shared: a parameter whose name the shared file uses is refused, saying \
      why: " ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"N in " reason
        && String.ends_with
             ~suffix:
               "means N when it is read into namespace Short and N when it is \
                read into namespace Long, so an edit of that file would change \
                both"
             reason
    | Ok _ -> false);
  let resolved = resolve ~files file shared_document in
  check "shared: prepareRename refuses the same name"
    (refused
       (Jide_engine.Rename.prepare ~index:(index_of resolved file)
          ~resolve:resolved
          ~byte:(byte shared_document ~within ~name:"N")
          ~broken:nothing_broken))

(* A global declared before two namespaces is one declaration, read by a file
   both namespaces require: each reading names the same global, so the file
   is edited once and the rename is correct. The name occurs in the shared
   file, which the rule that refused every such name would have refused. *)
let shared_global_document =
  "u32 g = 1;\n\n\
   namespace Short {\n\
  \  param int N = 1;\n\
  \  require \"uses.jinc\"\n\
   }\n\n\
   namespace Long {\n\
  \  param int N = 8;\n\
  \  require \"uses.jinc\"\n\
   }\n\n\
   export\n\
   fn test(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = g;\n\
  \  return r;\n\
   }\n"

let shared_global_library =
  "fn uses(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = x;\n\
  \  r += g;\n\
  \  return r;\n\
   }\n"

let test_a_global_read_by_a_shared_file_is_renamed_once () =
  let file = "/work/shared-global.jazz" in
  match
    renamed
      ~files:[ ("uses.jinc", shared_global_library) ]
      ~file ~text:shared_global_document ~within:"u32 g = 1;" ~name:"g"
      ~new_name:"h" ()
  with
  | Error reason ->
      check ("shared global: the rename is refused: " ^ reason) false
  | Ok edits ->
      let per_file =
        List.map
          (fun (path, group) -> (Filename.basename path, List.length group))
          edits
      in
      check
        (Printf.sprintf
           "shared global: the document holds two edits and the shared file \
            one, by file: %s"
           (String.concat ", "
              (List.map (fun (p, n) -> Printf.sprintf "%s %d" p n) per_file)))
        (List.sort compare per_file
         = [ ("shared-global.jazz", 2); ("uses.jinc", 1) ]
        || List.sort compare per_file
           = [ ("uses.jinc", 1); ("shared-global.jazz", 2) ]);
      check "shared global: the document's own text is renamed at both uses"
        (String.equal
           (apply shared_global_document
              (List.filter
                 (fun (path, _) ->
                   Filename.basename path = "shared-global.jazz")
                 edits))
           "u32 h = 1;\n\n\
            namespace Short {\n\
           \  param int N = 1;\n\
           \  require \"uses.jinc\"\n\
            }\n\n\
            namespace Long {\n\
           \  param int N = 8;\n\
           \  require \"uses.jinc\"\n\
            }\n\n\
            export\n\
            fn test(reg u32 a) -> reg u32 {\n\
           \  reg u32 r;\n\
           \  r = h;\n\
           \  return r;\n\
            }\n")

(* The document's global is renamed to a name a file it requires declares.
   Both declarations then sit in one frame. The compiler accepts that without a
   diagnostic, even where nothing uses the name, and a later use would bind to
   one of the two with nothing to say which. The rule keeps such a rename out.
   The file that holds the clashing declaration has no occurrence of the
   renamed symbol, so only the second end of the rule, asked of every file the
   resolution read, can see it. *)
let later_library_document =
  "u32 count = 0;\n\
   require \"lib.jinc\"\n\
   export\n\
   fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = a;\n\
  \  return r;\n\
   }\n"

let later_library = "u32 total = 1;\n"

let test_a_name_a_required_file_declares_is_refused () =
  let file = "/work/collide-document.jazz" in
  let result =
    renamed
      ~files:[ ("lib.jinc", later_library) ]
      ~file ~text:later_library_document ~within:"u32 count = 0;" ~name:"count"
      ~new_name:"total" ()
  in
  check
    ("required file: a rename to a name the required file declares is refused, \
      saying where: " ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"total already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/lib.jinc)" reason
    | Ok _ -> false)

(* The other direction: the library's global is renamed to a name the project
   file declares after the require. The library is the document, the project
   file reads the library, and the project file is searched only for the
   declaration it holds, which no occurrence of the renamed symbol reaches. *)
let later_document_text =
  "require \"lib.jinc\"\n\
   u32 total = 0;\n\
   export\n\
   fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = a;\n\
  \  r += total;\n\
  \  return r;\n\
   }\n"

let library_with_count = "u32 count = 1;\n"

let test_a_name_a_project_file_declares_after_the_require_is_refused () =
  let file = "/work/lib.jinc" in
  let result =
    renamed
      ~files:[ ("lib.jinc", library_with_count) ]
      ~project:[ ("/work/doc.jazz", later_document_text) ]
      ~file ~text:library_with_count ~within:"u32 count = 1;" ~name:"count"
      ~new_name:"total" ()
  in
  check
    ("project file: a rename to a name the project file declares after the \
      require is refused, saying where: " ^ refusal result)
    (match result with
    | Error reason ->
        String.starts_with ~prefix:"total already names a different declaration"
          reason
        && String.ends_with ~suffix:"/work/doc.jazz)" reason
    | Ok _ -> false)

(* Nothing to rename: a position on no name, a name with no declaration. Both
   reasons are asserted exactly, because prepareRename answers the same
   refusals as the rename, in the same words. *)
let test_positions_that_hold_no_name_are_refused () =
  let file = "/work/variables.jazz" in
  let at text ~within ~name =
    let resolved = resolve file text in
    Jide_engine.Rename.prepare ~index:(index_of resolved file) ~resolve:resolved
      ~byte:(byte text ~within ~name) ~broken:nothing_broken
  in
  check "no name: a byte inside the return keyword is refused, saying why"
    (match at variables ~within:"return count;" ~name:"return" with
    | Error reason ->
        String.equal reason "a keyword is not a name a rename can change"
    | Ok _ -> false);
  let commented =
    "fn f(reg u32 a) -> reg u32 {\n\
    \  // a word the index never sees\n\
    \  return a;\n\
     }\n"
  in
  check "no name: a byte inside a comment holds no name, saying why"
    (match
       at commented ~within:"// a word the index never sees" ~name:"index"
     with
    | Error reason -> String.equal reason "the position holds no name"
    | Ok _ -> false);
  check "no name: a name with no declaration says so"
    (match
       at "fn f(reg u32 a) -> reg u32 {\n  return nothing;\n}\n"
         ~within:"return nothing;" ~name:"nothing"
     with
    | Error reason ->
        String.equal reason "the name at the position has no declaration"
    | Ok _ -> false);
  check "no name: a name is prepared with its span and its kind"
    (match at variables ~within:"reg u32 count;" ~name:"count" with
    | Ok (span, kind) ->
        kind = Jide_engine.Index.Variable
        && span.Jide_lang.Span.start_byte
           = byte variables ~within:"reg u32 count;" ~name:"count"
        && span.Jide_lang.Span.end_byte
           = span.Jide_lang.Span.start_byte + String.length "count"
    | Error _ -> false)

(* A rename to the name the symbol already has changes nothing: every
   occurrence is replaced by itself. A later declaration of that name does not
   stand in its way either, because the renamed program declares nothing it
   did not declare before. *)
let test_a_rename_to_the_same_name_changes_nothing () =
  let file = "/work/variables.jazz" in
  (match
     renamed ~file ~text:variables ~within:"count = a;" ~name:"count"
       ~new_name:"count" ()
   with
  | Error reason -> check ("same name: refused: " ^ reason) false
  | Ok edits ->
      check "same name: the text is unchanged"
        (String.equal (apply variables edits) variables);
      check "same name: every occurrence is still an edit" (edit_count edits = 5));
  (* The same call on a program whose name is declared twice: the inner block
     declares [value] again, which is what the collision rule's second end
     looks for, and the outer declaration renamed to its own name must still
     be answered with the edits. *)
  let file = "/work/two-blocks.jazz" in
  match
    renamed ~file ~text:two_blocks ~within:"reg u32 value;\n  value = a;"
      ~name:"value" ~new_name:"value" ()
  with
  | Error reason ->
      check
        ("same name: refused where a block shadows the name: " ^ reason)
        false
  | Ok edits ->
      check "same name: the text is unchanged where a block shadows the name"
        (String.equal (apply two_blocks edits) two_blocks);
      check "same name: the outer occurrences are still edits"
        (edit_count edits = 3)

(* The gate's statement, measured case by case: the verdict is the compiler's
   own list, errors and warnings alike, and it is the same list before and
   after the rename. It is compared with the list the server publishes, so the
   test measures what a client sees rather than a filtered view of it. A case
   the rule refuses writes no text, so its verdict cannot change; it stays in
   the list so that a rule which stops refusing cannot pass this test. *)
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

(* One diagnostic of a verdict: how serious it is and what it says. The
   message is the compiler's own, never a summary, so two verdicts are equal
   only when they say the same things. *)
type verdict = (Jide_lang.Diagnostic.severity * string) list

let check_text ~file text =
  Jide_jasmin.Adapter.check x86_64 linux ~file ~text ~idirs:[]

(* The compiler's verdict of one check: every error it reports, then every
   warning printed for this file. Warnings are kept: the engine publishes
   them as diagnostics, so a warning is a change a client sees. *)
let compiler_verdict ~file (result : Jide_jasmin.Adapter.check_result) =
  let errors =
    List.map
      (fun (diagnostic : Jide_lang.Diagnostic.t) ->
        ( diagnostic.Jide_lang.Diagnostic.severity,
          diagnostic.Jide_lang.Diagnostic.message ))
      result.Jide_jasmin.Adapter.diagnostics
  in
  let warnings =
    List.filter_map
      (fun (warning : Jide_jasmin.Warnings.t) ->
        if String.equal warning.Jide_jasmin.Warnings.file file then
          Some
            (Jide_lang.Diagnostic.Warning, warning.Jide_jasmin.Warnings.message)
        else None)
      result.Jide_jasmin.Adapter.warnings
  in
  errors @ warnings

(* The server's verdict of the same check: what the engine publishes. *)
let server_verdict ~file ~text (result : Jide_jasmin.Adapter.check_result) =
  List.map
    (fun (diagnostic : Jide_lang.Diagnostic.t) ->
      ( diagnostic.Jide_lang.Diagnostic.severity,
        diagnostic.Jide_lang.Diagnostic.message ))
    (Jide_engine.Engine.diagnostics_of_check ~file ~text result)

let severity_rank = function
  | Jide_lang.Diagnostic.Error -> 0
  | Jide_lang.Diagnostic.Warning -> 1
  | Jide_lang.Diagnostic.Information -> 2
  | Jide_lang.Diagnostic.Hint -> 3

(* One order for both lists, so the comparison is element by element rather
   than by length: errors before warnings, then by message. *)
let canonical (verdict : verdict) =
  List.sort
    (fun (left_severity, left_message) (right_severity, right_message) ->
      match
        Int.compare (severity_rank left_severity) (severity_rank right_severity)
      with
      | 0 -> String.compare left_message right_message
      | order -> order)
    verdict

let show_verdict (verdict : verdict) =
  match verdict with
  | [] -> "[]"
  | entries ->
      String.concat "; "
        (List.map
           (fun (severity, message) ->
             Printf.sprintf "%s %S"
               (match severity with
               | Jide_lang.Diagnostic.Error -> "error"
               | Jide_lang.Diagnostic.Warning -> "warning"
               | Jide_lang.Diagnostic.Information -> "information"
               | Jide_lang.Diagnostic.Hint -> "hint")
               message)
           entries)

let test_the_renamed_program_keeps_its_verdict () =
  let file = "/work/verdict.jazz" in
  let accepted = `Accepted and refused = `Refused in
  let cases =
    [
      ("a clean name", verdict_program, "fn add", "add", "plus", accepted);
      ( "a name declared later in the same block",
        later_variable,
        "count = a;",
        "count",
        "total",
        refused );
      ( "a name declared later in a nested block",
        later_nested,
        "count = a;",
        "count",
        "total",
        refused );
      ( "a variable beside a variable in a namespace",
        namespace_variable_outside,
        "u32 count = 0;",
        "count",
        "total",
        accepted );
      ( "a namespace's variable beside a top-level variable",
        namespace_variable_inside,
        "u32 count = 0;",
        "count",
        "total",
        accepted );
      ( "a function under a namespace's own name",
        namespace_named_like_a_value,
        "fn g",
        "g",
        "A",
        accepted );
      ( "a variable under a namespace's own name",
        namespace_named_like_a_value_variable,
        "u32 g = 0;",
        "g",
        "A",
        accepted );
      ( "a qualified declaration's last segment",
        qualified_declaration,
        "::e ()",
        "e",
        "times",
        accepted );
    ]
  in
  List.iter
    (fun (label, text, within, name, new_name, expected) ->
      let before_check = check_text ~file text in
      let before = server_verdict ~file ~text before_check in
      (* A program that does not compile has the same verdict on both sides
         whatever the rule answers, so each case says first that this is a
         program the compiler accepts. *)
      check
        (Printf.sprintf
           "verdict: %s: the program compiles before the rename (%s)" label
           (show_verdict before))
        (List.for_all
           (fun (severity, _) -> severity <> Jide_lang.Diagnostic.Error)
           before);
      check
        (Printf.sprintf
           "verdict: %s: the server publishes the compiler's verdict before \
            the rename (got %s)"
           label (show_verdict before))
        (canonical before = canonical (compiler_verdict ~file before_check));
      match (expected, renamed ~file ~text ~within ~name ~new_name ()) with
      | `Refused, Error _ -> ()
      | `Refused, Ok edits ->
          (* The rule accepted a rename it refuses: the verdict either changed
             or it did not, and only the comparison can say. *)
          let renamed_text = apply text edits in
          let after_check = check_text ~file renamed_text in
          let after = server_verdict ~file ~text:renamed_text after_check in
          check
            (Printf.sprintf
               "verdict: %s: accepted, and the renamed program keeps the \
                verdict (before %s, after %s)"
               label (show_verdict before) (show_verdict after))
            (canonical before = canonical after)
      | `Accepted, Error reason ->
          check (Printf.sprintf "verdict: %s: refused: %s" label reason) false
      | `Accepted, Ok edits ->
          let renamed_text = apply text edits in
          let after_check = check_text ~file renamed_text in
          let after = server_verdict ~file ~text:renamed_text after_check in
          check
            (Printf.sprintf
               "verdict: %s: the server publishes the compiler's verdict after \
                the rename (got %s)"
               label (show_verdict after))
            (canonical after = canonical (compiler_verdict ~file after_check));
          check
            (Printf.sprintf
               "verdict: %s: the renamed program keeps the verdict (before %s, \
                after %s)"
               label (show_verdict before) (show_verdict after))
            (canonical before = canonical after))
    cases

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
  test_a_qualified_declaration_renames_its_last_segment ();
  test_a_namespace_name_is_refused ();
  test_a_namespace_prefix_is_refused ();
  test_a_spaced_qualified_name_is_refused ();
  test_an_invalid_identifier_is_refused ();
  test_a_collision_with_a_visible_declaration_is_refused ();
  test_a_later_declaration_in_the_same_block_is_refused ();
  test_a_later_function_in_the_same_file_is_refused ();
  test_a_later_declaration_in_a_nested_block_is_refused ();
  test_a_later_declaration_in_a_required_file_is_refused ();
  test_a_name_in_a_file_two_requires_bring_in_is_refused ();
  test_a_global_read_by_a_shared_file_is_renamed_once ();
  test_a_name_a_required_file_declares_is_refused ();
  test_a_name_a_project_file_declares_after_the_require_is_refused ();
  test_a_name_in_another_namespace_is_not_a_collision ();
  test_a_variable_in_another_namespace_is_not_a_collision ();
  test_a_function_in_another_namespace_is_a_collision ();
  test_a_namespace_name_is_not_a_value_collision ();
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
