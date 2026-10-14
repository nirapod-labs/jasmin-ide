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
