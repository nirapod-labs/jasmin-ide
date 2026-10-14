let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* The tests say where a byte is by quoting the source around it, never by a
   number written by hand. *)
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

let byte text ~within ~name =
  match byte_of_in text ~within ~name with
  | Some byte -> byte
  | None ->
      incr failures;
      Printf.printf "FAIL test bug: %s is not inside %s\n%!" name within;
      0

let resolve file text =
  Jide_engine.Resolve.of_text ~idirs:[] ~read:(fun _ -> None) ~file text

let index_of (resolved : Jide_engine.Resolve.t) path =
  match Jide_engine.Resolve.file resolved path with
  | Some index -> index
  | None -> Jide_engine.Index.of_text ~file:path "" |> fst

let highlights ~file ~text ~within ~name =
  let resolved = resolve file text in
  Jide_engine.Highlight.at ~index:(index_of resolved file) ~resolve:resolved
    ~byte:(byte text ~within ~name)

(* What each highlight covers, as the text it spans. A span the tests read
   back from the source is the strongest statement: a wrong range shows as the
   wrong word. *)
let words text highlights =
  List.map
    (fun (highlight : Jide_engine.Highlight.highlight) ->
      let start =
        max 0 highlight.Jide_engine.Highlight.span.Jide_lang.Span.start_byte
      in
      let stop =
        max start
          (min highlight.Jide_engine.Highlight.span.Jide_lang.Span.end_byte
             (String.length text))
      in
      String.sub text start (stop - start))
    highlights

let kinds highlights =
  List.map
    (fun (highlight : Jide_engine.Highlight.highlight) ->
      highlight.Jide_engine.Highlight.kind)
    highlights

(* ------------------------------------------------------------------ *)

let variables =
  "fn f(reg u32 a) -> reg u32 {\n\
  \  reg u32 count;\n\
  \  count = a;\n\
  \  count = count + a;\n\
  \  return count;\n\
   }\n"

(* Every occurrence of the one identity, in source order, and nothing else. A
   name that occurs five times in the file is five highlights; the parameter
   [a], which is a different identity, is not among them. *)
let test_only_the_same_identity () =
  let file = "/work/variables.jazz" in
  let highlights =
    highlights ~file ~text:variables ~within:"count = a;" ~name:"count"
  in
  check "identity: five occurrences are highlighted" (List.length highlights = 5);
  check "identity: every one of them is the word count"
    (List.for_all (String.equal "count") (words variables highlights));
  check "identity: the parameter is not among them"
    (not (List.exists (String.equal "a") (words variables highlights)));
  check "identity: they are in source order"
    (let starts =
       List.map
         (fun (highlight : Jide_engine.Highlight.highlight) ->
           highlight.Jide_engine.Highlight.span.Jide_lang.Span.start_byte)
         highlights
     in
     List.sort compare starts = starts)

(* The index records no read/write distinction for a variable occurrence, so
   every highlight is [Text] and the interface says so. This test pins that
   answer: if [Index] ever records the role, this test fails and the module
   has to change with it. *)
let test_reads_and_writes_are_text_today () =
  let file = "/work/variables.jazz" in
  let highlights =
    highlights ~file ~text:variables ~within:"count = a;" ~name:"count"
  in
  check "kinds: every highlight is Text, because the index records no role"
    (List.for_all
       (fun (kind : Jide_engine.Highlight.kind) ->
         kind = Jide_engine.Highlight.Text)
       (kinds highlights));
  (* A write and a read are both in the set, which is what the client lights
     up either way. *)
  check "kinds: the declaration, the write and the reads are all present"
    (List.length highlights = 5)

(* Two same-named declarations in different blocks are two symbols: only the
   one the byte is on is lit. *)
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

let test_a_shadowing_declaration_is_a_different_symbol () =
  let file = "/work/two-blocks.jazz" in
  let outer =
    highlights ~file ~text:two_blocks ~within:"reg u32 value;\n  value = a;"
      ~name:"value"
  in
  check "shadowing: the outer declaration has three occurrences"
    (List.length outer = 3);
  check "shadowing: they are the outer three"
    (let starts =
       List.map
         (fun (highlight : Jide_engine.Highlight.highlight) ->
           highlight.Jide_engine.Highlight.span.Jide_lang.Span.start_byte)
         outer
     in
     let inner_declaration =
       byte two_blocks ~within:"if (a < 1) {\n    reg u32 value;" ~name:"value"
     in
     not (List.mem inner_declaration starts));
  let inner =
    highlights ~file ~text:two_blocks
      ~within:"reg u32 value;\n    value = a;\n    a = value;" ~name:"value"
  in
  check "shadowing: the inner declaration has three occurrences"
    (List.length inner = 3);
  check "shadowing: the two sets do not overlap"
    (let starts_of highlights =
       List.map
         (fun (highlight : Jide_engine.Highlight.highlight) ->
           highlight.Jide_engine.Highlight.span.Jide_lang.Span.start_byte)
         highlights
     in
     List.for_all
       (fun start -> not (List.mem start (starts_of inner)))
       (starts_of outer))

(* A function's occurrences, which is what a client asks for when the cursor
   is on a call. *)
let calls =
  "fn add(reg u32 x) -> reg u32 { return x; }\n\
   fn use(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = add(x);\n\
  \  return r;\n\
   }\n"

let test_a_function_highlights_its_declaration_and_call () =
  let file = "/work/calls.jazz" in
  let highlights =
    highlights ~file ~text:calls ~within:"r = add(x);" ~name:"add"
  in
  check "function: the declaration and the call are highlighted"
    (words calls highlights = [ "add"; "add" ])

(* A qualified name carries one range for the whole name, so a highlight is
   the last segment, which is the part a client can paint. *)
let namespaced =
  "namespace A {\n\
  \  fn mul(reg u32 x) -> reg u32 { return x; }\n\
   }\n\
   fn use(reg u32 x) -> reg u32 {\n\
  \  reg u32 r;\n\
  \  r = A::mul(x);\n\
  \  return r;\n\
   }\n"

let test_a_qualified_name_highlights_its_last_segment () =
  let file = "/work/namespaced.jazz" in
  let highlights =
    highlights ~file ~text:namespaced ~within:"r = A::mul(x);" ~name:"mul"
  in
  check "qualified: the declaration and the qualified use are highlighted"
    (words namespaced highlights = [ "mul"; "mul" ]);
  check "qualified: the namespace prefix is not part of the highlight"
    (not (List.exists (String.equal "A::mul") (words namespaced highlights)))

(* A name with no declaration has no identity to light, and neither does a
   keyword or a position holding no name. *)
let test_nothing_to_highlight () =
  let file = "/work/variables.jazz" in
  let unresolved = "fn f(reg u32 a) -> reg u32 {\n  return nothing;\n}\n" in
  check "nothing: an unresolved name has no highlight"
    (highlights ~file ~text:unresolved ~within:"return nothing;" ~name:"nothing"
    = []);
  check "nothing: a keyword has no highlight"
    (highlights ~file ~text:variables ~within:"return count;" ~name:"return"
    = []);
  check "nothing: the namespace part of a qualified name has no highlight"
    (highlights ~file:"/work/namespaced.jazz" ~text:namespaced
       ~within:"r = A::mul(x);" ~name:"A"
    = [])

let () =
  test_only_the_same_identity ();
  test_reads_and_writes_are_text_today ();
  test_a_shadowing_declaration_is_a_different_symbol ();
  test_a_function_highlights_its_declaration_and_call ();
  test_a_qualified_name_highlights_its_last_segment ();
  test_nothing_to_highlight ();
  if !failures = 0 then print_endline "all highlight tests passed"
  else (
    Printf.printf "%d highlight test(s) failed\n" !failures;
    exit 1)
