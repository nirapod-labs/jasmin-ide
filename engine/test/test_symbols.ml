let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let index_of file text = Jide_engine.Index.of_text ~file text |> fst

let names (symbols : Jide_engine.Symbols.symbol list) =
  List.map
    (fun (symbol : Jide_engine.Symbols.symbol) ->
      symbol.Jide_engine.Symbols.name)
    symbols

let files_of (symbols : Jide_engine.Symbols.symbol list) =
  List.map
    (fun (symbol : Jide_engine.Symbols.symbol) ->
      symbol.Jide_engine.Symbols.file)
    symbols

let kinds (symbols : Jide_engine.Symbols.symbol list) =
  List.map
    (fun (symbol : Jide_engine.Symbols.symbol) ->
      symbol.Jide_engine.Symbols.kind)
    symbols

(* The text a symbol's name span covers, read back from the source: a wrong
   range shows as the wrong word. *)
let name_texts text (symbols : Jide_engine.Symbols.symbol list) =
  List.map
    (fun (symbol : Jide_engine.Symbols.symbol) ->
      let start =
        max 0 symbol.Jide_engine.Symbols.name_span.Jide_lang.Span.start_byte
      in
      let stop =
        max start
          (min symbol.Jide_engine.Symbols.name_span.Jide_lang.Span.end_byte
             (String.length text))
      in
      String.sub text start (stop - start))
    symbols

(* ------------------------------------------------------------------ *)

let first =
  "fn first_add(reg u32 a) -> reg u32 {\n\
  \  reg u32 part;\n\
  \  part = a;\n\
  \  return part;\n\
   }\n"

let second =
  "type added = u32;\n\
   fn second_add(reg u32 a) -> reg u32 { return a; }\n\
   u32 add_flag = 1;\n"

(* A query that matches declarations in both files. The query is a fragment,
   in another case, and every match carries the file it came from. *)
let test_a_query_matches_across_files () =
  let files =
    [ index_of "/work/first.jazz" first; index_of "/work/second.jazz" second ]
  in
  let found = Jide_engine.Symbols.search ~files ~query:"ADD" in
  check "across files: the four declarations holding add are found"
    (List.sort String.compare (names found)
    = [ "add_flag"; "added"; "first_add"; "second_add" ]);
  check "across files: each one keeps its own file"
    (List.sort String.compare (files_of found)
    = [
        "/work/first.jazz";
        "/work/second.jazz";
        "/work/second.jazz";
        "/work/second.jazz";
      ]);
  check "across files: each one keeps its kind"
    (List.sort String.compare
       (List.map
          (fun (kind : Jide_engine.Index.kind) ->
            match kind with
            | Function -> "function"
            | Global -> "global"
            | Type_alias -> "alias"
            | Parameter -> "parameter"
            | Variable -> "variable"
            | Namespace -> "namespace"
            | Exec -> "exec"
            | Require -> "require"
            | Intrinsic -> "intrinsic"
            | Annotation -> "annotation"
            | Include -> "include"
            | Unknown -> "unknown")
          (kinds found))
    = [ "alias"; "function"; "function"; "global" ]);
  check "across files: the order follows the files and then the source"
    (names found = [ "first_add"; "added"; "second_add"; "add_flag" ])

(* A name span read back from the source names the declaration's own word,
   which is what a client jumps to and shows in the palette. *)
let test_a_symbol_carries_the_declarations_own_spans () =
  let files = [ index_of "/work/first.jazz" first ] in
  let found = Jide_engine.Symbols.search ~files ~query:"first" in
  check "spans: one declaration"
    (List.length found = 1 && names found = [ "first_add" ]);
  check "spans: the name span is the declaration's own word"
    (name_texts first found = [ "first_add" ]);
  check "spans: the item span covers the whole declaration"
    (match found with
    | [ symbol ] ->
        let open Jide_engine.Symbols in
        (* The parser's item ends at the closing brace: the newline after it
           is not part of the declaration. *)
        symbol.span.Jide_lang.Span.start_byte = 0
        && symbol.span.Jide_lang.Span.end_byte = String.length first - 1
    | _ -> false);
  check "spans: a function carries its signature as its detail"
    (match found with
    | [ symbol ] -> symbol.Jide_engine.Symbols.detail <> None
    | _ -> false)

(* The empty query lists everything, which is what a client shows when the
   palette opens before the user types. *)
let test_an_empty_query_lists_all () =
  let files =
    [ index_of "/work/first.jazz" first; index_of "/work/second.jazz" second ]
  in
  let all = Jide_engine.Symbols.search ~files ~query:"" in
  check "empty query: every declaration of both files"
    (names all
    = [ "first_add"; "a"; "part"; "added"; "second_add"; "a"; "add_flag" ]);
  check "empty query: nothing is left out"
    (List.length all
    = List.fold_left
        (fun total index ->
          total + List.length index.Jide_engine.Index.declarations)
        0 files)

(* A query nothing matches lists nothing, and a query that matches only a
   parameter still finds it: a parameter is a declaration and a client
   navigates to it. *)
let test_a_query_with_no_match_and_one_on_a_parameter () =
  let files = [ index_of "/work/first.jazz" first ] in
  check "no match: an absent name finds nothing"
    (Jide_engine.Symbols.search ~files ~query:"nothing_like_this" = []);
  check "parameter: the variable is found by its own name"
    (names (Jide_engine.Symbols.search ~files ~query:"part") = [ "part" ])

(* A file that does not parse has no declarations, and a project with several
   files answers per file, in the order given. *)
let test_a_broken_file_contributes_nothing () =
  let files =
    [ index_of "/work/first.jazz" first; index_of "/work/broken.jazz" "fn {" ]
  in
  check "broken: the file that parses still answers"
    (names (Jide_engine.Symbols.search ~files ~query:"")
    = [ "first_add"; "a"; "part" ]);
  check "broken: an empty project answers nothing"
    (Jide_engine.Symbols.search ~files:[] ~query:"" = [])

let () =
  test_a_query_matches_across_files ();
  test_a_symbol_carries_the_declarations_own_spans ();
  test_an_empty_query_lists_all ();
  test_a_query_with_no_match_and_one_on_a_parameter ();
  test_a_broken_file_contributes_nothing ();
  if !failures = 0 then print_endline "all symbols tests passed"
  else (
    Printf.printf "%d symbols test(s) failed\n" !failures;
    exit 1)
