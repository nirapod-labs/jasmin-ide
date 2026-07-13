(* The typed facts, checked against programs small enough to read: every
   range the facts report is written out here by the text it covers. *)

module Facts = Jide_jasmin.Facts

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let file = "/tmp/hand-checked.jazz"

(* Two functions. The second one is the point: typing pushes an item's
   declarations in front of the environment it found, so every item after
   the first must be read from the front of the list while the first item
   alone cannot show a reader that goes to the wrong end. *)
let program =
  {|
#[ct="public -> public"]
fn add(reg u64 a, reg u64 b) -> reg u64 {
  reg u64 c;
  c = a + b;
  return c;
}

#[sct="secret -> public"]
fn sub_(reg u64 x, reg u64 y) -> reg u64 {
  reg u64 d;
  d = x - y;
  return d;
}
|}

let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let result =
  Jide_jasmin.Adapter.check x86_64 linux ~file ~text:program ~idirs:[]

let facts = result.Jide_jasmin.Adapter.facts

(* The facts of [text], with the promise that it types cleanly: a diagnostic
   would shorten the run of items the assertions below read. *)
let facts_of name text =
  let result = Jide_jasmin.Adapter.check x86_64 linux ~file ~text ~idirs:[] in
  check
    (name ^ ": the program has no diagnostics")
    (result.Jide_jasmin.Adapter.diagnostics = []);
  List.iter
    (fun (diagnostic : Jide_lang.Diagnostic.t) ->
      Printf.printf "  %s: %s\n%!" name diagnostic.message)
    result.Jide_jasmin.Adapter.diagnostics;
  result.Jide_jasmin.Adapter.facts

(* The byte where [needle] first appears in [text]. *)
let index_of text needle =
  let length = String.length text and width = String.length needle in
  let rec loop index =
    if index + width > length then None
    else if String.equal (String.sub text index width) needle then Some index
    else loop (index + 1)
  in
  loop 0

let offset_in text needle =
  match index_of text needle with
  | Some index -> index
  | None ->
      check ("the program contains " ^ needle) false;
      0

(* The range of [needle] in [text], or of the [length] bytes at [offset]
   inside it. *)
let span_in text ?(offset = 0) ?length needle =
  let start_byte = offset_in text needle + offset in
  let width =
    match length with
    | Some width -> width
    | None -> String.length needle - offset
  in
  Jide_lang.Span.make ~start_byte ~end_byte:(start_byte + width)

let offset_of = offset_in program
let span_of = span_in program

let same_span name (actual : Jide_lang.Span.t) (expected : Jide_lang.Span.t) =
  check
    (Printf.sprintf "%s: got %d..%d, wanted %d..%d" name actual.start_byte
       actual.end_byte expected.start_byte expected.end_byte)
    (Jide_lang.Span.compare actual expected = 0)

let with_item_in (facts : Facts.t) name f =
  match
    List.find_opt
      (fun (item : Facts.item) -> String.equal item.Facts.i_name name)
      facts.Facts.items
  with
  | Some item -> f item
  | None -> check ("the program has an item named " ^ name) false

let with_item = with_item_in facts

let test_diagnostics () =
  check "the program has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = [])

let test_items () =
  check
    (Printf.sprintf "the program has two items, got %d"
       (List.length facts.Facts.items))
    (List.length facts.Facts.items = 2);
  check "the items are in source order"
    (List.map (fun (item : Facts.item) -> item.Facts.i_name) facts.Facts.items
    = [ "add"; "sub_" ]);
  with_item "add" (fun item ->
      check "the item is a function" (item.Facts.i_kind = Facts.Function);
      same_span "the function's name is its name after fn"
        item.Facts.i_name_span
        (span_of "fn add(" ~offset:3 ~length:3);
      same_span "the function's range runs from its annotation to its brace"
        item.Facts.i_span
        (Jide_lang.Span.make ~start_byte:(offset_of "#[ct=")
           ~end_byte:(offset_of "return c;\n}" + String.length "return c;\n}")))

let test_second_item () =
  with_item "sub_" (fun item ->
      check "the second item is a function" (item.Facts.i_kind = Facts.Function);
      same_span "the second function's name is its name after fn"
        item.Facts.i_name_span
        (span_of "fn sub_(" ~offset:3 ~length:4);
      same_span
        "the second function's range runs from its annotation to its brace"
        item.Facts.i_span
        (Jide_lang.Span.make ~start_byte:(offset_of "#[sct=")
           ~end_byte:(offset_of "return d;\n}" + String.length "return d;\n}"));
      (match item.Facts.i_sig with
      | Some signature ->
          check "the second signature has two arguments"
            (signature.Facts.fs_in = [ "u64"; "u64" ]);
          check "the second signature returns one value"
            (signature.Facts.fs_out = [ "u64" ])
      | None -> check "the second function has a signature" false);
      let names =
        List.map (fun (v : Facts.var) -> v.Facts.v_name) item.Facts.i_vars
      in
      check
        ("the second function's variables are its parameters and its local, \
          got " ^ String.concat ", " names)
        (names = [ "x"; "y"; "d" ]);
      match item.Facts.i_vars with
      | [ x; y; d ] ->
          same_span "x is declared where its parameter names it" x.Facts.v_dloc
            (span_of "reg u64 x," ~offset:8 ~length:1);
          same_span "y is declared where its parameter names it" y.Facts.v_dloc
            (span_of "reg u64 y)" ~offset:8 ~length:1);
          same_span "d is declared where its declaration names it"
            d.Facts.v_dloc
            (span_of "reg u64 d;" ~offset:8 ~length:1)
      | _ -> ())

let test_signature () =
  with_item "add" (fun item ->
      match item.Facts.i_sig with
      | Some signature ->
          check "the signature has two arguments"
            (signature.Facts.fs_in = [ "u64"; "u64" ]);
          check "the signature returns one value"
            (signature.Facts.fs_out = [ "u64" ])
      | None -> check "the function has a signature" false)

let test_variables () =
  with_item "add" (fun item ->
      let names =
        List.map (fun (v : Facts.var) -> v.Facts.v_name) item.Facts.i_vars
      in
      check
        ("the variables are the parameters and the local, got "
       ^ String.concat ", " names)
        (names = [ "a"; "b"; "c" ]);
      List.iter
        (fun (v : Facts.var) ->
          check
            ("the kind of " ^ v.Facts.v_name)
            (v.Facts.v_kind
            = { Facts.storage = Facts.Reg; pointer = Facts.Direct });
          check
            ("the type of " ^ v.Facts.v_name)
            (String.equal v.Facts.v_ty "u64");
          check
            ("the file of " ^ v.Facts.v_name)
            (String.equal v.Facts.v_dfile file);
          check ("no annotation on " ^ v.Facts.v_name) (v.Facts.v_annot = []))
        item.Facts.i_vars;
      match item.Facts.i_vars with
      | [ a; b; c ] ->
          same_span "a is declared where its parameter names it" a.Facts.v_dloc
            (span_of "reg u64 a," ~offset:8 ~length:1);
          same_span "b is declared where its parameter names it" b.Facts.v_dloc
            (span_of "reg u64 b)" ~offset:8 ~length:1);
          same_span "c is declared where its declaration names it"
            c.Facts.v_dloc
            (span_of "reg u64 c;" ~offset:8 ~length:1)
      | _ -> ())

let test_annotations () =
  with_item "add" (fun item ->
      match item.Facts.i_annot with
      | [ annotation ] ->
          check "the annotation's key"
            (String.equal annotation.Facts.a_key "ct");
          check "the annotation's string"
            (annotation.Facts.a_value = Some "public -> public");
          same_span "the annotation's range" annotation.Facts.a_span
            (span_of "\"public -> public\"")
      | other ->
          check
            ("one annotation, got " ^ string_of_int (List.length other))
            false)

let test_kind_strings () =
  let kind storage pointer =
    Facts.string_of_var_kind { Facts.storage; pointer }
  in
  check "a register" (String.equal (kind Facts.Reg Facts.Direct) "reg");
  check "a writable register pointer"
    (String.equal (kind Facts.Reg Facts.Pointer_writable) "reg mut ptr");
  check "a constant stack pointer"
    (String.equal (kind Facts.Stack Facts.Pointer_constant) "stack const ptr");
  check "a parameter" (String.equal (kind Facts.Param Facts.Direct) "param");
  check "a global" (String.equal (kind Facts.Global Facts.Direct) "global");
  check "an inline value"
    (String.equal (kind Facts.Inline Facts.Direct) "inline")

let () =
  test_diagnostics ();
  test_items ();
  test_signature ();
  test_variables ();
  test_annotations ();
  test_kind_strings ();
  if !failures = 0 then print_endline "all facts tests passed"
  else (
    Printf.printf "%d facts test(s) failed\n" !failures;
    exit 1)
