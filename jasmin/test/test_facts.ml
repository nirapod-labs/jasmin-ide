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

(* A contract annotation comes in with [ct] and a security annotation with
   [sct]; both are strings and the adapter reads them the same way. *)
let test_security_annotation () =
  with_item "sub_" (fun item ->
      match item.Facts.i_annot with
      | [ annotation ] ->
          check "the security annotation's key"
            (String.equal annotation.Facts.a_key "sct");
          check "the security annotation's string"
            (annotation.Facts.a_value = Some "secret -> public");
          same_span "the security annotation's range" annotation.Facts.a_span
            (span_of "\"secret -> public\"")
      | other ->
          check
            ("one security annotation, got " ^ string_of_int (List.length other))
            false)

(* The item after the first one has to find its own declarations. A
   parameter, a global and an exec are the other three forms a top level item
   takes, and an exec is the one that declares nothing at all, so it is also
   the item whose typing leaves the list of declarations untouched. *)

let with_param =
  {|
fn one(reg u64 a) -> reg u64 {
  reg u64 b;
  b = a;
  return b;
}

param int rounds = 10;
|}

let with_global =
  {|
fn one(reg u64 a) -> reg u64 {
  reg u64 b;
  b = a;
  return b;
}

u64 counter = 0;
|}

(* The compiler's grammar takes an [exec] without a trailing semicolon: with
   one the parser reports an "unexpected token" and the whole file yields
   nothing. *)
let with_exec =
  {|
fn one(reg u64 a) -> reg u64 {
  reg u64 b;
  b = a;
  return b;
}

exec one(0:32)

fn two(reg u64 a) -> reg u64 {
  reg u64 b;
  b = a;
  return b;
}
|}

let test_second_item_is_a_param () =
  let facts = facts_of "a parameter second" with_param in
  check
    (Printf.sprintf "a parameter second: two items, got %d"
       (List.length facts.Facts.items))
    (List.length facts.Facts.items = 2);
  with_item_in facts "one" (fun item ->
      check "a parameter second: the first item is a function"
        (item.Facts.i_kind = Facts.Function));
  with_item_in facts "rounds" (fun item ->
      check "a parameter second: the item is a parameter"
        (item.Facts.i_kind = Facts.Param);
      same_span "a parameter second: its name is its name"
        item.Facts.i_name_span
        (span_in with_param "rounds");
      check "a parameter second: it has no signature" (item.Facts.i_sig = None);
      match item.Facts.i_vars with
      | [ variable ] ->
          check "a parameter second: one variable named rounds"
            (String.equal variable.Facts.v_name "rounds");
          check "a parameter second: its type"
            (String.equal variable.Facts.v_ty "int");
          check "a parameter second: its kind"
            (variable.Facts.v_kind
            = { Facts.storage = Facts.Param; pointer = Facts.Direct });
          check "a parameter second: its file"
            (String.equal variable.Facts.v_dfile file);
          same_span "a parameter second: its declaration is its name"
            variable.Facts.v_dloc
            (span_in with_param "rounds")
      | other ->
          check
            ("a parameter second: one variable, got "
            ^ string_of_int (List.length other))
            false)

let test_second_item_is_a_global () =
  let facts = facts_of "a global second" with_global in
  check
    (Printf.sprintf "a global second: two items, got %d"
       (List.length facts.Facts.items))
    (List.length facts.Facts.items = 2);
  with_item_in facts "one" (fun item ->
      check "a global second: the first item is a function"
        (item.Facts.i_kind = Facts.Function));
  with_item_in facts "counter" (fun item ->
      check "a global second: the item is a global"
        (item.Facts.i_kind = Facts.Global);
      same_span "a global second: its name is its name" item.Facts.i_name_span
        (span_in with_global "counter");
      check "a global second: it has no signature" (item.Facts.i_sig = None);
      match item.Facts.i_vars with
      | [ variable ] ->
          check "a global second: one variable named counter"
            (String.equal variable.Facts.v_name "counter");
          check "a global second: its type"
            (String.equal variable.Facts.v_ty "u64");
          check "a global second: its kind"
            (variable.Facts.v_kind
            = { Facts.storage = Facts.Global; pointer = Facts.Direct });
          check "a global second: its file"
            (String.equal variable.Facts.v_dfile file);
          same_span "a global second: its declaration is its name"
            variable.Facts.v_dloc
            (span_in with_global "counter")
      | other ->
          check
            ("a global second: one variable, got "
            ^ string_of_int (List.length other))
            false)

let test_second_item_is_an_exec () =
  let facts = facts_of "an exec second" with_exec in
  check
    (Printf.sprintf "an exec second: two items, got %d"
       (List.length facts.Facts.items))
    (List.length facts.Facts.items = 2);
  check "an exec second: the items are the two functions"
    (List.map (fun (item : Facts.item) -> item.Facts.i_name) facts.Facts.items
    = [ "one"; "two" ]);
  with_item_in facts "one" (fun item ->
      check "an exec second: the first item is a function"
        (item.Facts.i_kind = Facts.Function));
  with_item_in facts "two" (fun item ->
      check "an exec second: the item after the exec is a function"
        (item.Facts.i_kind = Facts.Function);
      check "an exec second: the item after the exec has its variables"
        (List.map (fun (v : Facts.var) -> v.Facts.v_name) item.Facts.i_vars
        = [ "a"; "b" ]))

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
  test_second_item ();
  test_signature ();
  test_variables ();
  test_annotations ();
  test_kind_strings ();
  if !failures = 0 then print_endline "all facts tests passed"
  else (
    Printf.printf "%d facts test(s) failed\n" !failures;
    exit 1)
