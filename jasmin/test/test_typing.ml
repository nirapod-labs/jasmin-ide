(* Tests for the typing loop over the tree a recovering parse returns.

   The style is the one the adapter tests use: a list of named checks, each
   one a predicate over a value, and an exit status that is non-zero when any
   of them fails. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let contains haystack needle =
  let n = String.length haystack and m = String.length needle in
  let rec loop index =
    if m = 0 then true
    else if index + m > n then false
    else String.sub haystack index m = needle || loop (index + 1)
  in
  loop 0

let file = "probe.jazz"
let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let check_text text =
  Jide_jasmin.Adapter.check x86_64 linux ~file ~text ~idirs:[]

module X86 =
  (val Jide_jasmin.Arch.module_of Jide_jasmin.Arch.X86_64 Jide_jasmin.Arch.Linux
      : Jasmin.Arch_full.Arch)

module Loop = Jide_jasmin.Check.Make (X86)

let codes (result : Jide_jasmin.Adapter.check_result) =
  List.map
    (fun (diagnostic : Jide_lang.Diagnostic.t) -> diagnostic.code)
    result.diagnostics

let mentions (result : Jide_jasmin.Adapter.check_result) needle =
  List.exists
    (fun (diagnostic : Jide_lang.Diagnostic.t) ->
      contains
        (String.lowercase_ascii diagnostic.message)
        (String.lowercase_ascii needle))
    result.diagnostics

(* A syntax error in the first item; the second one is whole and typable. *)
let broken_first_valid_second =
  {|fn first(reg u32 a -> reg u32 {
  return a;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 x;
  x = a;
  return x;
}
|}

(* The same, with a type error of its own in the item that survives. *)
let broken_first_typed_second =
  {|fn first(reg u32 a -> reg u32 {
  return a;
}

fn second(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}
|}

(* An item that fails to type, and a later item that calls it. *)
let failed_then_called =
  {|fn first(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 x;
  x = first(a);
  return x;
}
|}

(* An item recovery drops, and a later item that calls it. *)
let dropped_then_called =
  {|fn first(reg u32 a -> reg u32 {
  return a;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 x;
  x = first(a);
  return x;
}
|}

let with_type_error =
  {|fn f(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}
|}

(* The typing loop runs over the recovered tree, not only over a file that
   parsed cleanly: the item after the broken one is typed, and an error in it
   is reported beside the compiler's parse error. *)
let test_typing_over_recovered () =
  let result = check_text broken_first_typed_second in
  check "recovered: the parse error is kept"
    (List.mem (Some "parse") (codes result));
  (* The compiler's error, the stub of the item it could not keep, and the
     type error of the item that survived. *)
  check "recovered: the compiler's error, the stub, and the typing error"
    (List.length result.diagnostics = 3);
  check "recovered: the second one is a typing diagnostic"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.code = Some "typing")
       result.diagnostics);
  check "recovered: the valid second item still types"
    (let valid = check_text broken_first_valid_second in
     List.for_all
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.code = Some "parse")
       valid.diagnostics)

(* A failed item leaves the environment unchanged, so the items after it type;
   the references to its name are the same failure and are not repeated. *)
let test_failed_item_does_not_cascade () =
  let result = check_text failed_then_called in
  check "failed: the item's own error is reported"
    (List.length result.diagnostics >= 1);
  check "failed: its name is not reported as an unknown function"
    (not (mentions result "unknown function"));
  check "failed: one diagnostic, not two" (List.length result.diagnostics = 1)

(* A dropped item is not in the environment at all; a call to it would be an
   unknown function if the stub's name were not known to the loop. *)
let test_dropped_item_does_not_cascade () =
  let result = check_text dropped_then_called in
  (* The compiler's error and the stub of the item it dropped: two parse
     diagnostics, and nothing invented about the call to it. *)
  check "dropped: the compiler's error and the stub, and nothing else"
    (List.length result.diagnostics = 2);
  check "dropped: no unknown function is reported"
    (not (mentions result "unknown function"));
  check "dropped: the diagnostic is the parse one"
    (List.for_all
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.code = Some "parse")
       result.diagnostics)

(* A statement the parser choked on was blanked by recovery, and the
   declaration on that line is gone from the tree. The use three bytes after
   the rewritten bytes is the rewrite's consequence, not a second error: the
   suppression must cover the whole item, not only the spans that happen to
   overlap the region. *)
let recovered_statement_boundary =
  {|fn a(reg u32 x) -> reg u32 {
  reg u32 y;
  y = x
  reg bool c;
  c = x;
  return y;
}
|}

(* The same kind of damage in one item, and a genuine type error in the next
   one: that one is the program's, and the rule must not hide it. *)
let genuine_after_a_recovered_item =
  {|fn a(reg u32 x) -> reg u32 {
  reg u32 y;
  y = x
  return x;
}

fn b(reg u32 x) -> reg u32 {
  reg bool c;
  c = x;
  return x;
}
|}

(* A diagnostic whose span lies in a region recovery rewrote or skipped is a
   consequence of the broken text, not of the program: the loop drops it.
   The rule is tested directly, because the tree handed to the loop can come
   from anywhere. *)
let test_recovered_region_suppresses () =
  let parsed = Jide_jasmin.Parse.parse ~name:file with_type_error in
  check "region: the fixture parses" (not (Jide_jasmin.Parse.failed parsed));
  let plain, _ =
    Loop.type_items
      ~resolve:(fun _ -> None)
      ~budget:200 ~dropped:[] ~recovered:[] ~context:[] ~file
      ~text:with_type_error ~items:parsed.items ~idirs:[] ~preprocess:false
  in
  check "region: the error is reported without a recovered region"
    (List.length plain.Jide_jasmin.Check.diagnostics = 1);
  match plain.diagnostics with
  | [ diagnostic ] ->
      let suppressed, _ =
        Loop.type_items
          ~resolve:(fun _ -> None)
          ~budget:200 ~dropped:[]
          ~recovered:[ diagnostic.Jide_lang.Diagnostic.span ]
          ~context:[] ~file ~text:with_type_error ~items:parsed.items ~idirs:[]
          ~preprocess:false
      in
      check "region: the error inside the recovered region is dropped"
        (suppressed.Jide_jasmin.Check.diagnostics = [])
  | other ->
      check
        ("region: one diagnostic, got " ^ string_of_int (List.length other))
        false

(* A namespace is typed as one item, so one bad item inside it takes every
   name the namespace declared with it. The call to the healthy item is then
   an unknown function only because the namespace failed, and the compiler
   prints the name qualified, so the suppression has to know the path. *)
let namespace_with_a_bad_item =
  {|namespace N {
  fn good(reg u32 a) -> reg u32 { reg bool b; b = a; return a; }
}

fn use(reg u32 a) -> reg u32 { reg u32 x; x = N::good(a); return x; }
|}

(* The same namespace, dropped whole by recovery: the stub carries the
   namespace's name and nothing inside it, so the path is all the loop can
   key on. *)
let namespace_dropped_by_recovery =
  {|namespace N {
  fn good(reg u32 a -> reg u32 { return a; }
}

fn use(reg u32 a) -> reg u32 { reg u32 x; x = N::good(a); return x; }
|}

let test_namespace_does_not_cascade () =
  let result = check_text namespace_with_a_bad_item in
  check "namespace: the bad item's own error is the only one"
    (List.length result.diagnostics = 1
    && List.mem (Some "typing") (codes result));
  check "namespace: the healthy item is not reported as unknown"
    (not (mentions result "unknown function"));
  let dropped = check_text namespace_dropped_by_recovery in
  check "namespace dropped: the compiler's error and the stub, and nothing else"
    (List.length dropped.diagnostics = 2
    && List.for_all (fun code -> code = Some "parse") (codes dropped));
  check "namespace dropped: no unknown function for a name inside it"
    (not (mentions dropped "unknown function"))

(* Suppression is for names this file used to declare, and only after the
   item that declares them: a name that was never declared, or a call that
   comes before the failing declaration, is the program's own error. *)
let test_genuine_unknown_names_are_reported () =
  let undeclared =
    check_text
      {|fn f(reg u32 a) -> reg u32 {
  reg u32 x;
  x = nosuch(a);
  return x;
}
|}
  in
  check "unknown: a name that is nowhere is reported"
    (mentions undeclared "unknown function");
  check "unknown: and it is the only diagnostic"
    (List.length undeclared.diagnostics = 1);
  let before =
    check_text
      {|fn use(reg u32 a) -> reg u32 {
  reg u32 x;
  x = later(a);
  return x;
}

fn later(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}
|}
  in
  check "order: a call before the failing declaration is reported"
    (mentions before "unknown function");
  check "order: and the failing declaration's own error too"
    (List.length before.diagnostics = 2)

(* Every typing diagnostic of an item that contains recovered syntax is a
   consequence of the broken text and is dropped with it, wherever in the
   item its span falls. An error in a different, whole item is still the
   program's and is reported. *)
let test_recovered_item_is_quiet () =
  let result = check_text recovered_statement_boundary in
  check "recovered item: the parse error is the only diagnostic"
    (List.length result.diagnostics = 1
    && List.mem (Some "parse") (codes result));
  check "recovered item: no typing diagnostic from the rewritten item"
    (not (mentions result "unknown variable"));
  let control = check_text genuine_after_a_recovered_item in
  check "recovered item: a genuine error in the next item is reported"
    (List.length control.diagnostics = 2
    && List.exists
         (fun (diagnostic : Jide_lang.Diagnostic.t) ->
           diagnostic.code = Some "typing")
         control.diagnostics)

(* At most 200 diagnostics, however many items fail. *)
let test_diagnostic_bound () =
  let buffer = Buffer.create 65536 in
  for index = 0 to 249 do
    Buffer.add_string buffer
      (Printf.sprintf
         "fn f%d(reg u32 a) -> reg u32 {\n\
         \  reg bool b;\n\
         \  b = a;\n\
         \  return a;\n\
          }\n\n"
         index)
  done;
  let result = check_text (Buffer.contents buffer) in
  check "bound: exactly two hundred diagnostics"
    (List.length result.diagnostics = 200);
  check "bound: every one is a typing diagnostic"
    (List.for_all
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.code = Some "typing")
       result.diagnostics)

(* The bound is one number for the whole check. The loop's budget is what is
   left after the parse diagnostics, so a parse error and a stub are not
   silently traded against typing errors by a second truncation at the end. *)
let test_diagnostic_bound_with_parse_errors () =
  let buffer = Buffer.create 65536 in
  Buffer.add_string buffer
    "fn broken(reg u32 a -> reg u32 {\n  return a;\n}\n\n";
  for index = 0 to 299 do
    Buffer.add_string buffer
      (Printf.sprintf
         "fn f%d(reg u32 a) -> reg u32 {\n\
         \  reg bool b;\n\
         \  b = a;\n\
         \  return a;\n\
          }\n\n"
         index)
  done;
  let result = check_text (Buffer.contents buffer) in
  let count code =
    List.length (List.filter (fun got -> got = Some code) (codes result))
  in
  check "bound with holes: two hundred diagnostics in all"
    (List.length result.diagnostics = 200);
  check "bound with holes: the compiler's error and the stub are kept"
    (count "parse" = 2);
  check "bound with holes: the typing budget is what is left"
    (count "typing" = 198)

(* An exec clause is typed by the same call: a name that is not declared is a
   typing diagnostic, and one that is declared is silent. *)
let test_exec_clause () =
  let missing =
    check_text
      "exec no_such_fn (0 : 16)\n\n\
       fn f(reg u32 a) -> reg u32 {\n\
      \  return a;\n\
       }\n"
  in
  check "exec: the missing name is reported"
    (mentions missing "unknown function");
  check "exec: and it is the only diagnostic"
    (List.length missing.diagnostics = 1);
  let resolved =
    check_text
      "fn g(reg u32 a) -> reg u32 {\n  return a;\n}\n\nexec g (0 : 16)\n"
  in
  check "exec: a declared name is silent" (resolved.diagnostics = [])

(* The compiler's annotation exception is one of the five the adapter maps.
   No input in this suite makes the library raise it for a plain annotation,
   because annotation checking happens after pretyping, so the mapping is
   pinned here directly rather than left untested. *)
let test_annotation_error_mapping () =
  let location : Jasmin.Location.t =
    {
      Jasmin.Location.loc_fname = file;
      loc_start = (1, 0);
      loc_end = (1, 3);
      loc_bchar = 0;
      loc_echar = 3;
    }
  in
  let diagnostic =
    Jide_jasmin.Error.to_diagnostic
      (Jide_jasmin.Error.of_exn
         (Jasmin.Annot.AnnotationError
            (location, fun formatter -> Format.pp_print_string formatter "boom")))
  in
  check "annotation: the code is the annotation one"
    (diagnostic.Jide_lang.Diagnostic.code = Some "annotation");
  check "annotation: the message is printed" (diagnostic.message = "boom");
  check "annotation: the span is the location's"
    (diagnostic.span.Jide_lang.Span.start_byte = 0
    && diagnostic.span.Jide_lang.Span.end_byte = 3)

(* A dropped item whose header carries no name at all declares nothing the
   loop could know, so it suppresses nothing: an unrelated unknown name after
   it is still the program's own error. *)
let test_nameless_stub_suppresses_nothing () =
  let result =
    check_text
      "type ;\n\n\
       fn use(reg u32 a) -> reg u32 {\n\
      \  reg u32 x;\n\
      \  x = nosuch(a);\n\
      \  return x;\n\
       }\n"
  in
  check "nameless: the unknown name is reported"
    (mentions result "unknown function");
  check "nameless: beside the compiler's error and the stub"
    (List.length result.diagnostics = 3)

(* No exception leaves the adapter, whatever the input. *)
let test_nothing_escapes () =
  let hostile =
    [
      {|from NOPE require "x.jinc"

fn f(reg u32 a) -> reg u32 {
  return a;
}
|};
      "namespace N { namespace M { fn f( ";
      String.concat "" [ "fn f() { "; String.make 2000 '{'; " }" ];
      "\xff\xfe\xfd";
      "fn f(reg u32 a) -> reg u32 {\n  reg u32 x\n  x = a;\n  return x;\n}\n";
    ]
  in
  List.iter
    (fun text ->
      match check_text text with
      | result ->
          check "hostile: a check always answers"
            (List.length result.Jide_jasmin.Adapter.diagnostics >= 1)
      | exception _ -> check "hostile: nothing escapes" false)
    hostile

(* A compiler error the library raises as an exception renders the location
   and the kind itself. The diagnostic carries the location in its span and
   the kind in its code, so its message is the compiler's text without
   either: a check prints a diagnostic once, and an editor that has the span
   does not need the path and the line in the text as well. *)
let test_compiler_error_text () =
  let text =
    {|fn f(stack u64 a) -> reg u64 {
  reg u64 r;
  r = 1;
  return r;
}
|}
  in
  match (check_text text).Jide_jasmin.Adapter.diagnostics with
  | [ diagnostic ] ->
      check "compiler error: the kind is the diagnostic's code"
        (diagnostic.code = Some "typing");
      check "compiler error: the message is the compiler's own text"
        (diagnostic.message
       = "a has kind stack, only reg or reg ptr are allowed in parameter of \
          non inlined function");
      check "compiler error: the location is the span, not the message"
        (diagnostic.span = Jide_lang.Span.make ~start_byte:15 ~end_byte:16)
  | other ->
      check
        ("compiler error: one diagnostic, got "
        ^ string_of_int (List.length other))
        false

let () =
  test_typing_over_recovered ();
  test_failed_item_does_not_cascade ();
  test_dropped_item_does_not_cascade ();
  test_namespace_does_not_cascade ();
  test_genuine_unknown_names_are_reported ();
  test_recovered_region_suppresses ();
  test_recovered_item_is_quiet ();
  test_diagnostic_bound ();
  test_diagnostic_bound_with_parse_errors ();
  test_exec_clause ();
  test_annotation_error_mapping ();
  test_nameless_stub_suppresses_nothing ();
  test_nothing_escapes ();
  test_compiler_error_text ();
  if !failures = 0 then print_endline "all typing tests passed"
  else (
    Printf.printf "%d typing test(s) failed\n" !failures;
    exit 1)
