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

let file = "/tmp/probe.jazz"
let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let check_text text =
  Jide_jasmin.Adapter.check x86_64 linux ~file ~text ~idirs:[]

module X86 =
  (val Jasmin.CoreArchFactory.get_arch_module Jasmin.Utils.X86_64
         Jasmin.Glob_options.Linux
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
  check "recovered: one parse diagnostic and one typing diagnostic"
    (List.length result.diagnostics = 2);
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
  check "dropped: the compiler's parse error is the only one"
    (List.length result.diagnostics = 1);
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
    Loop.type_items ~dropped:[] ~recovered:[] ~file ~items:parsed.items
      ~idirs:[]
  in
  check "region: the error is reported without a recovered region"
    (List.length plain.Jide_jasmin.Check.diagnostics = 1);
  match plain.diagnostics with
  | [ diagnostic ] ->
      let suppressed, _ =
        Loop.type_items ~dropped:[]
          ~recovered:[ diagnostic.Jide_lang.Diagnostic.span ]
          ~file ~items:parsed.items ~idirs:[]
      in
      check "region: the error inside the recovered region is dropped"
        (suppressed.Jide_jasmin.Check.diagnostics = [])
  | other ->
      check
        ("region: one diagnostic, got " ^ string_of_int (List.length other))
        false

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

let () =
  test_typing_over_recovered ();
  test_failed_item_does_not_cascade ();
  test_dropped_item_does_not_cascade ();
  test_recovered_region_suppresses ();
  test_recovered_item_is_quiet ();
  test_diagnostic_bound ();
  test_nothing_escapes ();
  if !failures = 0 then print_endline "all typing tests passed"
  else (
    Printf.printf "%d typing test(s) failed\n" !failures;
    exit 1)
