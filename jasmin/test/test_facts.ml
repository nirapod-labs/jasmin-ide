(* The typed facts, checked against a program small enough to read: every
   range the facts report is written out here by the text it covers. *)

module Facts = Jide_jasmin.Facts

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let file = "/tmp/hand-checked.jazz"

let program =
  {|
#[ct="public -> public"]
fn add(reg u64 a, reg u64 b) -> reg u64 {
  reg u64 c;
  c = a + b;
  return c;
}
|}

let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let result =
  Jide_jasmin.Adapter.check x86_64 linux ~file ~text:program ~idirs:[]

let facts = result.Jide_jasmin.Adapter.facts

(* The byte where [needle] first appears in the program. *)
let index_of needle =
  let length = String.length program and width = String.length needle in
  let rec loop index =
    if index + width > length then None
    else if String.equal (String.sub program index width) needle then Some index
    else loop (index + 1)
  in
  loop 0

let offset_of needle =
  match index_of needle with
  | Some index -> index
  | None ->
      check ("the program contains " ^ needle) false;
      0

(* The range of [needle], or of the [length] bytes at [offset] inside it. *)
let span_of ?(offset = 0) ?length needle =
  let start_byte = offset_of needle + offset in
  let width =
    match length with
    | Some width -> width
    | None -> String.length needle - offset
  in
  Jide_lang.Span.make ~start_byte ~end_byte:(start_byte + width)

let same_span name (actual : Jide_lang.Span.t) (expected : Jide_lang.Span.t) =
  check
    (Printf.sprintf "%s: got %d..%d, wanted %d..%d" name actual.start_byte
       actual.end_byte expected.start_byte expected.end_byte)
    (Jide_lang.Span.compare actual expected = 0)

let with_item name f =
  match
    List.find_opt
      (fun (item : Facts.item) -> String.equal item.Facts.i_name name)
      facts.Facts.items
  with
  | Some item -> f item
  | None -> check ("the program has an item named " ^ name) false

let test_diagnostics () =
  check "the program has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = [])

let test_items () =
  check "the program has one item" (List.length facts.Facts.items = 1);
  with_item "add" (fun item ->
      check "the item is a function" (item.Facts.i_kind = Facts.Function);
      same_span "the function's name is its name after fn"
        item.Facts.i_name_span
        (span_of "fn add(" ~offset:3 ~length:3);
      same_span "the function's range runs from its annotation to its brace"
        item.Facts.i_span
        (Jide_lang.Span.make ~start_byte:(offset_of "#[ct=")
           ~end_byte:(offset_of "return c;\n}" + String.length "return c;\n}")))

let test_signature () =
  with_item "add" (fun item ->
      match item.Facts.i_sig with
      | Some signature ->
          check "the signature has two arguments"
            (signature.Facts.fs_in = [ "u64"; "u64" ]);
          check "the signature returns one value"
            (signature.Facts.fs_out = [ "u64" ])
      | None -> check "the function has a signature" false)

