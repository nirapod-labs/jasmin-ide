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

