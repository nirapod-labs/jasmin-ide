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

