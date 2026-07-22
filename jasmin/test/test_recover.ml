(* Tests for the recovering parse.

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

let parse text = Jide_jasmin.Parse.parse ~name:"probe.jazz" text

(** The diagnostic the compiler itself produces on these bytes, which the
    recovering parse must keep as its first. *)
let compiler_diagnostic text =
  match
    Jasmin.Parseio.parse_program ~name:"probe.jazz"
      (Jasmin.Utils.IO.input_string text)
  with
  | _ -> None
  | exception exn -> Some (Jide_jasmin.Error.to_diagnostic (Jide_jasmin.Error.of_exn exn))

let first_diagnostic parsed =
  match Jide_jasmin.Parse.diagnostics parsed with
  | first :: _ -> Some first
  | [] -> None

let same_span (a : Jide_lang.Diagnostic.t) (b : Jide_lang.Diagnostic.t) =
  a.span.Jide_lang.Span.start_byte = b.span.Jide_lang.Span.start_byte
  && a.span.Jide_lang.Span.end_byte = b.span.Jide_lang.Span.end_byte

let item_names parsed =
  List.filter_map
    (fun (item : Jasmin.Syntax.pitem Jasmin.Location.located) ->
      match Jasmin.Location.unloc item with
      | Jasmin.Syntax.PFundef definition ->
          Some (Jasmin.Location.unloc definition.Jasmin.Syntax.pdf_name)
      | Jasmin.Syntax.PParam parameter ->
          Some (Jasmin.Location.unloc parameter.Jasmin.Syntax.ppa_name)
      | Jasmin.Syntax.PTypeAlias (name, _, _)
      | Jasmin.Syntax.PNamespace (name, _) ->
          Some (Jasmin.Location.unloc name)
      | Jasmin.Syntax.PGlobal global ->
          Some (Jasmin.Location.unloc global.Jasmin.Syntax.pgd_name)
      | Jasmin.Syntax.Pexec _ | Jasmin.Syntax.Prequire _ -> None)
    (Jide_jasmin.Parse.items parsed)

let dropped_names parsed =
  List.filter_map
    (function
      | Jide_jasmin.Recover.Dropped { name; _ } -> name)
    (Jide_jasmin.Parse.dropped parsed)

let ends parsed =
  List.map
    (fun (item : Jasmin.Syntax.pitem Jasmin.Location.located) ->
      item.Jasmin.Location.pl_loc.Jasmin.Location.loc_echar)
    (Jide_jasmin.Parse.items parsed)

let counters parsed = Jide_jasmin.Parse.counters parsed
let recoveries parsed = (counters parsed).Jide_jasmin.Recover.recoveries
let item_restarts parsed = (counters parsed).Jide_jasmin.Recover.item_restarts
let statement_restarts parsed =
  (counters parsed).Jide_jasmin.Recover.statement_restarts

let byte_restarts parsed = (counters parsed).Jide_jasmin.Recover.byte_restarts

let valid =
  {|fn first(reg u32 a) -> reg u32 {
  reg u32 b;
  b = a;
  return b;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 c;
  c = a;
  return c;
}
|}

(* The five faults the recovery spike injected, on one base file. Each is a
   single damaged byte or line, so the first diagnostic is the compiler's own
   on exactly these bytes. *)
let base =
  {|fn first(reg u32 a) -> reg u32 {
  reg u32 b;
  b = a;
  return b;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 c;
  c = a;
  return c;
}
|}

let with_broken_header =
  {|fn first(reg u32 a -> reg u32 {
  reg u32 b;
  return b;
}

fn second(reg u32 a) -> reg u32 {
  return a;
}
|}

let first_item_broken =
  {|fn first(reg u32 a -> reg u32 {
  reg u32 b;
  b = a;
  return b;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 c;
  c = a;
  return c;
}

fn third(reg u32 a) -> reg u32 {
  return a;
}
|}

let missing_rbrace =
  {|fn first(reg u32 a) -> reg u32 {
  reg u32 b;
  b = a;
  return b;

fn second(reg u32 a) -> reg u32 {
  return a;
}
|}

let missing_semicolon =
  {|fn first(reg u32 a) -> reg u32 {
  reg u32 b;
  b = a
  return b;
}

fn second(reg u32 a) -> reg u32 {
  return a;
}
|}

let missing_fn =
  {|fn first(reg u32 a) -> reg u32 {
  return a;
}

second(reg u32 a) -> reg u32 {
  return a;
}

fn third(reg u32 a) -> reg u32 {
  return a;
}
|}

let test_valid () =
  let parsed = parse valid in
  check "valid: no diagnostics" (Jide_jasmin.Parse.diagnostics parsed = []);
  check "valid: nothing dropped" (Jide_jasmin.Parse.dropped parsed = []);
  check "valid: two items" (List.length (Jide_jasmin.Parse.items parsed) = 2);
  check "valid: the items keep their names"
    (item_names parsed = [ "first"; "second" ]);
  check "valid: no restart" (recoveries parsed = 0)

(* The compiler's own error is the first diagnostic, on its location and its
   message, for every fault. *)
let test_first_error_is_the_compilers () =
  let cases =
    [
      ("broken header", with_broken_header);
      ("missing rbrace", missing_rbrace);
      ("missing semicolon", missing_semicolon);
      ("missing fn", missing_fn);
    ]
  in
  List.iter
    (fun (label, text) ->
      match (compiler_diagnostic text, first_diagnostic (parse text)) with
      | Some expected, Some actual ->
          check (label ^ ": same span") (same_span expected actual);
          check (label ^ ": same message") (expected.message = actual.message);
          check (label ^ ": same code")
            (expected.code = actual.code && expected.code = Some "parse")
      | Some _, None -> check (label ^ ": a diagnostic survives recovery") false
      | None, _ -> check (label ^ ": the fixture does not parse") false)
    cases

let test_later_items_survive () =
  let parsed = parse first_item_broken in
  check "later items: both later items are in the tree"
    (List.length (Jide_jasmin.Parse.items parsed) = 2);
  check "later items: their names"
    (item_names parsed = [ "second"; "third" ]);
  check "later items: the broken one is stubbed"
    (dropped_names parsed = [ "first" ]);
  check "later items: the stub is an item stub"
    (List.exists
       (function
         | Jide_jasmin.Recover.Dropped { kind; _ } -> kind = "function")
       (Jide_jasmin.Parse.dropped parsed));
  check "later items: the checkpoint restart did the work"
    (item_restarts parsed >= 1 && byte_restarts parsed = 0)

let test_missing_rbrace () =
  let parsed = parse missing_rbrace in
  check "missing rbrace: the item after the broken one is in the tree"
    (item_names parsed = [ "second" ]);
  check "missing rbrace: the broken one is stubbed"
    (dropped_names parsed = [ "first" ])

let test_missing_semicolon () =
  let parsed = parse missing_semicolon in
  check "missing semicolon: both items are in the tree"
    (item_names parsed = [ "first"; "second" ]);
  check "missing semicolon: nothing is dropped"
    (Jide_jasmin.Parse.dropped parsed = []);
  check "missing semicolon: a statement checkpoint did the work"
    (statement_restarts parsed >= 1);
  check "missing semicolon: the repaired body is reported"
    (List.length (Jide_jasmin.Parse.recovered parsed) >= 1)

let test_missing_fn () =
  let parsed = parse missing_fn in
  check "missing fn: the items around the broken one are in the tree"
    (item_names parsed = [ "first"; "third" ]);
  check "missing fn: the broken item is stubbed with its name"
    (dropped_names parsed = [ "second" ]);
  check "missing fn: the item-level byte restart did the work"
    (byte_restarts parsed >= 1)

(* The guard against grammar drift: the tokens the driver restarts at are the
   grammar's own FIRST sets, read from Menhir's inspection API. *)
module Interpreter = Jasmin.Parser.MenhirInterpreter

