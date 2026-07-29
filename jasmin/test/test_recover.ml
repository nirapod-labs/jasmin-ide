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
  | exception exn ->
      Some (Jide_jasmin.Error.to_diagnostic (Jide_jasmin.Error.of_exn exn))

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
    (function Jide_jasmin.Recover.Dropped { name; _ } -> name)
    (Jide_jasmin.Parse.dropped parsed)

(** Every name the tree declares, namespaces walked into, so a test can say a
    name is nowhere in the tree rather than only at its top level. *)
let rec names_of_item (item : Jasmin.Syntax.pitem Jasmin.Location.located) =
  match Jasmin.Location.unloc item with
  | Jasmin.Syntax.PFundef definition ->
      [ Jasmin.Location.unloc definition.Jasmin.Syntax.pdf_name ]
  | Jasmin.Syntax.PParam parameter ->
      [ Jasmin.Location.unloc parameter.Jasmin.Syntax.ppa_name ]
  | Jasmin.Syntax.PTypeAlias (name, _, _) -> [ Jasmin.Location.unloc name ]
  | Jasmin.Syntax.PNamespace (name, inner) ->
      Jasmin.Location.unloc name :: List.concat_map names_of_item inner
  | Jasmin.Syntax.PGlobal global ->
      [ Jasmin.Location.unloc global.Jasmin.Syntax.pgd_name ]
  | Jasmin.Syntax.Pexec _ | Jasmin.Syntax.Prequire _ -> []

let all_names parsed =
  List.concat_map names_of_item (Jide_jasmin.Parse.items parsed)

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

let brace_restarts parsed = (counters parsed).Jide_jasmin.Recover.brace_restarts
let byte_restarts parsed = (counters parsed).Jide_jasmin.Recover.byte_restarts

(** The offset [needle] begins at, or -1. *)
let index_of haystack needle =
  let n = String.length haystack and m = String.length needle in
  let rec loop index =
    if m = 0 then 0
    else if index + m > n then -1
    else if String.sub haystack index m = needle then index
    else loop (index + 1)
  in
  loop 0

(** True when the diagnostic list has one whose code is [code]. *)
let reports parsed code =
  List.exists
    (fun (diagnostic : Jide_lang.Diagnostic.t) -> diagnostic.code = Some code)
    (Jide_jasmin.Parse.diagnostics parsed)

(** True when the first diagnostic the recovering parse reports is the one
    the compiler reports on the same bytes. *)
let keeps_the_compilers_error text parsed =
  match (compiler_diagnostic text, first_diagnostic parsed) with
  | Some expected, Some actual ->
      same_span expected actual && expected.message = actual.message
  | _ -> false

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

(* The two remaining mutations the recovery spike injected: an unmatched
   right parenthesis inserted inside a body, and a call cut where its
   arguments begin. *)
let inserted_rparen =
  {|fn first(reg u32 a) -> reg u32 {
  reg u32 b;
  b = a);
  return b;
}

fn second(reg u32 a) -> reg u32 {
  return a;
}
|}

let truncated_call =
  {|fn first(reg u32 a) -> reg u32 {
  reg u32 b;
  b = foo(a
  return b;
}

fn second(reg u32 a) -> reg u32 {
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
   message, for every fault, and it is the only diagnostic the driver adds to
   it: recovery rewrites bytes or skips them, it does not invent errors. *)
let test_first_error_is_the_compilers () =
  let cases =
    [
      ("delete rbrace", missing_rbrace);
      ("delete semicolon", missing_semicolon);
      ("insert rparen", inserted_rparen);
      ("truncate call", truncated_call);
      ("delete fn", missing_fn);
      ("broken header", with_broken_header);
    ]
  in
  List.iter
    (fun (label, text) ->
      match (compiler_diagnostic text, first_diagnostic (parse text)) with
      | Some expected, Some actual ->
          check (label ^ ": same span") (same_span expected actual);
          check (label ^ ": same message") (expected.message = actual.message);
          check (label ^ ": same code")
            (expected.code = actual.code && expected.code = Some "parse");
          check
            (label ^ ": one diagnostic")
            (List.length (Jide_jasmin.Parse.diagnostics (parse text)) = 1)
      | Some _, None -> check (label ^ ": a diagnostic survives recovery") false
      | None, _ -> check (label ^ ": the fixture does not parse") false)
    cases

let test_later_items_survive () =
  let parsed = parse first_item_broken in
  check "later items: both later items are in the tree"
    (List.length (Jide_jasmin.Parse.items parsed) = 2);
  check "later items: their names" (item_names parsed = [ "second"; "third" ]);
  check "later items: the broken one is stubbed"
    (dropped_names parsed = [ "first" ]);
  check "later items: the stub is an item stub"
    (List.exists
       (function
         | Jide_jasmin.Recover.Dropped { kind; _ } ->
             kind = Jide_jasmin.Recover.Function)
       (Jide_jasmin.Parse.dropped parsed));
  check "later items: the checkpoint restart did the work"
    (item_restarts parsed >= 1 && byte_restarts parsed = 0)

let test_missing_rbrace () =
  let parsed = parse missing_rbrace in
  (* The block that was never closed is closed by a virtual brace, which is
     the one repair that loses nothing: both functions stay in the tree and
     no byte of the file is rewritten. *)
  check "missing rbrace: both items are in the tree"
    (item_names parsed = [ "first"; "second" ]);
  check "missing rbrace: nothing is dropped"
    (Jide_jasmin.Parse.dropped parsed = []);
  check "missing rbrace: a virtual brace did the work"
    (brace_restarts parsed >= 1 && byte_restarts parsed = 0)

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

(* A fault inside a body keeps the function it is in: the statement is
   dropped, the rest of the body and the item after it are not. *)
let test_statement_faults () =
  List.iter
    (fun (label, text) ->
      let parsed = parse text in
      check
        (label ^ ": both functions are in the tree")
        (item_names parsed = [ "first"; "second" ]);
      check
        (label ^ ": nothing is dropped")
        (Jide_jasmin.Parse.dropped parsed = []);
      check
        (label ^ ": a statement checkpoint did the work")
        (statement_restarts parsed >= 1 && item_restarts parsed = 0))
    [ ("insert rparen", inserted_rparen); ("truncate call", truncated_call) ]

(* The restart sets name token kinds: a name token starts a statement
   whatever the name is. Structural equality would accept only the empty
   name written into the set, which is exactly what a name-level comparison
   against the grammar cannot see. *)
let test_token_matching () =
  check "tokens: a name starts a statement"
    (Jide_jasmin.Scan.statement_start (Jasmin.Parser.NID "anything"));
  check "tokens: the empty name does too"
    (Jide_jasmin.Scan.statement_start (Jasmin.Parser.NID ""));
  List.iter
    (fun token ->
      check "tokens: the statement set accepts its members"
        (Jide_jasmin.Scan.statement_start token))
    Jide_jasmin.Scan.statement_tokens;
  List.iter
    (fun token ->
      check "tokens: the item set accepts its members"
        (Jide_jasmin.Scan.item_start token))
    Jide_jasmin.Scan.item_tokens;
  check "tokens: a token outside the statement set is not a start"
    (not (Jide_jasmin.Scan.statement_start Jasmin.Parser.RETURN));
  check "tokens: a token outside the item set is not a start"
    (not (Jide_jasmin.Scan.item_start Jasmin.Parser.RETURN));
  check "tokens: the statement set has no duplicates"
    (List.length (List.sort_uniq compare Jide_jasmin.Scan.statement_tokens)
    = List.length Jide_jasmin.Scan.statement_tokens);
  check "tokens: the item set has no duplicates"
    (List.length (List.sort_uniq compare Jide_jasmin.Scan.item_tokens)
    = List.length Jide_jasmin.Scan.item_tokens)

(* The stub keeps the bytes of the item it stands for, so an outline can
   place it and the typing loop can tell which diagnostics fall inside
   recovered text. *)
let test_stub_span () =
  match Jide_jasmin.Parse.dropped (parse first_item_broken) with
  | [ Jide_jasmin.Recover.Dropped { name; span; _ } ] ->
      check "stub: the name" (name = Some "first");
      check "stub: it begins at the broken item"
        (span.Jide_lang.Span.start_byte = 0);
      check "stub: it ends where the next item begins"
        (span.Jide_lang.Span.end_byte > 0
        && contains
             (String.sub first_item_broken span.Jide_lang.Span.end_byte
                (String.length first_item_broken - span.end_byte))
             "fn second");
      check "stub: the recovered region is the stub's span"
        (List.exists
           (fun (recovered : Jide_lang.Span.t) -> recovered = span)
           (Jide_jasmin.Parse.recovered (parse first_item_broken)))
  | other ->
      check ("stub: one stub, got " ^ string_of_int (List.length other)) false

(* The guard against grammar drift: the tokens the driver restarts at are the
   grammar's own FIRST sets, read from Menhir's inspection API. *)
module Interpreter = Jasmin.Parser.MenhirInterpreter

let first_terminal_names (nonterminal : _ Interpreter.nonterminal) =
  Interpreter.foreach_terminal_but_error
    (fun symbol acc ->
      match symbol with
      | Interpreter.X (Interpreter.T terminal) ->
          if Interpreter.first nonterminal terminal then
            Token_names.terminal_name terminal :: acc
          else acc
      | Interpreter.X (Interpreter.N _) -> acc)
    []
  |> List.sort_uniq String.compare

let driver_names tokens =
  List.sort_uniq String.compare (List.map Token_names.token_name tokens)

let test_guard_top () =
  let top = first_terminal_names Interpreter.N_top in
  let type_token = first_terminal_names Interpreter.N_ptype in
  (* A top-level item that begins with a type is a global declaration, and a
     type name cannot be told from a stray identifier, so those tokens are
     not restart points. Every other token that can start an item is. *)
  let keywords = List.filter (fun name -> not (List.mem name type_token)) top in
  check "guard: the driver's item set is the grammar's item keywords"
    (driver_names Jide_jasmin.Scan.item_tokens = keywords);
  check "guard: no restart point is a type"
    (List.for_all
       (fun name -> not (List.mem name type_token))
       (driver_names Jide_jasmin.Scan.item_tokens));
  check "guard: every token that starts an item is accounted for"
    (List.for_all
       (fun name -> List.mem name top)
       (driver_names Jide_jasmin.Scan.item_tokens))

let test_guard_statement () =
  let pinstr = first_terminal_names Interpreter.N_pinstr in
  check "guard: the driver's statement set is the grammar's"
    (driver_names Jide_jasmin.Scan.statement_tokens = pinstr)

(* A buffer over the policy's byte ceiling is refused with one diagnostic of
   its own kind: a refusal by policy is not a syntax error, and no consumer
   should offer a syntax fix for it. The ceiling is read from the module so
   the test and the policy cannot drift apart. *)
let test_size_policy () =
  let text = String.make (Jide_jasmin.Scan.max_bytes + 1) ' ' in
  let parsed = parse text in
  check "size: one diagnostic"
    (List.length (Jide_jasmin.Parse.diagnostics parsed) = 1);
  check "size: nothing parsed" (Jide_jasmin.Parse.items parsed = []);
  check "size: no recovery ran" (recoveries parsed = 0);
  check "size: the code is the policy's own" (reports parsed "size");
  check "size: it is not a syntax error" (not (reports parsed "parse"));
  check "size: the span points at no byte"
    (match Jide_jasmin.Parse.diagnostics parsed with
    | [ diagnostic ] ->
        diagnostic.Jide_lang.Diagnostic.span.Jide_lang.Span.start_byte = 0
        && diagnostic.Jide_lang.Diagnostic.span.Jide_lang.Span.end_byte = 0
    | _ -> false);
  check "size: the message says so"
    (match Jide_jasmin.Parse.diagnostics parsed with
    | [ diagnostic ] ->
        contains diagnostic.Jide_lang.Diagnostic.message "larger"
    | _ -> false)

(* A buffer that is under the byte ceiling but is one token per byte is
   refused too: the number of tokens, not the number of bytes, is what the
   work is linear in, and building two million tokens on the thread that
   answers requests costs about a second. *)
let test_token_cap () =
  let text = String.make (2 * 1024 * 1024) '}' in
  let parsed = parse text in
  check "tokens: the dense buffer is refused" (reports parsed "size");
  check "tokens: nothing parsed" (Jide_jasmin.Parse.items parsed = []);
  check "tokens: no recovery ran" (recoveries parsed = 0);
  (* A dense buffer under the cap is still handled: this is the shape that
     used to cost a second, at a hundredth of the size. *)
  let small = parse (String.make 10_000 '}') in
  check "tokens: a small dense buffer is analysed"
    (Jide_jasmin.Parse.items small = []);
  check "tokens: it is stubbed once"
    (List.length (Jide_jasmin.Parse.dropped small) = 1)

(* The compiler rejects a byte order mark at byte zero. Blanking it before
   the fast path would report the file clean while the compiler reports an
   error, so the mark is blanked by the recovery instead: the compiler's own
   error is kept and the items after the mark are still returned. *)
let test_byte_order_mark () =
  let marked = "\xef\xbb\xbf" ^ valid in
  let parsed = parse marked in
  check "bom: the compiler's error is kept"
    (keeps_the_compilers_error marked parsed);
  check "bom: and it is the only one"
    (List.length (Jide_jasmin.Parse.diagnostics parsed) = 1);
  check "bom: both items" (List.length (Jide_jasmin.Parse.items parsed) = 2);
  check "bom: the mark is reported as rewritten"
    (List.exists
       (fun (span : Jide_lang.Span.t) ->
         span.Jide_lang.Span.start_byte = 0 && span.Jide_lang.Span.end_byte = 3)
       (Jide_jasmin.Parse.recovered parsed));
  (* Blanking keeps the length, so every offset after the mark is the
     marked file's own offset: the same item ends three bytes later than it
     does without the mark, and nothing else moves. *)
  check "bom: the offsets are the marked file's own"
    (ends parsed = List.map (fun at -> at + 3) (ends (parse valid)))

let test_empty_and_comment () =
  let empty = parse "" in
  check "empty: clean" (Jide_jasmin.Parse.diagnostics empty = []);
  check "empty: no items" (Jide_jasmin.Parse.items empty = []);
  let comment = parse "// nothing here\n\n/* nor here */\n" in
  check "comment: clean" (Jide_jasmin.Parse.diagnostics comment = []);
  check "comment: no items" (Jide_jasmin.Parse.items comment = [])

(* A buffer of [count] functions, each with one repairable fault, and the
   same buffer with the faults removed. *)
let broken_functions count =
  let buffer = Buffer.create 4096 in
  for index = 0 to count - 1 do
    Buffer.add_string buffer
      (Printf.sprintf
         "#[inline]\n\
          fn f%d(reg u32 a) -> reg u32 {\n\
         \  reg u32 b;\n\
         \  b = ;\n\
         \  return a;\n\
          }\n\n"
         index)
  done;
  Buffer.contents buffer

let test_recovery_bound () =
  let text = broken_functions 100 in
  let parsed = parse text in
  check "bound: never more than fifty restarts" (recoveries parsed <= 50);
  check "bound: it stops at exactly fifty" (recoveries parsed = 50);
  (* The bound must not throw away the fifty items it already repaired: a
     file with more faults than the bound returns the prefix. *)
  check "bound: the repaired prefix is kept"
    (List.length (Jide_jasmin.Parse.items parsed) = 50);
  check "bound: with their names, in order"
    (item_names parsed = List.init 50 (fun index -> Printf.sprintf "f%d" index));
  check "bound: the unrepaired tail is stubbed"
    (match Jide_jasmin.Parse.dropped parsed with
    | [ Jide_jasmin.Recover.Dropped { kind; name; _ } ] ->
        kind = Jide_jasmin.Recover.Function && name = Some "f50"
    | other ->
        let () = Printf.printf "  (bound: %d stubs)\n%!" (List.length other) in
        false)

(* A valid file followed by a comment that is never closed: the compiler
   reports the comment at the end of the buffer, where there is no line left
   to blank, so the tokens read before it are the whole of what can be kept
   and they are a valid prefix. Returning nothing here would throw away two
   functions the user wrote and the compiler read. *)
let test_unterminated_comment_keeps_the_prefix () =
  let text = valid ^ "\n/* oops\n" in
  let parsed = parse text in
  check "comment prefix: both items are in the tree"
    (item_names parsed = [ "first"; "second" ]);
  check "comment prefix: nothing is dropped"
    (Jide_jasmin.Parse.dropped parsed = []);
  check "comment prefix: the compiler's error is kept"
    (keeps_the_compilers_error text parsed);
  check "comment prefix: no recovery was needed" (recoveries parsed = 0)

(* A function whose closing brace is missing, and nothing after it. The
   design closes the block with a virtual brace so the header and the body
   read so far stay in the tree; a drop here loses the file's only item. *)
let test_virtual_brace_keeps_the_function () =
  let text = "fn only(reg u32 a) -> reg u32 {\n  return a;\n" in
  let parsed = parse text in
  check "virtual brace: the function stays in the tree"
    (item_names parsed = [ "only" ]);
  check "virtual brace: nothing is dropped"
    (Jide_jasmin.Parse.dropped parsed = []);
  check "virtual brace: a brace restart did the work" (brace_restarts parsed = 1);
  check "virtual brace: no byte is rewritten"
    (Jide_jasmin.Parse.recovered parsed = [] && byte_restarts parsed = 0);
  check "virtual brace: the compiler's error is kept"
    (keeps_the_compilers_error text parsed)

(* A namespace with one broken item and one whole one: the whole one parses,
   so it must reach the tree inside its namespace, and the broken one must be
   stubbed once, under its own name rather than the namespace's. *)
let namespace_with_a_broken_item =
  {|namespace N {
  fn broken(reg u32 a -> reg u32 {
    return a;
  }

  fn ok(reg u32 a) -> reg u32 {
    return a;
  }
}
|}

let test_namespace_keeps_the_whole_item () =
  let parsed = parse namespace_with_a_broken_item in
  check "namespace: one item" (List.length (Jide_jasmin.Parse.items parsed) = 1);
  check "namespace: the namespace survives with the whole function in it"
    (match Jide_jasmin.Parse.items parsed with
    | [ item ] -> (
        match Jasmin.Location.unloc item with
        | Jasmin.Syntax.PNamespace (name, inner) ->
            Jasmin.Location.unloc name = "N"
            && List.concat_map names_of_item inner = [ "ok" ]
        | _ -> false)
    | _ -> false);
  check "namespace: the broken item is stubbed under its own name"
    (dropped_names parsed = [ "broken" ]);
  check "namespace: one stub, not two"
    (List.length (Jide_jasmin.Parse.dropped parsed) = 1);
  check "namespace: the stub is a function stub"
    (match Jide_jasmin.Parse.dropped parsed with
    | [ Jide_jasmin.Recover.Dropped { kind; _ } ] ->
        kind = Jide_jasmin.Recover.Function
    | _ -> false)

(* An item after a namespace. A restart from a checkpoint inside the
   namespace at a top-level keyword reads the rest of the file as if it were
   still inside the namespace, whose closing brace the restart skipped, so
   the parser never sees the end of the block and the whole file is lost. *)
let test_namespace_and_the_items_after_it () =
  let text =
    namespace_with_a_broken_item
    ^ "\nfn after(reg u32 a) -> reg u32 {\n  return a;\n}\n"
  in
  let parsed = parse text in
  check "namespace tail: the namespace keeps its whole item"
    (match Jide_jasmin.Parse.items parsed with
    | first :: _ -> (
        match Jasmin.Location.unloc first with
        | Jasmin.Syntax.PNamespace (name, inner) ->
            Jasmin.Location.unloc name = "N"
            && List.concat_map names_of_item inner = [ "ok" ]
        | _ -> false)
    | [] -> false);
  check "namespace tail: the item after the namespace survives"
    (item_names parsed = [ "N"; "after" ]);
  check "namespace tail: the broken item is stubbed once"
    (List.length (Jide_jasmin.Parse.dropped parsed) = 1);
  (* A namespace that is dropped altogether must not take the items after it
     with it either. *)
  let dropped =
    parse
      "namespace N {\n\
      \  fn broken(reg u32 a -> reg u32 { return a; }\n\
       }\n\
       fn after(reg u32 a) -> reg u32 {\n\
      \  return a;\n\
       }\n"
  in
  check "namespace tail: a dropped namespace leaves the rest in the tree"
    (item_names dropped = [ "after" ]);
  check "namespace tail: the dropped namespace is stubbed under its own name"
    (match Jide_jasmin.Parse.dropped dropped with
    | [ Jide_jasmin.Recover.Dropped { kind; name; _ } ] ->
        kind = Jide_jasmin.Recover.Namespace && name = Some "N"
    | _ -> false)

(* A stub names the thing it stands for. A parameter and a global are named
   after their type, so the first name after the keyword is the wrong one. *)
let test_stub_names_the_item () =
  let stub_of text =
    match Jide_jasmin.Parse.dropped (parse text) with
    | [ Jide_jasmin.Recover.Dropped { kind; name; _ } ] -> (kind, name)
    | other ->
        (Jide_jasmin.Recover.Item, Some (string_of_int (List.length other)))
  in
  check "stub name: a parameter is named after its type"
    (stub_of
       "param Foo x = ;\n\nfn ok(reg u32 a) -> reg u32 {\n  return a;\n}\n"
    = (Jide_jasmin.Recover.Parameter, Some "x"));
  check "stub name: a global is named after its type"
    (stub_of "u32 g = ;\n\nfn ok(reg u32 a) -> reg u32 {\n  return a;\n}\n"
    = (Jide_jasmin.Recover.Global, Some "g"));
  check "stub name: a type alias"
    (stub_of "type T = ;\n\nfn ok(reg u32 a) -> reg u32 {\n  return a;\n}\n"
    = (Jide_jasmin.Recover.Type, Some "T"))

(* No two stubs may stand for the same bytes: a consumer that lists them
   would show the same broken item twice. *)
let test_stubs_do_not_overlap () =
  let spans =
    List.map
      (function Jide_jasmin.Recover.Dropped { span; _ } -> span)
      (Jide_jasmin.Parse.dropped (parse namespace_with_a_broken_item))
  in
  let disjoint (a : Jide_lang.Span.t) (b : Jide_lang.Span.t) =
    a.Jide_lang.Span.end_byte <= b.Jide_lang.Span.start_byte
    || b.Jide_lang.Span.end_byte <= a.Jide_lang.Span.start_byte
  in
  let rec no_overlap = function
    | [] -> true
    | span :: others -> List.for_all (disjoint span) others && no_overlap others
  in
  check "stubs: no two stand for the same bytes" (no_overlap spans)

(* The worst input found in review: a function written inside a string the
   user never closed. The compiler never tokenises those bytes, so nothing
   the user wrote is a function there, and the tree must not gain one. *)
let unterminated_string_multiline =
  valid
  ^ "\nrequire \"oops\nfn phantom(reg u32 z) -> reg u32 {\n  return z;\n}\n"

let test_no_function_from_a_string () =
  let parsed = parse unterminated_string_multiline in
  check "string: only the two items that are really there"
    (item_names parsed = [ "first"; "second" ]);
  check "string: the name inside the string is nowhere in the tree"
    (not (List.mem "phantom" (all_names parsed)));
  check "string: the compiler's error is kept"
    (keeps_the_compilers_error unterminated_string_multiline parsed);
  (* The same, one line long, and with a single stray quote: every shape of a
     half-typed string. *)
  List.iter
    (fun (label, text) ->
      let parsed = parse text in
      check
        (label ^ ": no phantom function")
        (not (List.mem "phantom" (all_names parsed)));
      check
        (label ^ ": the two real items survive")
        (item_names parsed = [ "first"; "second" ]))
    [
      ("one line", valid ^ "\nrequire \"phantom\n");
      ("stray quote", valid ^ "\n\"\nfn phantom(reg u32 z) -> reg u32 {\n}\n");
    ]

(* Bytes the scanner rewrote are bytes the compiler never read, so they are
   reported like the ones the driver rewrote: a typing diagnostic inside them
   is a consequence of the broken text, not of the program. *)
let test_blanked_bytes_are_reported () =
  let text = valid ^ "\n@@@garbage@@@\n" in
  let parsed = parse text in
  let at = index_of text "@@@garbage@@@" in
  check "blanked: both items survive" (item_names parsed = [ "first"; "second" ]);
  check "blanked: the compiler's error is kept"
    (keeps_the_compilers_error text parsed);
  check "blanked: the rewritten line is reported"
    (at >= 0
    && List.exists
         (fun (span : Jide_lang.Span.t) ->
           span.Jide_lang.Span.start_byte <= at
           && span.Jide_lang.Span.end_byte > at)
         (Jide_jasmin.Parse.recovered parsed))

(* An item that reaches the tree through an item restart is placed where its
   bytes are, not at the start of the buffer: an outline that lists it points
   at it, and {!Jide_jasmin.Parse.items_with_stubs} can order it. The two
   items abut, as they do in a clean parse: the compiler places an item at
   the end of the token before it, not at the item's own keyword, so the test
   pins the layout rule rather than an offset inside the item's leading
   whitespace. *)
let test_restarted_items_keep_their_place () =
  let parsed = parse first_item_broken in
  let places =
    List.map
      (fun (item : Jasmin.Syntax.pitem Jasmin.Location.located) ->
        ( item.Jasmin.Location.pl_loc.Jasmin.Location.loc_bchar,
          item.Jasmin.Location.pl_loc.Jasmin.Location.loc_echar ))
      (Jide_jasmin.Parse.items parsed)
  in
  let stub_end =
    match Jide_jasmin.Parse.dropped parsed with
    | [ Jide_jasmin.Recover.Dropped { span; _ } ] ->
        span.Jide_lang.Span.end_byte
    | _ -> -1
  in
  check "place: the first kept item begins where the stub ends"
    (match places with (start, _) :: _ -> start = stub_end | [] -> false);
  check "place: every kept item begins where the one before it ends"
    (let rec contiguous = function
       | (_, stop) :: ((start, _) :: _ as rest) ->
           stop = start && contiguous rest
       | _ -> true
     in
     contiguous places);
  check "place: every kept item contains its own keyword"
    (List.for_all2
       (fun (start, stop) keyword ->
         let at = index_of first_item_broken keyword in
         at >= 0 && start <= at && at < stop)
       places
       [ "fn second"; "fn third" ])

(* The stub is not a [Syntax.pitem] because that type is the compiler's, so
   the item stream a consumer walks is the merge of the kept items and the
   stubs, in source order. *)
let test_stubs_are_in_the_item_stream () =
  let parsed = parse first_item_broken in
  let stream = Jide_jasmin.Parse.items_with_stubs parsed in
  check "stream: the stub and the kept items together"
    (List.length stream
    = List.length (Jide_jasmin.Parse.dropped parsed)
      + List.length (Jide_jasmin.Parse.items parsed));
  (match stream with
  | Jide_jasmin.Parse.Stub (Jide_jasmin.Recover.Dropped { name; _ }) :: rest ->
      check "stream: the stub comes first, where its bytes are"
        (name = Some "first");
      check "stream: the kept items follow in source order"
        (List.filter_map
           (function
             | Jide_jasmin.Parse.Kept
                 (item : Jasmin.Syntax.pitem Jasmin.Location.located) -> (
                 match Jasmin.Location.unloc item with
                 | Jasmin.Syntax.PFundef definition ->
                     Some
                       (Jasmin.Location.unloc definition.Jasmin.Syntax.pdf_name)
                 | _ -> None)
             | _ -> None)
           rest
        = [ "second"; "third" ])
  | _ -> check "stream: the stub is first" false);
  check "stream: a tree with no faults has no stub"
    (List.for_all
       (function Jide_jasmin.Parse.Kept _ -> true | _ -> false)
       (Jide_jasmin.Parse.items_with_stubs (parse valid)))

let test_no_exception_escapes () =
  let hostile =
    [
      "fn \x00 f( {\n";
      String.concat "" [ "fn f() { "; String.make 2000 '{'; " }" ];
      "namespace N { namespace M { fn f( ";
      "\xff\xfe\xfd";
      {|require "unterminated|};
    ]
  in
  List.iter
    (fun text ->
      match parse text with
      | parsed ->
          check "hostile: a parse always answers"
            (List.length (Jide_jasmin.Parse.diagnostics parsed) >= 1)
      | exception _ -> check "hostile: nothing escapes" false)
    hostile

let () =
  test_valid ();
  test_first_error_is_the_compilers ();
  test_later_items_survive ();
  test_missing_rbrace ();
  test_missing_semicolon ();
  test_missing_fn ();
  test_statement_faults ();
  test_token_matching ();
  test_stub_span ();
  test_guard_top ();
  test_guard_statement ();
  test_size_policy ();
  test_token_cap ();
  test_byte_order_mark ();
  test_empty_and_comment ();
  test_recovery_bound ();
  test_unterminated_comment_keeps_the_prefix ();
  test_virtual_brace_keeps_the_function ();
  test_namespace_keeps_the_whole_item ();
  test_namespace_and_the_items_after_it ();
  test_stub_names_the_item ();
  test_stubs_do_not_overlap ();
  test_no_function_from_a_string ();
  test_blanked_bytes_are_reported ();
  test_restarted_items_keep_their_place ();
  test_stubs_are_in_the_item_stream ();
  test_no_exception_escapes ();
  if !failures = 0 then print_endline "all recovery tests passed"
  else (
    Printf.printf "%d recovery test(s) failed\n" !failures;
    exit 1)
