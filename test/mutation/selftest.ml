(** The harness's own checks. They need no corpus, so they run under
    [dune runtest]: the tokenizer, the four mutations, the outline
    comparison and the child-process isolation are all exercised here. A
    failure in the corpus run becomes one of these before it is fixed. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let equal name expected actual =
  if expected <> actual then (
    incr failures;
    Printf.printf "FAIL %s: expected %S, got %S\n%!" name expected actual)

let contains text needle =
  let text_length = String.length text
  and needle_length = String.length needle in
  let rec loop index =
    if needle_length = 0 then true
    else if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  loop 0

let tokens_of text = Token.tokenize text
let texts_of text = List.map (fun token -> token.Token.text) (tokens_of text)
let joined text = String.concat "|" (texts_of text)

let test_tokens () =
  equal "a body splits into its tokens"
    "fn|f|(|reg|u32|a|)|->|reg|u32|{|a|=|a|+|1|;|return|a|;|}"
    (joined
       "fn f(reg u32 a) -> reg u32 { // ;\n  a = a + 1; /* } */ return a; }");
  equal "a string is one token" "\"a;b->c\"|;" (joined "\"a;b->c\" ;");
  equal "an operator keeps its spelling" "a|>>r|b|<|-|c" (joined "a >>r b <- c");
  equal "a signed comparison" "a|<=u|b|#unaligned|#["
    (joined "a <=u b #unaligned #[");
  equal "a number is one token" "0x1f|1_000|;" (joined "0x1f 1_000 ;")

let source = "a ; b } c"
let tokens = Array.of_list (tokens_of source)

let test_mutations () =
  equal "delete removes one token" "a  b } c"
    (Mutant.delete ~source tokens 1).text;
  equal "duplicate copies one token in place" "a ;; b } c"
    (Mutant.duplicate ~source tokens 1).text;
  equal "swap turns a semicolon into a brace" "a } b } c"
    (Mutant.swap ~source tokens 1).text;
  equal "swap turns a brace into a semicolon" "a ; b ; c"
    (Mutant.swap ~source tokens 3).text;
  equal "truncate cuts at the token" "a ; "
    (Mutant.truncate ~source tokens 2).text;
  let deletion = Mutant.delete ~source tokens 1 in
  check "a deletion maps the bytes after it back"
    (deletion.map_back 3 = 4 && deletion.map_back 1 = 1);
  let copy = Mutant.duplicate ~source tokens 1 in
  check "a copy maps the bytes after it back"
    (copy.map_back 5 = 4 && copy.map_back 1 = 1);
  let sites = Mutant.sites ~every:2 tokens Mutant.Truncate in
  equal "truncation takes every second token" "0 2 4"
    (String.concat " " (List.map string_of_int sites));
  let chosen, available = Mutant.sample ~maximum:2 sites in
  equal "a sample spread over them keeps its own count" "2"
    (string_of_int (List.length chosen));
  equal "and knows how many it started from" "3" (string_of_int available);
  let sampled, total = Mutant.sample ~maximum:2 [ 0; 1; 2; 3; 4; 5 ] in
  equal "a sample spreads over the whole list" "0 3"
    (String.concat " " (List.map string_of_int sampled));
  equal "and keeps the total" "6" (string_of_int total)

let entry kind name start_byte end_byte =
  { Outline.kind; name; start_byte; end_byte }

let test_comparison () =
  let original =
    [ entry "fn" "a" 0 20; entry "fn" "b" 21 40; entry "fn" "c" 41 60 ]
  in
  let identical =
    Outline.compare ~original ~mutant:original ~edit_start:30 ~edit_end:31
      ~map_back:(fun offset -> offset)
  in
  check "an untouched outline is unchanged"
    (identical.Outline.identity_ok && identical.Outline.span_ok
   && identical.Outline.away = 2);
  let shortened = [ List.nth original 0; List.nth original 1 ] in
  let lost =
    Outline.compare ~original ~mutant:shortened ~edit_start:30 ~edit_end:31
      ~map_back:(fun offset -> offset)
  in
  check "a lost item before the end is caught" (not lost.Outline.identity_ok);
  let replaced =
    [ entry "fn" "a" 0 20; entry "fn" "b" 21 40; entry "fn" "d" 41 60 ]
  in
  let named =
    Outline.compare ~original ~mutant:replaced ~edit_start:30 ~edit_end:31
      ~map_back:(fun offset -> offset)
  in
  check "the first difference names the item after the edit"
    ((not named.Outline.identity_ok) && contains named.Outline.detail "fn c");
  let moved =
    [ entry "fn" "a" 0 20; entry "fn" "b" 21 40; entry "fn" "c" 40 59 ]
  in
  let shifted =
    Outline.compare ~original ~mutant:moved ~edit_start:2 ~edit_end:3
      ~map_back:(fun offset -> offset)
  in
  check "a moved item is caught by the extent, not by its name"
    (shifted.Outline.identity_ok && not shifted.Outline.span_ok);
  let compensated =
    Outline.compare ~original ~mutant:original ~edit_start:2 ~edit_end:3
      ~map_back:(fun offset -> offset + 1)
  in
  check "a shifted item whose offsets map back is unchanged"
    (compensated.Outline.identity_ok && not compensated.Outline.span_ok);
  (* An item that covers the edit is in neither list, and the mutant still
     holds it. A namespace is that item for any edit inside its body: pairing
     the mutant by position would take it for the first item after the edit
     and every pair after it would compare the wrong two items. *)
  let enclosing =
    [
      entry "namespace" "N" 0 100;
      entry "fn" "a" 5 20;
      entry "fn" "b" 21 40;
      entry "fn" "c" 60 80;
    ]
  in
  let contains =
    Outline.compare ~original:enclosing
      ~mutant:
        [
          entry "namespace" "M" 0 99;
          entry "fn" "a" 5 20;
          entry "fn" "b" 21 40;
          entry "fn" "c" 59 79;
        ]
      ~edit_start:50 ~edit_end:51
      ~map_back:(fun offset -> if offset < 50 then offset else offset + 1)
  in
  check "an item that contains the edit does not shift the pairs"
    (contains.Outline.identity_ok && contains.Outline.span_ok
   && contains.Outline.away = 3);
  let lost_after =
    Outline.compare ~original:enclosing
      ~mutant:
        [
          entry "namespace" "N" 0 99; entry "fn" "a" 5 20; entry "fn" "b" 21 40;
        ]
      ~edit_start:50 ~edit_end:51
      ~map_back:(fun offset -> if offset < 50 then offset else offset + 1)
  in
  check "an item after the edit is still compared when one contains it"
    (not lost_after.Outline.identity_ok);
  let empty =
    Outline.compare ~original:[] ~mutant:[] ~edit_start:0 ~edit_end:1
      ~map_back:(fun o -> o)
  in
  check "an empty outline is unchanged"
    (empty.Outline.identity_ok && empty.Outline.away = 0)

let test_parse () =
  let program =
    "fn a(reg u32 x) -> reg u32 { return x; }\n\
     fn b(reg u32 y) -> reg u32 { return y; }\n"
  in
  let parsed = Jide_jasmin.Adapter.parse ~name:"selftest.jazz" program in
  let outline = Outline.of_items parsed.items in
  equal "two functions are two entries" "fn a|fn b"
    (String.concat "|"
       (List.map
          (fun item -> item.Outline.kind ^ " " ^ item.Outline.name)
          outline));
  check "an entry covers bytes"
    (List.for_all
       (fun item -> item.Outline.end_byte > item.Outline.start_byte)
       outline)

let test_isolation () =
  let hanging =
    Isolate.run ~budget:0.2 (fun () ->
        Unix.sleepf 10.;
        { Probe.errors = 0; warnings = 0; outline = [] })
  in
  check "a hanging check is reported as a hang, not waited for"
    (match hanging with Isolate.Timed_out _ -> true | _ -> false);
  let raising = Isolate.run ~budget:0.5 (fun () -> failwith "boom") in
  check "an exception is reported with its message"
    (match raising with
    | Isolate.Raised { message; _ } -> contains message "boom"
    | _ -> false);
  let returning =
    Isolate.run ~budget:0.5 (fun () ->
        { Probe.errors = 2; warnings = 1; outline = [ entry "fn" "a" 0 5 ] })
  in
  check "a result crosses the process boundary"
    (match returning with
    | Isolate.Returned { result; _ } ->
        result.Probe.errors = 2 && List.length result.Probe.outline = 1
    | _ -> false)

let run () =
  failures := 0;
  test_tokens ();
  test_mutations ();
  test_comparison ();
  test_parse ();
  test_isolation ();
  if !failures = 0 then (
    print_endline "selftest: ok";
    true)
  else (
    Printf.printf "selftest: %d failures\n" !failures;
    false)
