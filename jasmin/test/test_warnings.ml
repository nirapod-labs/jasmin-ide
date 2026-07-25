(* Tests for warnings as diagnostics: the byte span the printed line and
   columns give, a dummy location, a message that carries "warning:", a
   stacked location, a duplicate, a warning from another file, the
   single-line form and the clamping of a place outside the text.

   The fixtures are what the compiler prints, because that is what the
   adapter parses back: the location, the entries of its stack, then the
   message. The checked text is three lines that start at bytes 0, 4 and 10,
   so the expected span of a fixture reads off by hand. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let file = "probe.jazz"

(* Line 0 starts at 0, line 1 at 4 and line 2 at 10. *)
let text = "aaa\nbbbbb\ncc\n"

let convert printed =
  Jide_jasmin.Warnings.to_diagnostics ~file ~text
    (Jide_jasmin.Warnings.parse ~file printed)

let range start_byte end_byte = Jide_lang.Span.make ~start_byte ~end_byte

let span_of (placed : Jide_jasmin.Warnings.placed) =
  placed.diagnostic.Jide_lang.Diagnostic.span

let message_of (placed : Jide_jasmin.Warnings.placed) =
  placed.diagnostic.Jide_lang.Diagnostic.message

let count places = string_of_int (List.length places)

(* The one warning of a fixture. When there is not exactly one, the shape is
   reported and the checks after it run against an empty warning, so every
   one of them reports what it saw instead of raising. *)
let unique name places =
  match places with
  | [ placed ] -> placed
  | other ->
      check (name ^ ": one warning, got " ^ count other) false;
      {
        Jide_jasmin.Warnings.file = "";
        diagnostic =
          Jide_lang.Diagnostic.make ~severity:Jide_lang.Diagnostic.Warning
            (range 0 0) "";
      }

(* A check that printed nothing has nothing to report. *)
let test_empty () = check "empty: no warnings, no diagnostics" (convert "" = [])

(* A warning the printer wrote on two lines: the location first, then the
   message. The span is the first byte of the warning's line plus each
   printed column, and the message has lost its unique-id suffix. *)
let test_two_line_span () =
  let placed =
    unique "two line"
      (convert "\"probe.jazz\", line 2 (1-3)\nwarning: unused variable x.7\n")
  in
  check "two line: the file is the checked one" (placed.file = file);
  check "two line: severity"
    (placed.diagnostic.Jide_lang.Diagnostic.severity
   = Jide_lang.Diagnostic.Warning);
  check "two line: code"
    (placed.diagnostic.Jide_lang.Diagnostic.code = Some "warning");
  check "two line: source"
    (placed.diagnostic.Jide_lang.Diagnostic.source = "jasmin");
  check "two line: the span is the line's first byte plus the columns"
    (span_of placed = range 5 7);
  check "two line: the unique-id suffix is stripped"
    (message_of placed = "unused variable x")

(* A warning the printer wrote without a location: the message is all there
   is, and it belongs to the document being checked. *)
let test_dummy_location () =
  let placed = unique "dummy" (convert "warning: deprecated cast\n") in
  check "dummy: the message is kept" (message_of placed = "deprecated cast");
  check "dummy: the file is the checked one" (placed.file = file);
  check "dummy: the span is empty" (span_of placed = range 0 0);
  (* Line 0 is what makes a location dummy, whatever columns come with it:
     the printer writes none, but the rule is the line. *)
  let handmade =
    {
      Jide_jasmin.Warnings.file;
      line = 0;
      start_column = 2;
      end_column = 5;
      message = "handmade";
    }
  in
  let converted =
    Jide_jasmin.Warnings.to_diagnostics ~file ~text [ handmade ]
  in
  check "dummy: line 0 has no span even with columns on it"
    (match converted with
    | [ placed ] -> span_of placed = range 0 0 && message_of placed = "handmade"
    | _ -> false)

(* The message itself contains "warning:" and quotes a location. Only the
   first "warning: " on the line starts the message, and a quoted location
   inside a message supplies none. *)
let test_message_with_warning () =
  let placed =
    unique "message"
      (convert
         "\"probe.jazz\", line 2 (1-3)\n\
          warning: replaced warning: see \"/tmp/o.jazz\", line 77 (3-4)\n")
  in
  check "message: the whole message is kept"
    (message_of placed = "replaced warning: see \"/tmp/o.jazz\", line 77 (3-4)");
  check "message: the location does not move" (span_of placed = range 5 7);
  check "message: it stays on the checked file" (placed.file = file)

(* A stacked location prints its entries under the first one; the warning is
   about the first, which is the one that gives the place. *)
let test_stacked_location () =
  let placed =
    unique "stacked"
      (convert
         "\"probe.jazz\", line 2 (1-3)\n\
          from \"/tmp/other.jazz\", line 9 (0-2)\n\
          warning: unused variable x.7\n")
  in
  check "stacked: the first entry gives the file" (placed.file = file);
  check "stacked: the first entry gives the span" (span_of placed = range 5 7);
  check "stacked: the message" (message_of placed = "unused variable x")

(* A repeated item prints its warning twice; the pair is kept once. A
   different message at the same place is another warning. *)
let test_duplicate_suppressed () =
  let block = "\"probe.jazz\", line 2 (1-3)\nwarning: unused variable x.7\n" in
  let placed = unique "duplicate" (convert (block ^ block)) in
  check "duplicate: the first one is kept"
    (message_of placed = "unused variable x");
  check "duplicate: with its span" (span_of placed = range 5 7);
  check "duplicate: a different message at the same span is kept"
    (List.length
       (convert
          (block
         ^ "\"probe.jazz\", line 2 (1-3)\nwarning: unused variable y.8\n"))
    = 2)

(* A warning for a file the check did not ask about keeps that file, so the
   caller can drop it, and has no span in the checked text. *)
let test_other_file () =
  let placed =
    unique "other file"
      (convert "\"other.jazz\", line 2 (1-3)\nwarning: unused variable y.8\n")
  in
  check "other file: the file it names is kept" (placed.file = "other.jazz");
  check "other file: the message is kept"
    (message_of placed = "unused variable y");
  check "other file: the span is empty" (span_of placed = range 0 0);
  (* Both have the empty span and the same message; the file tells them
     apart, so the foreign one cannot suppress this document's. *)
  match
    convert
      "warning: deprecated cast\n\
       \"other.jazz\", line 2 (1-3)\n\
       warning: deprecated cast\n"
  with
  | [ mine; theirs ] ->
      check "other file: both are kept"
        (mine.file = file && theirs.file = "other.jazz")
  | other -> check ("other file: both kept, got " ^ count other) false

(* The printer writes the single-line form too: location, then the message
   after ": warning: ". *)
let test_single_line () =
  let placed =
    unique "single line"
      (convert "\"probe.jazz\", line 2 (1-3): warning: unused variable x.7\n")
  in
  check "single line: the span" (span_of placed = range 5 7);
  check "single line: the message" (message_of placed = "unused variable x");
  check "single line: the file" (placed.file = file)

(* A place outside the text is clamped rather than followed: a column past
   the end of its line stops at the line's end, and a line past the last one
   lands on the empty line a final line feed leaves. *)
let test_clamped () =
  let past_column =
    unique "clamped column"
      (convert "\"probe.jazz\", line 3 (0-99)\nwarning: past the column\n")
  in
  check "clamped column: the span stops at the line's end"
    (span_of past_column = range 10 12);
  check "clamped column: the message"
    (message_of past_column = "past the column");
  let past_line =
    unique "clamped line"
      (convert "\"probe.jazz\", line 99 (0-99)\nwarning: past the line\n")
  in
  check "clamped line: the span stays inside the text"
    (span_of past_line = range 13 13);
  check "clamped line: the message" (message_of past_line = "past the line")

(* A real check, not a fixture: the warning the compiler prints for an
   unused variable lands on the bytes of the name, which is what an editor
   underlines. *)
let test_from_a_check () =
  let source =
    "fn f(reg u32 a) -> reg u32 {\n  reg u32 unused;\n  return a;\n}\n"
  in
  let offset_of needle =
    let length = String.length source and width = String.length needle in
    let rec loop index =
      if index + width > length then -1
      else if String.sub source index width = needle then index
      else loop (index + 1)
    in
    loop 0
  in
  let result =
    Jide_jasmin.Adapter.check
      (Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64"))
      (Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux"))
      ~file ~text:source ~idirs:[]
  in
  match
    Jide_jasmin.Warnings.to_diagnostics ~file ~text:source result.warnings
  with
  | [ placed ] ->
      let start = offset_of "unused" in
      check "check: the warning is on the checked file" (placed.file = file);
      check "check: it names the variable"
        (message_of placed = "unused variable unused");
      check "check: the span covers the name"
        (span_of placed = range start (start + String.length "unused"))
  | other -> check ("check: one warning, got " ^ count other) false

let () =
  test_empty ();
  test_two_line_span ();
  test_dummy_location ();
  test_message_with_warning ();
  test_stacked_location ();
  test_duplicate_suppressed ();
  test_other_file ();
  test_single_line ();
  test_clamped ();
  test_from_a_check ();
  if !failures = 0 then print_endline "all warning tests passed"
  else (
    Printf.printf "%d warning test(s) failed\n" !failures;
    exit 1)
