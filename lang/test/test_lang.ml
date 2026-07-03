(* Tests for the language library. Each case prints its name, so the output
   of a run doubles as the list of what was checked. *)

open Jide_lang

(* LATIN SMALL LETTER E WITH ACUTE: two bytes of UTF-8, one code unit. *)
let e_acute = "\xC3\xA9"

(* EURO SIGN: three bytes of UTF-8, one code unit. *)
let euro = "\xE2\x82\xAC"

(* MATHEMATICAL DOUBLE-STRUCK CAPITAL A: four bytes of UTF-8, and a surrogate
   pair, so two code units, in UTF-16. *)
let astral = "\xF0\x9D\x94\xB8"

(* A UTF-8 byte-order mark. *)
let bom = "\xEF\xBB\xBF"

let failures = ref 0
let tests = ref 0
let current = ref ""

let fail format =
  Printf.ksprintf
    (fun message ->
      incr failures;
      Printf.printf "FAIL %s: %s\n" !current message)
    format

let check_bool label condition =
  if not condition then fail "%s: the condition does not hold" label

let check_int label expected actual =
  if expected <> actual then
    fail "%s: expected %d, got %d" label expected actual

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail "%s: expected %S, got %S" label expected actual

let span start_byte end_byte = Span.make ~start_byte ~end_byte
let position line character = Position.make ~line ~character

let check_span label (expected : Span.t) (actual : Span.t) =
  check_int (label ^ ", start") expected.start_byte actual.start_byte;
  check_int (label ^ ", end") expected.end_byte actual.end_byte

let check_position label (expected_line, expected_character)
    (actual : Position.t) =
  check_int (label ^ ", line") expected_line actual.line;
  check_int (label ^ ", character") expected_character actual.character

let check_path label expected = function
  | None -> fail "%s: expected the path %S, got None" label expected
  | Some actual -> check_string label expected actual

let check_no_path label = function
  | None -> ()
  | Some actual -> fail "%s: expected no path, got %S" label actual

let test name body =
  current := name;
  incr tests;
  let before = !failures in
  (try body () with exn -> fail "raised %s" (Printexc.to_string exn));
  if !failures = before then Printf.printf "ok   %s\n" name

let position_of index byte = Line_index.position_of_byte index byte

let byte_of index line character =
  Line_index.byte_of_position index (position line character)

(* One line, three lines, and the clamps at both ends. *)
let ascii_positions () =
  let index = Line_index.create "abc\ndefgh\n" in
  check_int "line count" 3 (Line_index.line_count index);
  check_int "line start 0" 0 (Line_index.line_start index 0);
  check_int "line start 1" 4 (Line_index.line_start index 1);
  check_int "line start 2" 10 (Line_index.line_start index 2);
  check_int "line end 0" 3 (Line_index.line_end index 0);
  check_int "line end 1" 9 (Line_index.line_end index 1);
  check_int "line end 2" 10 (Line_index.line_end index 2);
  check_position "byte 0" (0, 0) (position_of index 0);
  check_position "byte 2" (0, 2) (position_of index 2);
  check_position "the line feed" (0, 3) (position_of index 3);
  check_position "byte 4" (1, 0) (position_of index 4);
  check_position "byte 8" (1, 4) (position_of index 8);
  check_position "byte 9" (1, 5) (position_of index 9);
  check_position "byte 10" (2, 0) (position_of index 10);
  check_position "before the text" (0, 0) (position_of index (-3));
  check_position "past the text" (2, 0) (position_of index 40);
  check_int "byte of (0, 0)" 0 (byte_of index 0 0);
  check_int "byte of (0, 3)" 3 (byte_of index 0 3);
  check_int "byte of (1, 2)" 6 (byte_of index 1 2);
  check_int "byte of (1, 5)" 9 (byte_of index 1 5);
  check_int "byte of (2, 0)" 10 (byte_of index 2 0);
  check_int "line past the index" 10 (byte_of index 7 0);
  check_int "character past the line" 3 (byte_of index 0 100);
  check_int "units of the text" 10 (Line_index.utf16_length index (span 0 10));
  check_int "units of a line" 5 (Line_index.utf16_length index (span 4 9));
  check_int "units of nothing" 0 (Line_index.utf16_length index (span 4 4));
  check_int "units with a negative start" 3
    (Line_index.utf16_length index (span (-5) 3));
  check_bool "position order"
    (Position.compare (position 0 9) (position 1 0) < 0)

(* The astral character is two code units, so everything after it shifts. *)
let utf16_astral_character () =
  let index = Line_index.create (astral ^ "x") in
  check_int "line count" 1 (Line_index.line_count index);
  check_position "byte 0" (0, 0) (position_of index 0);
  check_position "inside the character" (0, 0) (position_of index 1);
  check_position "byte 4" (0, 2) (position_of index 4);
  check_position "byte 5" (0, 3) (position_of index 5);
  check_int "byte of the first unit" 0 (byte_of index 0 0);
  check_int "byte inside the pair" 0 (byte_of index 0 1);
  check_int "byte of the second unit" 4 (byte_of index 0 2);
  check_int "byte past the character" 5 (byte_of index 0 3);
  check_int "character past the end" 5 (byte_of index 0 4);
  check_int "units of the character" 2
    (Line_index.utf16_length index (span 0 4));
  check_int "units of the line" 3 (Line_index.utf16_length index (span 0 5))

(* A carriage return before a line feed belongs to the terminator. *)
let crlf () =
  let index = Line_index.create "a\r\nb\r\n" in
  check_int "line count" 3 (Line_index.line_count index);
  check_int "line start 1" 3 (Line_index.line_start index 1);
  check_int "line start 2" 6 (Line_index.line_start index 2);
  check_int "line end 0" 1 (Line_index.line_end index 0);
  check_int "line end 1" 4 (Line_index.line_end index 1);
  check_int "line end 2" 6 (Line_index.line_end index 2);
  check_position "the carriage return" (0, 1) (position_of index 1);
  check_position "the line feed" (0, 1) (position_of index 2);
  check_position "the next line" (1, 0) (position_of index 3);
  check_int "byte of (0, 1)" 1 (byte_of index 0 1);
  check_int "byte past the content" 1 (byte_of index 0 2);
  check_int "byte of (1, 0)" 3 (byte_of index 1 0);
  check_int "units of the terminator" 1
    (Line_index.utf16_length index (span 1 2))

(* A carriage return on its own is content. *)
let lone_cr () =
  let index = Line_index.create "a\rb" in
  check_int "line count" 1 (Line_index.line_count index);
  check_int "line start" 0 (Line_index.line_start index 0);
  check_int "line end" 3 (Line_index.line_end index 0);
  check_position "the carriage return" (0, 1) (position_of index 1);
  check_position "byte 2" (0, 2) (position_of index 2);
  check_int "byte of (0, 2)" 2 (byte_of index 0 2)

let empty_string () =
  let index = Line_index.create "" in
  check_int "line count" 1 (Line_index.line_count index);
  check_int "line start" 0 (Line_index.line_start index 0);
  check_int "line end" 0 (Line_index.line_end index 0);
  check_position "byte 0" (0, 0) (position_of index 0);
  check_position "byte 9" (0, 0) (position_of index 9);
  check_int "byte of (0, 0)" 0 (byte_of index 0 0);
  check_int "byte of (0, 4)" 0 (byte_of index 0 4);
  check_int "byte of line 3" 0 (byte_of index 3 7);
  check_int "units" 0 (Line_index.utf16_length index (span 0 0));
  check_int "units past the text" 0 (Line_index.utf16_length index (span 4 9))

(* One line feed makes a second, empty line. *)
let empty_file_with_one_newline () =
  let index = Line_index.create "\n" in
  check_int "line count" 2 (Line_index.line_count index);
  check_int "line start 0" 0 (Line_index.line_start index 0);
  check_int "line end 0" 0 (Line_index.line_end index 0);
  check_int "line start 1" 1 (Line_index.line_start index 1);
  check_int "line end 1" 1 (Line_index.line_end index 1);
  check_position "byte 0" (0, 0) (position_of index 0);
  check_position "byte 1" (1, 0) (position_of index 1);
  let crlf = Line_index.create "\r\n" in
  check_int "crlf line count" 2 (Line_index.line_count crlf);
  check_int "crlf line end 0" 0 (Line_index.line_end crlf 0);
  check_int "crlf line start 1" 2 (Line_index.line_start crlf 1)

(* Both characters are wider than one byte, so byte and unit offsets differ. *)
let byte_inside_multibyte_character () =
  let index = Line_index.create (e_acute ^ euro) in
  check_position "byte 0" (0, 0) (position_of index 0);
  check_position "inside the two-byte character" (0, 0) (position_of index 1);
  check_position "byte 2" (0, 1) (position_of index 2);
  check_position "inside the three-byte character" (0, 1) (position_of index 3);
  check_position "byte 4" (0, 1) (position_of index 4);
  check_position "byte 5" (0, 2) (position_of index 5);
  check_int "byte of (0, 0)" 0 (byte_of index 0 0);
  check_int "byte of (0, 1)" 2 (byte_of index 0 1);
  check_int "byte of (0, 2)" 5 (byte_of index 0 2);
  check_int "units of both characters" 2
    (Line_index.utf16_length index (span 0 5));
  check_int "units of the two-byte character" 1
    (Line_index.utf16_length index (span 0 2));
  check_int "a character cut in half is not counted" 0
    (Line_index.utf16_length index (span 0 1));
  check_int "a lone continuation byte counts once" 1
    (Line_index.utf16_length index (span 1 2))

(* The mark is removed by the reader and is one character to the index. *)
let byte_order_mark () =
  check_string "removes the mark" "fn f() = 0"
    (Text.strip_bom (bom ^ "fn f() = 0"));
  check_string "leaves other text" "abc" (Text.strip_bom "abc");
  check_string "a mark alone" "" (Text.strip_bom bom);
  check_string "removes one mark" bom (Text.strip_bom (bom ^ bom));
  let index = Line_index.create (bom ^ "fn") in
  check_int "line count" 1 (Line_index.line_count index);
  check_position "byte 0" (0, 0) (position_of index 0);
  check_position "after the mark" (0, 1) (position_of index 3);
  check_position "the end" (0, 3) (position_of index 5);
  check_int "byte of (0, 0)" 0 (byte_of index 0 0);
  check_int "byte of (0, 1)" 3 (byte_of index 0 1);
  check_int "byte of (0, 3)" 5 (byte_of index 0 3);
  let stripped = Line_index.create (Text.strip_bom (bom ^ "fn")) in
  check_position "after stripping, byte 2" (0, 2) (position_of stripped 2);
  check_int "after stripping, byte of (0, 2)" 2 (byte_of stripped 0 2)

(* An accented comment and a string with a euro sign, as a file would have. *)
