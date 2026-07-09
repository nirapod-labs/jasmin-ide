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
let non_ascii_in_comment_and_string () =
  let comment = "(* h" ^ e_acute ^ "llo *)" in
  let code = "fn f() = \"" ^ euro ^ "\";" in
  let text = comment ^ "\n" ^ code ^ "\n" in
  let index = Line_index.create text in
  let code_start = String.length comment + 1 in
  check_int "line count" 3 (Line_index.line_count index);
  check_int "line start 1" code_start (Line_index.line_start index 1);
  check_int "line end 0" (String.length comment) (Line_index.line_end index 0);
  check_int "line end 1"
    (code_start + String.length code)
    (Line_index.line_end index 1);
  check_int "line end 2" (String.length text) (Line_index.line_end index 2);
  check_position "the first byte of the accent" (0, 4) (position_of index 4);
  check_position "inside the accent" (0, 4) (position_of index 5);
  check_position "after the accent" (0, 5) (position_of index 6);
  check_int "byte of (0, 5)" 6 (byte_of index 0 5);
  let euro_byte = code_start + 10 in
  check_position "the first byte of the euro sign" (1, 10)
    (position_of index euro_byte);
  check_position "inside the euro sign" (1, 10)
    (position_of index (euro_byte + 1));
  check_position "after the euro sign" (1, 11)
    (position_of index (euro_byte + 3));
  check_int "byte of (1, 11)" (euro_byte + 3) (byte_of index 1 11);
  check_int "units of the code line" 13
    (Line_index.utf16_length index
       (span code_start (Line_index.line_end index 1)));
  check_int "units of the comment line" 11
    (Line_index.utf16_length index (span 0 (String.length comment)))

let span_containment_and_merging () =
  let a = span 0 10 in
  let b = span 2 5 in
  let c = span 8 20 in
  let d = span 10 20 in
  check_bool "contains the first byte" (Span.contains a ~byte:0);
  check_bool "contains the last byte" (Span.contains a ~byte:9);
  check_bool "the end is excluded" (not (Span.contains a ~byte:10));
  check_bool "nothing before the start" (not (Span.contains a ~byte:(-1)));
  check_bool "contains its own start" (Span.contains b ~byte:2);
  check_bool "contains inside" (Span.contains b ~byte:4);
  check_bool "excludes its end" (not (Span.contains b ~byte:5));
  check_bool "an empty span is empty" (Span.is_empty (span 5 5));
  check_bool "a reversed span is empty" (Span.is_empty (span 6 5));
  check_bool "a normal span is not empty" (not (Span.is_empty a));
  check_bool "overlapping spans" (Span.overlaps a c);
  check_bool "overlapping is symmetric" (Span.overlaps c a);
  check_bool "touching spans do not overlap" (not (Span.overlaps a d));
  check_bool "touching spans do not overlap, the other way"
    (not (Span.overlaps d a));
  check_bool "an empty span overlaps nothing"
    (not (Span.overlaps (span 4 4) a));
  check_bool "an empty span inside another overlaps nothing"
    (not (Span.overlaps (span 4 4) (span 0 10)));
  check_bool "a reversed span overlaps nothing"
    (not (Span.overlaps (span 6 5) (span 0 10)));
  check_span "merge across a gap" (span 0 20) (Span.merge a c);
  check_span "merge with a contained span" (span 0 10) (Span.merge a b);
  check_span "merge is symmetric" (span 0 20) (Span.merge c a);
  check_span "shift forward" (span 3 13) (Span.shift a 3);
  check_span "shift backward" (span (-3) 7) (Span.shift a (-3));
  check_span "shift below zero" (span (-5) 0) (Span.shift (span 5 10) (-10));
  check_int "order by start" (-1) (Span.compare a b);
  check_int "order by end" (-1) (Span.compare (span 0 5) (span 0 10));
  check_int "equal spans" 0 (Span.compare a (span 0 10))

let diagnostic_fields () =
  let place = span 3 7 in
  let related = [ (span 10 12, "declared here") ] in
  let diagnostic =
    Diagnostic.make ~code:"unused-variable" ~source:"jasminc" ~related
      ~severity:Diagnostic.Warning place "unused variable"
  in
  check_span "span" place diagnostic.span;
  check_bool "severity" (diagnostic.severity = Diagnostic.Warning);
  check_string "message" "unused variable" diagnostic.message;
  check_bool "code" (diagnostic.code = Some "unused-variable");
  check_string "source" "jasminc" diagnostic.source;
  check_int "related count" 1 (List.length diagnostic.related);
  (match diagnostic.related with
  | [ (place, message) ] ->
    check_span "related span" (span 10 12) place;
    check_string "related message" "declared here" message
  | _ -> fail "related should hold one entry");
  let bare = Diagnostic.make ~severity:Diagnostic.Error place "boom" in
  check_bool "code defaults to none" (bare.code = None);
  check_string "source has a default" "jasmin" bare.source;
  check_int "related defaults to empty" 0 (List.length bare.related);
  check_bool "severities differ" (Diagnostic.Error <> Diagnostic.Warning);
  check_bool "information differs" (Diagnostic.Information <> Diagnostic.Hint);
  let first = Diagnostic.make ~severity:Diagnostic.Error (span 0 1) "a" in
  let second = Diagnostic.make ~severity:Diagnostic.Error (span 0 1) "b" in
  let later = Diagnostic.make ~severity:Diagnostic.Error (span 1 2) "a" in
  check_int "order by message" (-1) (Diagnostic.compare first second);
  check_int "order by start" (-1) (Diagnostic.compare second later);
  let same = Diagnostic.make ~severity:Diagnostic.Hint (span 0 1) "a" in
  check_int "same place and message" 0 (Diagnostic.compare first same)

let file_uri_with_spaces () =
  let uri = Uri.of_string "file:///tmp/a%20b/c" in
  check_path "the decoded path" "/tmp/a b/c" (Uri.path uri);
  check_string "round trip" "file:///tmp/a%20b/c" (Uri.to_string uri);
  check_bool "equal to itself" (Uri.equal uri uri);
  let localhost = Uri.of_string "file://localhost/a%20b" in
  check_path "the localhost path" "/a b" (Uri.path localhost);
  check_bool "a host differs from no host" (not (Uri.equal uri localhost));
  let from_path = Uri.of_path "/tmp/a b/c" in
  check_string "a path is encoded" "file:///tmp/a%20b/c"
    (Uri.to_string from_path);
  check_bool "a path equals its parsed form" (Uri.equal from_path uri);
  check_path "a path is decoded again" "/tmp/a b/c" (Uri.path from_path);
  check_bool "different paths differ"
    (not (Uri.equal (Uri.of_string "file:///x") (Uri.of_string "file:///y")));
  check_bool "an empty authority equals none"
    (Uri.equal (Uri.of_string "file:/x") (Uri.of_string "file:///x"))

let file_uri_with_non_ascii () =
  let uri = Uri.of_string "file:///tmp/%C3%A9" in
  check_path "the decoded path" ("/tmp/" ^ e_acute) (Uri.path uri);
  check_string "round trip" "file:///tmp/%C3%A9" (Uri.to_string uri);
  check_bool "equal to the same escape"
    (Uri.equal uri (Uri.of_string "file:///tmp/%c3%a9"));
  let from_path = Uri.of_path ("/tmp/" ^ e_acute ^ " x") in
  check_string "a path is encoded" "file:///tmp/%C3%A9%20x"
    (Uri.to_string from_path);
  check_path "a path is decoded again" ("/tmp/" ^ e_acute ^ " x")
    (Uri.path from_path);
  check_bool "the scheme is case insensitive"
    (Uri.equal (Uri.of_string "FILE:///a") (Uri.of_string "file:///a"));
  check_bool "the host is case insensitive"
    (Uri.equal
       (Uri.of_string "file://LOCALHOST/a")
       (Uri.of_string "file://localhost/a"))

let file_uri_with_a_bad_escape () =
  let uri = Uri.of_string "file:///tmp/a%zzb" in
  check_path "a bad escape is kept as literal text" "/tmp/a%zzb" (Uri.path uri);
  check_string "a bad escape is encoded on the way out" "file:///tmp/a%25zzb"
    (Uri.to_string uri);
  let short = Uri.of_string "file:///tmp/a%2" in
  check_path "a short escape is kept as literal text" "/tmp/a%2"
    (Uri.path short);
  let trailing = Uri.of_string "file:///tmp/a%" in
  check_path "a trailing percent is kept" "/tmp/a%" (Uri.path trailing);
  let good = Uri.of_string "file:///tmp/a%20b" in
  check_path "a good escape still decodes" "/tmp/a b" (Uri.path good)

let non_file_scheme () =
  let https = Uri.of_string "https://example.com/a" in
  check_no_path "no path for https" (Uri.path https);
  check_string "round trip" "https://example.com/a" (Uri.to_string https);
  let untitled = Uri.of_string "untitled:Untitled-1" in
  check_no_path "no path for an opaque scheme" (Uri.path untitled);
  check_string "round trip for an opaque scheme" "untitled:Untitled-1"
    (Uri.to_string untitled);
  check_no_path "no path for the empty string" (Uri.path (Uri.of_string ""));
  check_string "the empty string round trips" ""
    (Uri.to_string (Uri.of_string ""));
  check_no_path "no path without a scheme" (Uri.path (Uri.of_string "/tmp/a"));
  check_string "a path without a scheme round trips" "/tmp/a"
    (Uri.to_string (Uri.of_string "/tmp/a"));
  check_path "a path without an authority" "/a/b"
    (Uri.path (Uri.of_string "file:/a/b"));
  check_bool "a remote host has no path"
    (Uri.path (Uri.of_string "file://server/share/a") = None)

let last_line_with_trailing_newline () =
  let index = Line_index.create "a\nb\n" in
  check_int "line count" 3 (Line_index.line_count index);
  check_int "line start 2" 4 (Line_index.line_start index 2);
  check_int "line end 2" 4 (Line_index.line_end index 2);
  check_int "line end 1" 3 (Line_index.line_end index 1);
  check_position "byte 4" (2, 0) (position_of index 4);
  check_int "byte of (2, 0)" 4 (byte_of index 2 0);
  check_int "a line past the index" 4 (Line_index.line_start index 99);
  check_int "a line before the index" 1 (Line_index.line_end index (-1))

let last_line_without_trailing_newline () =
  let index = Line_index.create "a\nb" in
  check_int "line count" 2 (Line_index.line_count index);
  check_int "line start 1" 2 (Line_index.line_start index 1);
  check_int "line end 1" 3 (Line_index.line_end index 1);
  check_int "line end 0" 1 (Line_index.line_end index 0);
  check_position "byte 3" (1, 1) (position_of index 3);
  check_int "byte of (1, 1)" 3 (byte_of index 1 1);
  check_int "byte past the last line" 3 (byte_of index 1 9)

let text_measurements () =
  check_bool "ascii is utf8" (Text.is_utf8 "abc");
  check_bool "the empty string is utf8" (Text.is_utf8 "");
  check_bool "mixed scripts are utf8"
    (Text.is_utf8 ("a" ^ e_acute ^ euro ^ astral));
  check_bool "a lone continuation byte is not utf8" (not (Text.is_utf8 "\x80"));
  check_bool "an overlong form is not utf8" (not (Text.is_utf8 "\xC0\x80"));
  check_bool "a truncated sequence is not utf8"
    (not (Text.is_utf8 ("a" ^ "\xE2\x82")));
  check_bool "a surrogate is not utf8" (not (Text.is_utf8 "\xED\xA0\x80"));
  check_bool "past the last code point is not utf8"
    (not (Text.is_utf8 "\xF5\x80\x80\x80"));
  check_bool "a mark is utf8" (Text.is_utf8 bom);
  check_int "byte length" 5
    (Text.utf8_byte_length ("a" ^ e_acute ^ euro) 0 5);
  check_int "byte length clamps the end" 6
    (Text.utf8_byte_length ("a" ^ e_acute ^ euro) 0 99);
  check_int "byte length clamps the start" 0
    (Text.utf8_byte_length "abc" 9 4);
  check_int "byte length of an empty range" 0
    (Text.utf8_byte_length "abc" 2 2)

(* Every byte maps to a position that maps back to the start of its
   character, and that position maps to itself again. *)
let position_round_trip () =
  let text =
    "fn f() = (* " ^ e_acute ^ " " ^ astral ^ " *)\r\nlet x = \"" ^ euro
    ^ "\";\n"
  in
  let index = Line_index.create text in
  for byte = 0 to String.length text do
    let place = position_of index byte in
    let back = byte_of index place.line place.character in
    check_bool
      (Printf.sprintf "byte %d maps back before itself" byte)
      (back <= byte);
    check_bool
      (Printf.sprintf "byte %d maps to the same position" byte)
      (Position.compare (position_of index back) place = 0)
  done;
  let last = position_of index (String.length text) in
  check_bool "the end of the text is on the last line"
    (last.line = Line_index.line_count index - 1)

let () =
  test "ASCII positions" ascii_positions;
  test "UTF-16 astral character" utf16_astral_character;
  test "CRLF" crlf;
  test "lone CR" lone_cr;
  test "empty string" empty_string;
  test "empty file with one newline" empty_file_with_one_newline;
  test "byte inside a multi-byte character" byte_inside_multibyte_character;
  test "byte-order mark" byte_order_mark;
  test "non-ASCII in a comment and in a string" non_ascii_in_comment_and_string;
  test "span containment and merging" span_containment_and_merging;
  test "diagnostic fields" diagnostic_fields;
  test "file URI with spaces" file_uri_with_spaces;
  test "file URI with non-ASCII" file_uri_with_non_ascii;
  test "file URI with a bad escape" file_uri_with_a_bad_escape;
  test "non-file scheme" non_file_scheme;
  test "line_start and line_end at the last line with a trailing newline"
    last_line_with_trailing_newline;
  test "line_start and line_end at the last line without a trailing newline"
    last_line_without_trailing_newline;
  test "text measurements" text_measurements;
  test "position round trip" position_round_trip;
  if !failures > 0 then begin
    Printf.printf "\n%d tests, %d failed checks\n" !tests !failures;
    exit 1
  end
  else Printf.printf "\n%d tests, no failed checks\n" !tests
