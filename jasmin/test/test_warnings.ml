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
