(** The verdict of a check, as the checker prints it. *)

open Jide_lang

let internal message =
  Diagnostic.make ~code:"internal" ~severity:Diagnostic.Error
    (Span.make ~start_byte:0 ~end_byte:0)
    message

(** A warning's span. The compiler prints a warning with a line number
    starting at 1 and byte columns counted from the start of that line, so
    the line index gives the line's first byte and the columns offset from
    it. Line 0 means the printer wrote no location. *)
let span_of_warning ~(text : string) (warning : Jide_jasmin.Warnings.t) =
  if warning.line <= 0 then Span.make ~start_byte:0 ~end_byte:0
  else
    let index = Line_index.create text in
    let line = warning.line - 1 in
    let start = Line_index.line_start index line in
    let limit = Line_index.line_end index line - start in
    let clamp column = start + min (max column 0) limit in
    Span.make
      ~start_byte:(clamp warning.start_column)
      ~end_byte:(clamp warning.end_column)

let diagnostic_of_warning ~text (warning : Jide_jasmin.Warnings.t) =
  Diagnostic.make ~code:"warning" ~severity:Diagnostic.Warning
    (span_of_warning ~text warning)
    warning.message

let rank (diagnostic : Diagnostic.t) =
  match diagnostic.severity with Diagnostic.Error -> 0 | _ -> 1

let compare a b =
  match Int.compare (rank a) (rank b) with
  | 0 -> Diagnostic.compare a b
  | order -> order

let of_check ~file ~text (result : Jide_jasmin.Adapter.check_result) =
  let warnings =
    List.filter
      (fun (warning : Jide_jasmin.Warnings.t) -> String.equal warning.file file)
      result.warnings
  in
  let diagnostics =
    result.diagnostics @ List.map (diagnostic_of_warning ~text) warnings
  in
  List.stable_sort compare diagnostics

let line_column ~(text : string) (span : Span.t) =
  let index = Line_index.create text in
  let byte = min (max span.Span.start_byte 0) (String.length text) in
  let position = Line_index.position_of_byte index byte in
  ( position.Position.line + 1,
    byte - Line_index.line_start index position.line + 1 )

let kind (diagnostic : Diagnostic.t) =
  match diagnostic.code with
  | Some code -> code
  | None -> (
      match diagnostic.severity with
      | Diagnostic.Warning -> "warning"
      | Diagnostic.Error | Diagnostic.Information | Diagnostic.Hint ->
          "internal")

let escape message =
  let buffer = Buffer.create (String.length message) in
  String.iter
    (fun character ->
      match character with
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | character -> Buffer.add_char buffer character)
    message;
  Buffer.contents buffer

let line ~file ~text (diagnostic : Diagnostic.t) =
  let line, column = line_column ~text diagnostic.span in
  Printf.sprintf "%s:%d:%d: %s: %s" file line column (kind diagnostic)
    (escape diagnostic.message)

let lines ~file ~text diagnostics = List.map (line ~file ~text) diagnostics

let summary ~file diagnostics =
  let errors =
    List.length
      (List.filter
         (fun (diagnostic : Diagnostic.t) ->
           diagnostic.severity = Diagnostic.Error)
         diagnostics)
  in
  match errors with
  | 0 -> file ^ ": ok"
  | 1 -> file ^ ": 1 error"
  | count -> Printf.sprintf "%s: %d errors" file count
