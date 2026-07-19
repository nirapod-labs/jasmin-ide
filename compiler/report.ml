(** The verdict of a check, as the checker prints it. *)

open Jide_lang

let internal message =
  Diagnostic.make ~code:"internal" ~severity:Diagnostic.Error
    (Span.make ~start_byte:0 ~end_byte:0)
    message

(** A warning's span, from the line and byte columns the compiler prints.
    The conversion lives in the engine, so the checker and the server report
    one warning the same way. *)
let of_check ~file ~text (result : Jide_jasmin.Adapter.check_result) =
  Jide_engine.Engine.diagnostics_of_check ~file ~text result

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
