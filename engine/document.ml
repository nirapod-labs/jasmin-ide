type t = {
  uri : string;
  path : string option;
  version : int;
  text : string;
  lines : Jide_lang.Line_index.t;
}

let create ~uri ~version ~text =
  let path = Jide_lang.Uri.path (Jide_lang.Uri.of_string uri) in
  { uri; path; version; text; lines = Jide_lang.Line_index.create text }

let with_text t ~version text =
  { t with version; text; lines = Jide_lang.Line_index.create text }

(* The largest file the reader accepts. A larger file would not be analysed,
   so it is refused rather than read and then discarded. *)
let max_file_bytes = 8 * 1024 * 1024

(* A regular file no larger than [max_file_bytes]. A directory, a FIFO or a
   device is refused before anything is read: reading a device either fails
   or never ends, and a request must not wait on one. *)
let regular_file path =
  try
    let stats = Unix.stat path in
    match stats.Unix.st_kind with
    | Unix.S_REG when stats.Unix.st_size <= max_file_bytes ->
        Ok stats.Unix.st_size
    | Unix.S_REG ->
        Error
          (Printf.sprintf "%s is %d bytes, over the limit of %d" path
             stats.Unix.st_size max_file_bytes)
    | _ -> Error (Printf.sprintf "%s is not a regular file" path)
  with Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)

let readable path =
  match regular_file path with Ok _ -> true | Error _ -> false

(* One reader for both the file loader and the engine. The bytes are kept
   as they are: stripping a byte-order mark belongs to the analysis, which
   is the step that knows the lexer rejects it. *)
let read_file path =
  match regular_file path with
  | Error message -> Error message
  | Ok _ -> (
      try Ok (In_channel.with_open_bin path In_channel.input_all)
      with Sys_error message -> Error message)

let of_file ~uri path =
  match read_file path with
  | Error message -> Error message
  | Ok text ->
      Ok
        {
          uri;
          path = Some path;
          version = 0;
          text;
          lines = Jide_lang.Line_index.create text;
        }

let uri t = t.uri
let path t = t.path
let version t = t.version
let text t = t.text
let lines t = t.lines

let byte_of_position t position =
  Jide_lang.Line_index.byte_of_position t.lines position

let position_of_byte t byte = Jide_lang.Line_index.position_of_byte t.lines byte

let span_of_bytes t ~start_byte ~end_byte =
  let length = String.length t.text in
  let start_byte = max 0 (min start_byte length) in
  let end_byte = max start_byte (min end_byte length) in
  Jide_lang.Span.make ~start_byte ~end_byte

let line_text t line =
  let start = Jide_lang.Line_index.line_start t.lines line in
  let stop = Jide_lang.Line_index.line_end t.lines line in
  if start < 0 || stop < start || stop > String.length t.text then ""
  else String.sub t.text start (stop - start)
