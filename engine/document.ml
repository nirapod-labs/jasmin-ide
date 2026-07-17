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
