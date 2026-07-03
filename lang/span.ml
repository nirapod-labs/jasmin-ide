type t = { start_byte : int; end_byte : int }

let make ~start_byte ~end_byte = { start_byte; end_byte }

let is_empty span = span.end_byte <= span.start_byte

let compare a b =
  let by_start = Int.compare a.start_byte b.start_byte in
  if by_start <> 0 then by_start else Int.compare a.end_byte b.end_byte

let contains span ~byte = byte >= span.start_byte && byte < span.end_byte

let overlaps a b = a.start_byte < b.end_byte && b.start_byte < a.end_byte

let merge a b =
  {
    start_byte = min a.start_byte b.start_byte;
    end_byte = max a.end_byte b.end_byte;
  }

let shift span delta =
  { start_byte = span.start_byte + delta; end_byte = span.end_byte + delta }
