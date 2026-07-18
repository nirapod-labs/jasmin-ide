(* Framed JSON-RPC messages: "Content-Length: N\r\n\r\n" followed by exactly
   N bytes of UTF-8. The reader reads exactly N bytes, never a line more. *)

type error = Timeout | End_of_input | Malformed of string

let error_message = function
  | Timeout -> "timeout"
  | End_of_input -> "end of input"
  | Malformed reason -> "malformed frame: " ^ reason

(* A pollable byte source: [fill] reads up to [length] bytes and returns how
   many it read, 0 at end of input; [wait_readable] waits up to [ms]
   milliseconds for a read to make progress and reports whether it did. *)
type input = { fill : Bytes.t -> int -> int; wait_readable : int -> bool }

let now = Unix.gettimeofday

let ms_until deadline =
  max 0 (int_of_float (ceil ((deadline -. now ()) *. 1000.)))

let string_input (text : string) : input =
  let position = ref 0 in
  let fill bytes length =
    let remaining = String.length text - !position in
    if remaining <= 0 then 0
    else
      let take = min length remaining in
      Bytes.blit_string text !position bytes 0 take;
      position := !position + take;
      take
  in
  { fill; wait_readable = (fun _ -> true) }

let fd_input fd : input =
  let rec retry attempt =
    try attempt () with Unix.Unix_error (Unix.EINTR, _, _) -> retry attempt
  in
  let fill bytes length = retry (fun () -> Unix.read fd bytes 0 length) in
  let wait_readable ms =
    let ready, _, _ =
      retry (fun () -> Unix.select [ fd ] [] [] (float_of_int ms /. 1000.))
    in
    ready <> []
  in
  { fill; wait_readable }

type reader = {
  input : input;
  bytes : Bytes.t;
  mutable length : int;
  mutable offset : int;
}

let make_reader input =
  { input; bytes = Bytes.create 4096; length = 0; offset = 0 }

(* Buffer at least one more byte, or report why that is impossible. *)
let refill reader ~deadline =
  if reader.offset < reader.length then Ok ()
  else (
    reader.offset <- 0;
    reader.length <- 0;
    if not (reader.input.wait_readable (ms_until deadline)) then Error Timeout
    else
      match reader.input.fill reader.bytes (Bytes.length reader.bytes) with
      | 0 -> Error End_of_input
      | filled ->
          reader.length <- filled;
          Ok ())

(* One header line without its CRLF. An input that ends before a line starts
   has ended; a line cut short is a malformed frame. *)
let read_line reader ~deadline =
  let line = Buffer.create 32 in
  let rec loop () =
    match refill reader ~deadline with
    | Error End_of_input when Buffer.length line = 0 -> Error End_of_input
    | Error End_of_input ->
        Error (Malformed "the header ends before its line feed")
    | Error error -> Error error
    | Ok () ->
        let character = Bytes.get reader.bytes reader.offset in
        reader.offset <- reader.offset + 1;
        if character = '\n' then
          let text = Buffer.contents line in
          let length = String.length text in
          Ok
            (if length > 0 && text.[length - 1] = '\r' then
               String.sub text 0 (length - 1)
             else text)
        else if Buffer.length line >= 8192 then
          Error (Malformed "header line longer than 8192 bytes")
        else (
          Buffer.add_char line character;
          loop ())
  in
  loop ()

(* The body of a frame whose header arrived in full. An input that ends
   inside the body is the server leaving before it replied, not a malformed
   frame: the header said how much was coming and the peer stopped. *)
let read_exact reader ~deadline count =
  let body = Bytes.create count in
  let rec loop filled =
    if filled = count then Ok (Bytes.to_string body)
    else
      match refill reader ~deadline with
      | Error End_of_input -> Error End_of_input
      | Error error -> Error error
      | Ok () ->
          let take = min (count - filled) (reader.length - reader.offset) in
          Bytes.blit reader.bytes reader.offset body filled take;
          reader.offset <- reader.offset + take;
          loop (filled + take)
  in
  loop 0

(* A body larger than this is refused before it is read, so a broken or
   hostile server cannot make the client allocate an arbitrary buffer. *)
let max_body_bytes = 8 * 1024 * 1024

(* "Content-Length: 12" gives [Ok (Some 12)]; any other header [Ok None]. *)
let content_length line =
  match String.index_opt line ':' with
  | None -> Error (Malformed (Printf.sprintf "expected a header, got %S" line))
  | Some colon -> (
      let name = String.trim (String.sub line 0 colon) in
      let value =
        String.trim
          (String.sub line (colon + 1) (String.length line - colon - 1))
      in
      if String.lowercase_ascii name <> "content-length" then Ok None
      else
        match int_of_string_opt value with
        | Some length when length >= 0 && length <= max_body_bytes ->
            Ok (Some length)
        | Some _ | None ->
            Error (Malformed (Printf.sprintf "bad Content-Length %S" value)))

let read_message reader ~timeout_ms =
  let deadline = now () +. (float_of_int timeout_ms /. 1000.) in
  let rec headers started length =
    match read_line reader ~deadline with
    | Error End_of_input when not started -> Error End_of_input
    | Error End_of_input ->
        Error (Malformed "the header ends before its blank line")
    | Error error -> Error error
    | Ok "" -> (
        match length with
        | None -> Error (Malformed "no Content-Length header")
        | Some count -> read_exact reader ~deadline count)
    | Ok header -> (
        match content_length header with
        | Error error -> Error error
        | Ok None -> headers true length
        | Ok (Some count) -> headers true (Some count))
  in
  headers false None

let encode body =
  Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) ^ body

let write channel body =
  output_string channel (encode body);
  flush channel
