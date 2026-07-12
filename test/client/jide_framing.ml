(* Framed JSON-RPC messages: "Content-Length: N\r\n\r\n" followed by exactly
   N bytes of UTF-8. The reader reads exactly N bytes, never a line more. *)

type error =
  | Timeout
  | End_of_input
  | Malformed of string

let error_message = function
  | Timeout -> "timeout"
  | End_of_input -> "end of input"
  | Malformed reason -> "malformed frame: " ^ reason

(* A pollable byte source: [fill] reads up to [length] bytes and returns how
   many it read, 0 at end of input; [wait_readable] waits up to [ms]
   milliseconds for a read to make progress and reports whether it did. *)
type input = {
  fill : Bytes.t -> int -> int;
  wait_readable : int -> bool;
}

let now = Unix.gettimeofday
let ms_until deadline = max 0 (int_of_float (ceil ((deadline -. now ()) *. 1000.)))

let string_input (text : string) : input =
  let position = ref 0 in
  let fill bytes length =
    let remaining = String.length text - !position in
    if remaining <= 0 then 0
    else begin
      let take = min length remaining in
      Bytes.blit_string text !position bytes 0 take;
      position := !position + take;
      take
    end
  in
  { fill; wait_readable = (fun _ -> true) }

let fd_input fd : input =
  let rec retry attempt =
    try attempt () with Unix.Unix_error (Unix.EINTR, _, _) -> retry attempt
  in
  let fill bytes length = retry (fun () -> Unix.read fd bytes 0 length) in
  let wait_readable ms =
    let ready, _, _ = retry (fun () -> Unix.select [ fd ] [] [] (float_of_int ms /. 1000.)) in
    ready <> []
  in
  { fill; wait_readable }

type reader = {
  input : input;
  bytes : Bytes.t;
  mutable length : int;
  mutable offset : int;
}

let make_reader input = { input; bytes = Bytes.create 4096; length = 0; offset = 0 }

(* Buffer at least one more byte, or report why that is impossible. *)
let refill reader ~deadline =
  if reader.offset < reader.length then Ok ()
  else begin
    reader.offset <- 0;
    reader.length <- 0;
    if not (reader.input.wait_readable (ms_until deadline)) then Error Timeout
    else
      match reader.input.fill reader.bytes (Bytes.length reader.bytes) with
      | 0 -> Error End_of_input
      | filled ->
        reader.length <- filled;
        Ok ()
  end

(* One header line without its CRLF. An input that ends before a line starts
   has ended; a line cut short is a malformed frame. *)
let read_line reader ~deadline =
  let line = Buffer.create 32 in
  let rec loop () =
    match refill reader ~deadline with
    | Error End_of_input when Buffer.length line = 0 -> Error End_of_input
    | Error End_of_input -> Error (Malformed "the header ends before its line feed")
    | Error error -> Error error
    | Ok () ->
      let character = Bytes.get reader.bytes reader.offset in
      reader.offset <- reader.offset + 1;
      if character = '\n' then begin
        let text = Buffer.contents line in
        let length = String.length text in
        Ok (if length > 0 && text.[length - 1] = '\r' then String.sub text 0 (length - 1) else text)
      end
      else if Buffer.length line >= 8192 then Error (Malformed "header line longer than 8192 bytes")
      else begin
        Buffer.add_char line character;
        loop ()
      end
  in
  loop ()

