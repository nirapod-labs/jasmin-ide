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

