(* A scripted server for testing the client itself. It speaks the same
   framing as a real server, over in-memory pipes: each request that arrives
   is answered with the next step of the script, [Sleep] delays the answer,
   [Close] closes the server's output, and a script that runs out leaves the
   request unanswered. *)

let now = Unix.gettimeofday

let pop_bytes (buffer : Buffer.t) (bytes : Bytes.t) (count : int) =
  let available = Buffer.length buffer in
  if available = 0 then 0
  else begin
    let take = min count available in
    Buffer.blit buffer 0 bytes 0 take;
    let rest = Buffer.sub buffer take (available - take) in
    Buffer.clear buffer;
    Buffer.add_string buffer rest;
    take
  end

