type t = {
  input : Unix.file_descr;
  output : out_channel;
  mutable buffer : Bytes.t;
      (* The bytes read from [input] and not consumed yet, from [start] to
         [stop]. It grows for a header line longer than it is; a body is read
         straight into a string of its own instead. *)
  mutable start : int;
  mutable stop : int;
  mutable poisoned : bool;
}

type packet = Jsonrpc.Packet.t
type read_result = (packet, string) result option

let max_frame_bytes = 64 * 1024 * 1024
let max_json_depth = 256
let initial_buffer_bytes = 8192

let create ~input ~output =
  {
    input = Unix.descr_of_in_channel input;
    output;
    buffer = Bytes.create initial_buffer_bytes;
    start = 0;
    stop = 0;
    poisoned = false;
  }

let buffered channels = channels.stop - channels.start

let strip_cr line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then
    String.sub line 0 (length - 1)
  else line

let parse_header line =
  match String.index_opt line ':' with
  | None -> Error ("a header line without a colon: " ^ line)
  | Some colon -> (
      let name = String.trim (String.sub line 0 colon) in
      let value =
        String.trim
          (String.sub line (colon + 1) (String.length line - colon - 1))
      in
      if not (String.equal (String.lowercase_ascii name) "content-length") then
        Ok None
      else
        match int_of_string_opt value with
        | Some length when length >= 0 -> Ok (Some length)
        | Some _ -> Error "a negative Content-Length"
        | None -> Error ("a Content-Length that is not a number: " ^ value))

(* Make room for [extra] more bytes after [stop], moving what is unread to
   the front first and growing the buffer only when that is not enough. *)
let make_room channels extra =
  let used = buffered channels in
  if Bytes.length channels.buffer - channels.stop < extra then (
    if used + extra <= Bytes.length channels.buffer then (
      Bytes.blit channels.buffer channels.start channels.buffer 0 used;
      channels.start <- 0;
      channels.stop <- used)
    else
      let size = ref (Bytes.length channels.buffer) in
      while !size < used + extra do
        size := !size * 2
      done;
      let grown = Bytes.create !size in
      Bytes.blit channels.buffer channels.start grown 0 used;
      channels.buffer <- grown;
      channels.start <- 0;
      channels.stop <- used)

(* More bytes, or [false] at the end of the input. *)
let rec read_some channels =
  make_room channels 1;
  match
    Unix.read channels.input channels.buffer channels.stop
      (Bytes.length channels.buffer - channels.stop)
  with
  | 0 -> false
  | count ->
      channels.stop <- channels.stop + count;
      true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_some channels

(* The first line break still to be read, or [None] when the buffer holds
   none: the scan stops at [stop], because the room after it is stale. *)
let find_break channels =
  let rec scan index =
    if index >= channels.stop then None
    else if Char.equal (Bytes.get channels.buffer index) '\n' then Some index
    else scan (index + 1)
  in
  scan channels.start

(* One line, without its line break, or [None] at the end of the input. A
   last line that the peer ended without a break is still a line, which is
   what [input_line] does and what the error messages then describe. *)
let rec read_line channels =
  match find_break channels with
  | Some break ->
      let line =
        Bytes.sub_string channels.buffer channels.start (break - channels.start)
      in
      channels.start <- break + 1;
      Some line
  | None ->
      if read_some channels then read_line channels
      else if buffered channels = 0 then None
      else
        let line =
          Bytes.sub_string channels.buffer channels.start (buffered channels)
        in
        channels.start <- channels.stop;
        Some line

(* Exactly [length] bytes, or [None] when the input ends first. What is
   already buffered is copied first and the rest is read into the result, so a
   frame body of any accepted size needs no buffer of its own. *)
let read_exactly channels length =
  let body = Bytes.create length in
  let copied = min (buffered channels) length in
  Bytes.blit channels.buffer channels.start body 0 copied;
  channels.start <- channels.start + copied;
  let rec fill offset =
    if offset >= length then Some (Bytes.unsafe_to_string body)
    else
      match Unix.read channels.input body offset (length - offset) with
      | 0 -> None
      | count -> fill (offset + count)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> fill offset
  in
  fill copied

(* The first non-space byte decides whether the body claims to be JSON. A
   body that does not is left to the JSON decoder, which reports it as
   invalid JSON rather than as a nesting problem. *)
let looks_like_json body =
  let rec first index =
    if index >= String.length body then None
    else
      match body.[index] with
      | ' ' | '\t' | '\r' | '\n' -> first (index + 1)
      | c -> Some c
  in
  match first 0 with Some ('{' | '[') -> true | _ -> false

let too_deep (body : string) : bool =
  let depth = ref 0 in
  let deepest = ref 0 in
  let in_string = ref false in
  let escaped = ref false in
  String.iter
    (fun c ->
      if !in_string then (
        if !escaped then escaped := false
        else if Char.equal c '\\' then escaped := true
        else if Char.equal c '"' then in_string := false)
      else if Char.equal c '"' then in_string := true
      else if Char.equal c '{' || Char.equal c '[' then (
        incr depth;
        if !depth > !deepest then deepest := !depth)
      else if Char.equal c '}' || Char.equal c ']' then
        depth := max 0 (!depth - 1))
    body;
  !deepest > max_json_depth

let dispatch channels =
  let poison message =
    channels.poisoned <- true;
    Some (Error message)
  in
  let rec headers started content_length =
    match read_line channels with
    | None ->
        if started then poison "the header ended before its blank line"
        else None
    | Some line -> (
        let line = strip_cr line in
        if String.equal line "" then
          match content_length with
          | None -> poison "no Content-Length header"
          | Some length when length > max_frame_bytes ->
              poison "a Content-Length above the frame limit"
          | Some length -> body length
        else
          match parse_header line with
          | Error message -> poison message
          | Ok None -> headers true content_length
          | Ok (Some length) -> (
              match content_length with
              | Some _ -> poison "more than one Content-Length header"
              | None -> headers true (Some length)))
  and body length =
    match read_exactly channels length with
    | None -> poison "the body ends before Content-Length bytes"
    | Some text -> (
        if looks_like_json text && too_deep text then
          poison "a JSON nesting depth above the limit"
        else
          match Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string text) with
          | packet -> Some (Ok packet)
          | exception Yojson.Json_error message ->
              Some (Error ("invalid json: " ^ message))
          | exception Jsonrpc.Json.Of_json (what, _) ->
              Some (Error ("invalid packet: " ^ what))
          | exception Stack_overflow ->
              poison "the body is too deeply nested to decode")
  in
  headers false None

let read channels =
  if channels.poisoned then None
  else
    match dispatch channels with
    | result -> result
    | exception Sys_error message ->
        channels.poisoned <- true;
        Some (Error ("read error: " ^ message))
    | exception Unix.Unix_error (error, _, _) ->
        channels.poisoned <- true;
        Some (Error ("read error: " ^ Unix.error_message error))

(* A poisoned stream is ready: the read that follows answers end of input
   from the poison alone, so a caller that waits before every read must not
   be parked on a descriptor that can never produce another byte. *)
let wait channels ~timeout =
  if channels.poisoned then true
  else if buffered channels > 0 then true
  else
    match Unix.select [ channels.input ] [] [] timeout with
    | ready, _, _ -> ready <> []
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> false

let write channels packet =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let data = Yojson.Safe.to_string json in
  let header =
    Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length data)
  in
  output_string channels.output header;
  output_string channels.output data;
  flush channels.output
