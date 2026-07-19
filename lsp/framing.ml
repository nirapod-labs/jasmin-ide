type t = { input : in_channel; output : out_channel; mutable poisoned : bool }
type packet = Jsonrpc.Packet.t
type read_result = (packet, string) result option

let max_frame_bytes = 64 * 1024 * 1024
let max_json_depth = 256
let create ~input ~output = { input; output; poisoned = false }

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
    match input_line channels.input with
    | exception End_of_file ->
        if started then poison "the header ended before its blank line"
        else None
    | line -> (
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
    match really_input_string channels.input length with
    | exception End_of_file ->
        poison "the body ends before Content-Length bytes"
    | text -> (
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

let write channels packet =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let data = Yojson.Safe.to_string json in
  let header =
    Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length data)
  in
  output_string channels.output header;
  output_string channels.output data;
  flush channels.output
