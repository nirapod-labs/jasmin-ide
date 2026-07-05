(* Round-trip and malformed-frame tests for [Jide_lsp.Framing]. *)

let failures = ref 0

let check name cond =
  if not cond then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let show_result = function
  | Ok packet -> Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet)
  | Error msg -> "error: " ^ msg

let show_opt = function
  | None -> "end-of-input"
  | Some result -> show_result result

(* Write [chunks] to a pipe, one chunk at a time, then close the writer. *)
let with_writer chunks f =
  let r, w = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r in
  let oc = Unix.out_channel_of_descr w in
  let writer () =
    List.iter
      (fun chunk ->
        output_string oc chunk;
        flush oc)
      chunks;
    close_out oc
  in
  let thread = Thread.create writer () in
  let result = f (Jide_lsp.Framing.{ input = ic; output = stdout }) in
  Thread.join thread;
  close_in_noerr ic;
  result

let frame body =
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

let read_all chunks =
  with_writer chunks (fun channels ->
      let rec loop acc =
        match Jide_lsp.Framing.read channels with
        | None -> List.rev acc
        | Some result -> loop (result :: acc)
      in
      loop [])

let test_notification () =
  let body = {|{"jsonrpc":"2.0","method":"initialized","params":{}}|} in
  match read_all [ frame body ] with
  | [ Ok (Jsonrpc.Packet.Notification n) ] ->
    check "notification method" (String.equal n.method_ "initialized")
  | other -> check ("notification: " ^ show_result (List.hd other)) false

let test_request_numeric_id () =
  let body =
    {|{"jsonrpc":"2.0","id":7,"method":"textDocument/hover","params":{}}|}
  in
  match read_all [ frame body ] with
  | [ Ok (Jsonrpc.Packet.Request req) ] ->
    check "request id" (req.id = `Int 7);
    check "request method" (String.equal req.method_ "textDocument/hover")
  | other -> check ("request: " ^ show_result (List.hd other)) false

let test_response_ok () =
  let body = {|{"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}|} in
  match read_all [ frame body ] with
  | [ Ok (Jsonrpc.Packet.Response response) ] -> (
    match response.result with
    | Ok _ -> check "response ok" true
    | Error _ -> check "response ok is an error" false)
  | other -> check ("response: " ^ show_result (List.hd other)) false

let test_response_error () =
  let body =
    {|{"jsonrpc":"2.0","id":2,"error":{"code":-32601,"message":"nope"}}|}
  in
  match read_all [ frame body ] with
  | [ Ok (Jsonrpc.Packet.Response response) ] -> (
    match response.result with
    | Ok _ -> check "response error is a result" false
    | Error error ->
      check "response error code"
        (error.Jsonrpc.Response.Error.code = Jsonrpc.Response.Error.Code.MethodNotFound))
  | other -> check ("response error: " ^ show_result (List.hd other)) false

let test_split_frame () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let full = frame body in
  let half = String.length full / 2 in
  match
    read_all [ String.sub full 0 half; String.sub full half (String.length full - half) ]
  with
  | [ Ok (Jsonrpc.Packet.Notification n) ] ->
    check "split frame" (String.equal n.method_ "exit")
  | other -> check ("split frame: " ^ show_result (List.hd other)) false

let test_header_casing () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw = Printf.sprintf "content-length: %d\r\n\r\n%s" (String.length body) body in
  match read_all [ raw ] with
  | [ Ok (Jsonrpc.Packet.Notification _) ] -> check "header casing" true
  | other -> check ("header casing: " ^ show_result (List.hd other)) false

let test_separator_split () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw = frame body in
  let crlf = 4 in
  let parts =
    [ String.sub raw 0 crlf; String.sub raw crlf (String.length raw - crlf) ]
  in
  match read_all parts with
  | [ Ok (Jsonrpc.Packet.Notification _) ] -> check "separator split" true
  | other -> check ("separator split: " ^ show_result (List.hd other)) false

let test_truncated_body () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw = frame body in
  let cut = String.length raw - 5 in
  match read_all [ String.sub raw 0 cut ] with
  | [ Error _ ] -> check "truncated body is an error" true
  | other -> check ("truncated body: " ^ show_result (List.hd other)) false

let test_bad_json () =
  let raw = frame "not json" in
  match read_all [ raw ] with
  | [ Error _ ] -> check "bad json is an error" true
  | other -> check ("bad json: " ^ show_result (List.hd other)) false

let test_two_frames_one_chunk () =
  let a = frame {|{"jsonrpc":"2.0","method":"initialized"}|} in
  let b = frame {|{"jsonrpc":"2.0","method":"exit"}|} in
  match read_all [ a ^ b ] with
  | [ Ok _; Ok _ ] -> check "two frames" true
  | other -> check ("two frames: " ^ String.concat " " (List.map show_result other)) false

let test_end_of_input () =
  match read_all [] with [] -> check "end of input" true | other -> check ("end of input: " ^ string_of_int (List.length other)) false

let test_write_round_trip () =
  let request =
    Jsonrpc.Packet.Request
      (Jsonrpc.Request.create ~id:(`Int 3) ~method_:"shutdown" ())
  in
  let r, w = Unix.pipe () in
  let oc = Unix.out_channel_of_descr w in
  Jide_lsp.Framing.write { input = stdin; output = oc } request;
  close_out oc;
  let ic = Unix.in_channel_of_descr r in
  let strip_cr s =
    if String.length s > 0 && s.[String.length s - 1] = '\r' then
      String.sub s 0 (String.length s - 1)
    else s
  in
  let header = strip_cr (input_line ic) in
  let content_type = strip_cr (input_line ic) in
  let blank = strip_cr (input_line ic) in
  let prefix = "Content-Length: " in
  let body_length =
    if String.length header > String.length prefix
       && String.sub header 0 (String.length prefix) = prefix
    then int_of_string (String.sub header (String.length prefix)
                          (String.length header - String.length prefix))
    else -1
  in
  let body = if body_length >= 0 then really_input_string ic body_length else "" in
  close_in_noerr ic;
  check "written header" (body_length > 0);
  check "written content type" (String.length content_type > 0);
  check "written blank line" (String.equal blank "");
  let parsed = Yojson.Safe.from_string body in
  check "written body"
    (match parsed with
     | `Assoc fields -> List.mem_assoc "id" fields && List.mem_assoc "method" fields
     | _ -> false)

let () =
  test_notification ();
  test_request_numeric_id ();
  test_response_ok ();
  test_response_error ();
  test_split_frame ();
  test_header_casing ();
  test_separator_split ();
  test_truncated_body ();
  test_bad_json ();
  test_two_frames_one_chunk ();
  test_end_of_input ();
  test_write_round_trip ();
  if !failures = 0 then print_endline "all framing tests passed"
  else (
    Printf.printf "%d framing test(s) failed\n" !failures;
    exit 1)
