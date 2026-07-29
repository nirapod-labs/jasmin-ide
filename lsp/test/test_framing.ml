(* Round-trip and hostile-frame tests for [Jide_lsp.Framing]. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let show_result = function
  | Ok packet -> Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet)
  | Error msg -> "error: " ^ msg

let show_opt = function
  | None -> "end-of-input"
  | Some result -> show_result result

let contains haystack needle =
  let n = String.length haystack and m = String.length needle in
  let rec loop i =
    i + m <= n && (String.sub haystack i m = needle || loop (i + 1))
  in
  m = 0 || loop 0

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
  let result = f (Jide_lsp.Framing.create ~input:ic ~output:stdout) in
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
  | other -> check ("notification: " ^ show_opt (Some (List.hd other))) false

let test_request_numeric_id () =
  let body =
    {|{"jsonrpc":"2.0","id":7,"method":"textDocument/hover","params":{}}|}
  in
  match read_all [ frame body ] with
  | [ Ok (Jsonrpc.Packet.Request req) ] ->
      check "request id" (req.id = `Int 7);
      check "request method" (String.equal req.method_ "textDocument/hover")
  | other -> check ("request: " ^ show_opt (Some (List.hd other))) false

let test_response_ok () =
  let body = {|{"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}|} in
  match read_all [ frame body ] with
  | [ Ok (Jsonrpc.Packet.Response response) ] -> (
      match response.result with
      | Ok _ -> check "response ok" true
      | Error _ -> check "response ok is an error" false)
  | other -> check ("response: " ^ show_opt (Some (List.hd other))) false

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
            (error.Jsonrpc.Response.Error.code
           = Jsonrpc.Response.Error.Code.MethodNotFound))
  | other -> check ("response error: " ^ show_opt (Some (List.hd other))) false

let test_split_frame () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let full = frame body in
  let half = String.length full / 2 in
  match
    read_all
      [
        String.sub full 0 half; String.sub full half (String.length full - half);
      ]
  with
  | [ Ok (Jsonrpc.Packet.Notification n) ] ->
      check "split frame" (String.equal n.method_ "exit")
  | other -> check ("split frame: " ^ show_opt (Some (List.hd other))) false

let test_header_casing () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw =
    Printf.sprintf "content-length: %d\r\n\r\n%s" (String.length body) body
  in
  match read_all [ raw ] with
  | [ Ok (Jsonrpc.Packet.Notification _) ] -> check "header casing" true
  | other -> check ("header casing: " ^ show_opt (Some (List.hd other))) false

let test_separator_split () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw = frame body in
  let parts =
    [ String.sub raw 0 4; String.sub raw 4 (String.length raw - 4) ]
  in
  match read_all parts with
  | [ Ok (Jsonrpc.Packet.Notification _) ] -> check "separator split" true
  | other -> check ("separator split: " ^ show_opt (Some (List.hd other))) false

let test_truncated_body () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw = frame body in
  let cut = String.length raw - 5 in
  match read_all [ String.sub raw 0 cut ] with
  | [ Error _ ] -> check "truncated body is an error" true
  | other -> check ("truncated body: " ^ show_opt (Some (List.hd other))) false

let test_bad_json () =
  match read_all [ frame "not json" ] with
  | [ Error message ] ->
      check "bad json says so" (contains message "invalid json")
  | other -> check ("bad json: " ^ show_opt (Some (List.hd other))) false

let test_two_frames_one_chunk () =
  let a = frame {|{"jsonrpc":"2.0","method":"initialized"}|} in
  let b = frame {|{"jsonrpc":"2.0","method":"exit"}|} in
  match read_all [ a ^ b ] with
  | [ Ok _; Ok _ ] -> check "two frames" true
  | other ->
      check
        ("two frames: " ^ String.concat " " (List.map show_result other))
        false

let test_end_of_input () =
  match read_all [] with
  | [] -> check "end of input" true
  | other -> check ("end of input: " ^ string_of_int (List.length other)) false

let test_write_round_trip () =
  let request =
    Jsonrpc.Packet.Request
      (Jsonrpc.Request.create ~id:(`Int 3) ~method_:"shutdown" ())
  in
  let r, w = Unix.pipe () in
  let oc = Unix.out_channel_of_descr w in
  Jide_lsp.Framing.write
    (Jide_lsp.Framing.create ~input:stdin ~output:oc)
    request;
  close_out oc;
  let ic = Unix.in_channel_of_descr r in
  let strip_cr line =
    if String.length line > 0 && Char.equal line.[String.length line - 1] '\r'
    then String.sub line 0 (String.length line - 1)
    else line
  in
  let header = strip_cr (input_line ic) in
  let blank = strip_cr (input_line ic) in
  let prefix = "Content-Length: " in
  let body_length =
    if
      String.length header > String.length prefix
      && String.sub header 0 (String.length prefix) = prefix
    then
      int_of_string
        (String.sub header (String.length prefix)
           (String.length header - String.length prefix))
    else -1
  in
  let body =
    if body_length >= 0 then really_input_string ic body_length else ""
  in
  close_in_noerr ic;
  check "written header" (body_length > 0);
  check "written blank line" (String.equal blank "");
  let parsed = Yojson.Safe.from_string body in
  check "written body"
    (match parsed with
    | `Assoc fields ->
        List.mem_assoc "id" fields && List.mem_assoc "method" fields
    | _ -> false)

(* A negative length followed by a second Content-Length must not be
   honoured: the first error poisons the stream. *)
let test_minus_one_then_second_header () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw =
    Printf.sprintf "Content-Length: -1\r\nContent-Length: %d\r\n\r\n%s"
      (String.length body) body
  in
  match read_all [ raw ] with
  | [ Error message ] ->
      check "minus one names the negative length" (contains message "negative")
  | other ->
      check
        ("minus one: " ^ String.concat " " (List.map show_result other))
        false

let test_negative_length () =
  match read_all [ "Content-Length: -2\r\n\r\n{}" ] with
  | [ Error message ] ->
      check "negative length is an error" (contains message "negative")
  | other ->
      check
        ("negative length: " ^ String.concat " " (List.map show_result other))
        false

let test_huge_length () =
  match read_all [ "Content-Length: 1073741824\r\n\r\n" ] with
  | [ Error message ] ->
      check "huge length is refused before reading"
        (contains message "frame limit")
  | other ->
      check
        ("huge length: " ^ String.concat " " (List.map show_result other))
        false

let test_no_content_length () =
  match read_all [ "Content-Type: application/json\r\n\r\n{}" ] with
  | [ Error message ] ->
      check "missing length is an error" (contains message "no Content-Length")
  | other ->
      check
        ("missing length: " ^ String.concat " " (List.map show_result other))
        false

let test_two_content_lengths () =
  let body = {|{"jsonrpc":"2.0","method":"exit"}|} in
  let raw =
    Printf.sprintf "Content-Length: %d\r\nContent-Length: %d\r\n\r\n%s"
      (String.length body) (String.length body) body
  in
  match read_all [ raw ] with
  | [ Error message ] ->
      check "two lengths are refused" (contains message "more than one")
  | other ->
      check
        ("two lengths: " ^ String.concat " " (List.map show_result other))
        false

let test_header_without_colon () =
  match read_all [ "not a header\r\n\r\n{}" ] with
  | [ Error message ] -> check "header without colon" (contains message "colon")
  | other ->
      check
        ("header without colon: "
        ^ String.concat " " (List.map show_result other))
        false

let nested_body depth =
  String.concat "" (List.init depth (fun _ -> "["))
  ^ String.concat "" (List.init depth (fun _ -> "]"))

let test_deeply_nested_body () =
  let body = nested_body 1_000_000 in
  check "the hostile body is about 2 MB" (String.length body = 2_000_000);
  match read_all [ frame body ] with
  | [ Error message ] ->
      check "deep nesting is refused" (contains message "nesting depth")
  | other ->
      check
        ("deep nesting: " ^ String.concat " " (List.map show_result other))
        false

let test_depth_boundary () =
  let rec deep n = if n = 0 then `Null else `List [ deep (n - 1) ] in
  let body depth =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("jsonrpc", `String "2.0");
          ("id", `Int 1);
          ("method", `String "x");
          ("params", deep depth);
        ])
  in
  (match read_all [ frame (body (Jide_lsp.Framing.max_json_depth - 10)) ] with
  | [ Ok _ ] -> check "a body under the limit is accepted" true
  | other ->
      check
        ("under the limit: " ^ String.concat " " (List.map show_result other))
        false);
  match read_all [ frame (body (Jide_lsp.Framing.max_json_depth + 10)) ] with
  | [ Error message ] ->
      check "a body over the limit is refused"
        (contains message "nesting depth")
  | other ->
      check
        ("over the limit: " ^ String.concat " " (List.map show_result other))
        false

let test_braces_in_strings () =
  let body =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("jsonrpc", `String "2.0");
          ("method", `String "exit");
          ("params", `Assoc [ ("text", `String (nested_body 1000)) ]);
        ])
  in
  match read_all [ frame body ] with
  | [ Ok _ ] -> check "braces inside a string do not count" true
  | other ->
      check
        ("braces in strings: " ^ String.concat " " (List.map show_result other))
        false

let test_non_json_body () =
  match read_all [ frame "hello" ] with
  | [ Error message ] ->
      check "a non-JSON body is invalid json, not nesting"
        (contains message "invalid json")
  | other ->
      check
        ("non-JSON body: " ^ String.concat " " (List.map show_result other))
        false

let test_waits_for_nothing () =
  let r, w = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r in
  let oc = Unix.out_channel_of_descr w in
  let channels = Jide_lsp.Framing.create ~input:ic ~output:stdout in
  check "an idle stream is no reason to read"
    (not (Jide_lsp.Framing.wait channels ~timeout:0.01));
  output_string oc (frame {|{"jsonrpc":"2.0","method":"exit"}|});
  flush oc;
  check "a frame that arrived ends the wait"
    (Jide_lsp.Framing.wait channels ~timeout:1.0);
  (match Jide_lsp.Framing.read channels with
  | Some (Ok (Jsonrpc.Packet.Notification n)) ->
      check "the frame after a wait" (String.equal n.method_ "exit")
  | other -> check ("the frame after a wait: " ^ show_opt other) false);
  check "a read frame leaves nothing to wait for"
    (not (Jide_lsp.Framing.wait channels ~timeout:0.01));
  output_string oc
    (frame {|{"jsonrpc":"2.0","method":"initialized"}|}
    ^ frame {|{"jsonrpc":"2.0","method":"exit"}|});
  flush oc;
  (* Both frames arrive in one write: the second is buffered, so waiting must
     not fall back on the descriptor the buffer has already emptied. *)
  check "a buffered second frame is a reason to read"
    (Jide_lsp.Framing.wait channels ~timeout:0.01);
  (match Jide_lsp.Framing.read channels with
  | Some (Ok (Jsonrpc.Packet.Notification n)) ->
      check "the first of two frames" (String.equal n.method_ "initialized")
  | other -> check ("the first of two frames: " ^ show_opt other) false);
  check "the second frame is still a reason to read"
    (Jide_lsp.Framing.wait channels ~timeout:0.01);
  (match Jide_lsp.Framing.read channels with
  | Some (Ok (Jsonrpc.Packet.Notification n)) ->
      check "the second of two frames" (String.equal n.method_ "exit")
  | other -> check ("the second of two frames: " ^ show_opt other) false);
  close_out oc;
  (* The descriptor of a closed writer is readable, and the read that follows
     learns the input has ended: that is a reason to wait, and the read is
     what reports [None]. *)
  ignore (Jide_lsp.Framing.wait channels ~timeout:0.01);
  check "end of input ends the read" (Jide_lsp.Framing.read channels = None);
  close_in_noerr ic

(* A stream a bad header poisoned has nothing left to read, and that is
   exactly what a wait has to report: ready, so the caller's next read is
   end of input. The bug this pins down made the wait look at the descriptor
   instead, so a server waiting without a timeout waited for a byte that
   never came. The writer stays open and no byte follows the header, so
   neither buffered bytes nor the descriptor can answer the wait: only the
   poison can. The timeout is small rather than the -1 the server passes,
   so a regression fails here instead of hanging the suite. *)
let test_poisoned_stream_is_ready () =
  let r, w = Unix.pipe () in
  let ic = Unix.in_channel_of_descr r in
  let oc = Unix.out_channel_of_descr w in
  let channels = Jide_lsp.Framing.create ~input:ic ~output:stdout in
  output_string oc "Content-Length: -1\r\n";
  flush oc;
  (match Jide_lsp.Framing.read channels with
  | Some (Error message) ->
      check "a header that cannot be framed is an error"
        (contains message "negative")
  | other -> check ("the bad header: " ^ show_opt other) false);
  check "a poisoned stream is ready to read"
    (Jide_lsp.Framing.wait channels ~timeout:0.05);
  check "the read after a poisoned stream is end of input"
    (Jide_lsp.Framing.read channels = None);
  close_out oc;
  close_in_noerr ic

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
  test_minus_one_then_second_header ();
  test_negative_length ();
  test_huge_length ();
  test_no_content_length ();
  test_two_content_lengths ();
  test_header_without_colon ();
  test_deeply_nested_body ();
  test_depth_boundary ();
  test_braces_in_strings ();
  test_non_json_body ();
  test_waits_for_nothing ();
  test_poisoned_stream_is_ready ();
  if !failures = 0 then print_endline "all framing tests passed"
  else (
    Printf.printf "%d framing test(s) failed\n" !failures;
    exit 1)
