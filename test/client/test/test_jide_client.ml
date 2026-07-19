(* Tests for the protocol test client. Every case runs against the fake
   server; a few also check the framing reader directly. *)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "ok: %s\n%!" name
  else (
    incr failures;
    Printf.printf "not ok: %s\n%!" name)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file contents body =
  let path = Filename.temp_file "jide_test_client" ".txt" in
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel;
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> body path)

type outcome = { code : int; output : string; message : string }

(* Run one session against an arbitrary transport, capturing stdout. *)
let run_transport ~timeout_ms ~session transport =
  let output_path = Filename.temp_file "jide_test_client_stdout" ".txt" in
  let out = open_out_bin output_path in
  let code, message =
    match Jide_session.parse_string session with
    | Error message -> (2, message)
    | Ok steps -> (
        match Jide_client.run ~out ~transport ~timeout_ms steps with
        | Ok () -> (0, "")
        | Error failure ->
            (failure.Jide_client.code, failure.Jide_client.message))
  in
  close_out out;
  let output = read_file output_path in
  Sys.remove output_path;
  { code; output; message }

let run_fake ~timeout_ms ~script ~session =
  match Jide_session.parse_script_string script with
  | Error message -> { code = 2; output = ""; message }
  | Ok script ->
      run_transport ~timeout_ms ~session (Jide_fake.transport ~script)

(* A transport that reads raw bytes, for frames that the fake server cannot
   express as a JSON step. *)
let raw_transport bytes =
  let reader = Jide_framing.make_reader (Jide_framing.string_input bytes) in
  {
    Jide_client.send = (fun _ -> ());
    recv = (fun ~timeout_ms -> Jide_framing.read_message reader ~timeout_ms);
    close_input = (fun () -> ());
    shutdown = (fun () -> ());
  }

let numeric_id () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|}
      ~script:{|{"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}|}
  in
  check "numeric id"
    (outcome.code = 0 && outcome.output = "1 result {\"capabilities\":{}}\n")

let string_id () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":"req-7","method":"shutdown"}|}
      ~script:{|{"jsonrpc":"2.0","id":"req-7","result":null}|}
  in
  check "string id" (outcome.code = 0 && outcome.output = "req-7 result null\n")

let notification_is_not_answered () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:
        {|{"jsonrpc":"2.0","method":"initialized","params":{}}

{"jsonrpc":"2.0","id":7,"method":"shutdown"}|}
      ~script:{|{"jsonrpc":"2.0","id":7,"result":null}|}
  in
  check "notification is not answered"
    (outcome.code = 0 && outcome.output = "7 result null\n")

let error_reply () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":3,"method":"no/such/method"}|}
      ~script:
        {|{"jsonrpc":"2.0","id":3,"error":{"message":"method not found","code":-32601}}|}
  in
  check "error reply"
    (outcome.code = 0
    && outcome.output
       = "3 error {\"code\":-32601,\"message\":\"method not found\"}\n")

let nested_key_sorting () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":4,"method":"server/capabilities"}|}
      ~script:
        {|{"jsonrpc":"2.0","id":4,"result":{"b":{"z":1,"a":2},"a":[{"y":true,"x":null}]}}|}
  in
  check "nested key sorting"
    (outcome.code = 0
    && outcome.output
       = "4 result {\"a\":[{\"x\":null,\"y\":true}],\"b\":{\"a\":2,\"z\":1}}\n"
    )

let timeout () =
  let outcome =
    run_fake ~timeout_ms:120
      ~session:{|{"jsonrpc":"2.0","id":5,"method":"slow"}|}
      ~script:{|#sleep 5000

{"jsonrpc":"2.0","id":5,"result":null}|}
  in
  check "timeout exits 1"
    (outcome.code = 1 && outcome.output = "" && outcome.message = "timeout")

let malformed_content_length () =
  let bytes = "Content-Length: banana\r\n\r\n{}" in
  let reader = Jide_framing.make_reader (Jide_framing.string_input bytes) in
  check "malformed Content-Length (reader)"
    (match Jide_framing.read_message reader ~timeout_ms:100 with
    | Error (Jide_framing.Malformed _) -> true
    | Error (Jide_framing.Timeout | Jide_framing.End_of_input) | Ok _ -> false);
  let outcome =
    run_transport ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":6,"method":"initialize"}|}
      (raw_transport bytes)
  in
  check "malformed Content-Length (exit 2)"
    (outcome.code = 2 && outcome.output = "" && outcome.message <> "")

let framing_details () =
  let truncated = "Content-Length: 40\r\n\r\n{}" in
  let reader = Jide_framing.make_reader (Jide_framing.string_input truncated) in
  check "truncated body is the server leaving"
    (match Jide_framing.read_message reader ~timeout_ms:100 with
    | Error Jide_framing.End_of_input -> true
    | Error (Jide_framing.Timeout | Jide_framing.Malformed _) | Ok _ -> false);
  let truncated_run =
    run_transport ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":16,"method":"shutdown"}|}
      (raw_transport truncated)
  in
  check "truncated body exits 3"
    (truncated_run.code = 3 && truncated_run.output = ""
    && truncated_run.message <> "");
  let cut_header =
    Jide_framing.make_reader (Jide_framing.string_input "Content-Len")
  in
  check "a header cut short is a malformed frame"
    (match Jide_framing.read_message cut_header ~timeout_ms:100 with
    | Error (Jide_framing.Malformed _) -> true
    | Error (Jide_framing.Timeout | Jide_framing.End_of_input) | Ok _ -> false);
  let body = {|{"jsonrpc":"2.0","id":14,"result":null}|} in
  let bytes =
    Printf.sprintf
      "content-length: %d\r\nContent-Type: application/json\r\n\r\n%s"
      (String.length body) body
  in
  let outcome =
    run_transport ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":14,"method":"shutdown"}|}
      (raw_transport bytes)
  in
  check "lower-case header name and extra headers are accepted"
    (outcome.code = 0 && outcome.output = "14 result null\n")

(* One frame body of exactly [count] bytes: {"s":"<count - 8 characters>"}. *)
let body_of_length count =
  Printf.sprintf {|{"s":"%s"}|} (String.make (count - 8) 'x')

let body_limit () =
  let limit = Jide_framing.max_body_bytes in
  let accepted = body_of_length (limit - 1) in
  let frame =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length accepted)
      accepted
  in
  let reader = Jide_framing.make_reader (Jide_framing.string_input frame) in
  let read_accepted () =
    match Jide_framing.read_message reader ~timeout_ms:1000 with
    | Ok received -> String.length received = limit - 1
    | Error _ -> false
  in
  check "a body just under the limit is accepted"
    (String.length accepted = limit - 1 && read_accepted ());
  let over = Printf.sprintf "Content-Length: %d\r\n\r\n" (limit + 1) in
  let over_reader = Jide_framing.make_reader (Jide_framing.string_input over) in
  check "a body above the limit is refused before it is read"
    (match Jide_framing.read_message over_reader ~timeout_ms:1000 with
    | Error (Jide_framing.Malformed _) -> true
    | Error (Jide_framing.Timeout | Jide_framing.End_of_input) | Ok _ -> false);
  check "an absurd Content-Length is refused without allocating"
    (match Jide_framing.content_length "Content-Length: 999999999999" with
    | Error (Jide_framing.Malformed _) -> true
    | Error (Jide_framing.Timeout | Jide_framing.End_of_input) | Ok _ -> false);
  let over_run =
    run_transport ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":17,"method":"shutdown"}|}
      (raw_transport over)
  in
  check "a body above the limit exits 2"
    (over_run.code = 2 && over_run.output = "" && over_run.message <> "")

let session_file_errors () =
  let directive text =
    match Jide_session.parse_string text with Error _ -> true | Ok _ -> false
  in
  check "session file errors"
    (directive "#frobnicate 1" && directive "#sleep" && directive "#sleep soon"
    &&
    match Jide_session.parse_script_string "#expect-timeout 10" with
    | Error _ -> true
    | Ok _ -> false)

let end_of_input () =
  let reader = Jide_framing.make_reader (Jide_framing.string_input "") in
  check "end of input (reader)"
    (match Jide_framing.read_message reader ~timeout_ms:100 with
    | Error Jide_framing.End_of_input -> true
    | Error (Jide_framing.Timeout | Jide_framing.Malformed _) | Ok _ -> false);
  let closed_by_server =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":8,"method":"shutdown"}|}
      ~script:{|#close|}
  in
  check "end of input (server closes before replying)"
    (closed_by_server.code = 3 && closed_by_server.output = "");
  let closed_by_client =
    run_fake ~timeout_ms:1000
      ~session:{|#close

{"jsonrpc":"2.0","id":9,"method":"after/close"}|}
      ~script:""
  in
  check "end of input (client closes stdin)"
    (closed_by_client.code = 3 && closed_by_client.output = "")

let server_notification () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":10,"method":"textDocument/hover"}|}
      ~script:
        {|{"jsonrpc":"2.0","method":"window/logMessage","params":{"type":3,"message":"hello"}}

{"jsonrpc":"2.0","id":10,"result":null}|}
  in
  check "server notification before the reply"
    (outcome.code = 0
    && outcome.output
       = "- window/logMessage {\"message\":\"hello\",\"type\":3}\n\
          10 result null\n")

let expect_timeout () =
  let silent =
    run_fake ~timeout_ms:1000
      ~session:
        {|{"jsonrpc":"2.0","method":"initialized","params":{}}

#expect-timeout 120

{"jsonrpc":"2.0","id":11,"method":"shutdown"}|}
      ~script:{|{"jsonrpc":"2.0","id":11,"result":null}|}
  in
  check "expect-timeout passes on silence"
    (silent.code = 0 && silent.output = "11 result null\n");
  let violated =
    run_transport ~timeout_ms:1000 ~session:{|#expect-timeout 120|}
      (raw_transport
         (Jide_framing.encode {|{"jsonrpc":"2.0","id":13,"result":null}|}))
  in
  check "expect-timeout reports an unexpected reply"
    (violated.code = 2 && violated.output = "")

let deterministic_output () =
  let session =
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}

{"jsonrpc":"2.0","method":"initialized","params":{}}

{"jsonrpc":"2.0","id":"two","method":"server/capabilities"}|}
  in
  let script =
    {|{"jsonrpc":"2.0","id":1,"result":{"b":1,"a":{"d":4,"c":3}}}

{"jsonrpc":"2.0","id":"two","error":{"message":"nope","code":-32000}}|}
  in
  let first = run_fake ~timeout_ms:1000 ~session ~script in
  let second = run_fake ~timeout_ms:1000 ~session ~script in
  check "deterministic output"
    (first.code = 0 && first.code = second.code
    && first.output = second.output
    && first.output
       = "1 result {\"a\":{\"c\":3,\"d\":4},\"b\":1}\n\
          two error {\"code\":-32000,\"message\":\"nope\"}\n")

let executable =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else failwith "the test needs the client executable as its first argument"

let run_binary arguments =
  let output_path = Filename.temp_file "jide_test_client_cli_out" ".txt" in
  let error_path = Filename.temp_file "jide_test_client_cli_err" ".txt" in
  let out = Unix.openfile output_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let err = Unix.openfile error_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let input = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o600 in
  let argv = Array.of_list (executable :: arguments) in
  let pid = Unix.create_process executable argv input out err in
  List.iter Unix.close [ out; err; input ];
  let _, status = Unix.waitpid [] pid in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
  in
  let outcome =
    { code; output = read_file output_path; message = read_file error_path }
  in
  Sys.remove output_path;
  Sys.remove error_path;
  outcome

let command_line () =
  let session =
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|}
  in
  let script = {|{"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}|} in
  with_file session (fun session_file ->
      with_file script (fun script_file ->
          let outcome =
            run_binary
              [
                "--fake-server"; script_file; "--timeout"; "1000"; session_file;
              ]
          in
          check "command line (numeric id accepted)"
            (outcome.code = 0
            && outcome.output = "1 result {\"capabilities\":{}}\n"
            && outcome.message = "");
          let no_session = run_binary [ "--fake-server"; script_file ] in
          check "fake server needs a session file"
            (no_session.code = 2 && no_session.output = ""
           && no_session.message <> "");
          let slow = {|#sleep 5000

{"jsonrpc":"2.0","id":1,"result":null}|} in
          with_file slow (fun slow_file ->
              let outcome =
                run_binary
                  [
                    "--fake-server"; slow_file; "--timeout"; "120"; session_file;
                  ]
              in
              check "command line (timeout reported)"
                (outcome.code = 1 && outcome.output = ""
               && outcome.message <> ""))))

(* The card asks for the binary itself to print the same bytes twice on one
   session, so both runs are real processes over the same two files. *)
let binary_twice () =
  let session =
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}

{"jsonrpc":"2.0","method":"initialized","params":{}}

{"jsonrpc":"2.0","id":"two","method":"server/capabilities"}|}
  in
  let script =
    {|{"jsonrpc":"2.0","id":1,"result":{"b":1,"a":{"d":4,"c":3}}}

{"jsonrpc":"2.0","id":"two","error":{"message":"nope","code":-32000}}|}
  in
  with_file session (fun session_file ->
      with_file script (fun script_file ->
          let arguments =
            [ "--fake-server"; script_file; "--timeout"; "1000"; session_file ]
          in
          let first = run_binary arguments in
          let second = run_binary arguments in
          check "the binary prints the same bytes twice"
            (first.code = 0 && second.code = 0
            && first.output = second.output
            && first.message = "" && second.message = ""
            && first.output
               = "1 result {\"a\":{\"c\":3,\"d\":4},\"b\":1}\n\
                  two error {\"code\":-32000,\"message\":\"nope\"}\n")))

let usage_error () =
  let outcome = run_binary [] in
  check "usage error exits 2"
    (outcome.code = 2 && outcome.output = "" && outcome.message <> "")

let () =
  numeric_id ();
  string_id ();
  notification_is_not_answered ();
  error_reply ();
  nested_key_sorting ();
  timeout ();
  malformed_content_length ();
  framing_details ();
  body_limit ();
  session_file_errors ();
  end_of_input ();
  server_notification ();
  expect_timeout ();
  deterministic_output ();
  command_line ();
  binary_twice ();
  usage_error ();
  Printf.printf "%d failure(s)\n%!" !failures;
  Stdlib.exit (if !failures = 0 then 0 else 1)
