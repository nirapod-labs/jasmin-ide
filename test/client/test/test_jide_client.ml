(* Tests for the protocol test client. Every case runs against the fake
   server; a few also check the framing reader directly. *)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "ok: %s\n%!" name
  else begin
    incr failures;
    Printf.printf "not ok: %s\n%!" name
  end

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
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ()) (fun () -> body path)

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
      | Error failure -> (failure.Jide_client.code, failure.Jide_client.message))
  in
  close_out out;
  let output = read_file output_path in
  Sys.remove output_path;
  { code; output; message }

let run_fake ~timeout_ms ~script ~session =
  match Jide_session.parse_script_string script with
  | Error message -> { code = 2; output = ""; message }
  | Ok script -> run_transport ~timeout_ms ~session (Jide_fake.transport ~script)

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
  check "numeric id" (outcome.code = 0 && outcome.output = "1 result {\"capabilities\":{}}\n")

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
  check "notification is not answered" (outcome.code = 0 && outcome.output = "7 result null\n")

let error_reply () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":3,"method":"no/such/method"}|}
      ~script:
        {|{"jsonrpc":"2.0","id":3,"error":{"message":"method not found","code":-32601}}|}
  in
  check "error reply"
    (outcome.code = 0
    && outcome.output = "3 error {\"code\":-32601,\"message\":\"method not found\"}\n")

let nested_key_sorting () =
  let outcome =
    run_fake ~timeout_ms:1000
      ~session:{|{"jsonrpc":"2.0","id":4,"method":"server/capabilities"}|}
      ~script:
        {|{"jsonrpc":"2.0","id":4,"result":{"b":{"z":1,"a":2},"a":[{"y":true,"x":null}]}}|}
  in
  check "nested key sorting"
    (outcome.code = 0 && outcome.output = "4 result {\"a\":[{\"x\":null,\"y\":true}],\"b\":{\"a\":2,\"z\":1}}\n")

let timeout () =
  let outcome =
    run_fake ~timeout_ms:120
      ~session:{|{"jsonrpc":"2.0","id":5,"method":"slow"}|}
      ~script:
        {|#sleep 5000

{"jsonrpc":"2.0","id":5,"result":null}|}
  in
  check "timeout exits 1" (outcome.code = 1 && outcome.output = "" && outcome.message = "timeout")

