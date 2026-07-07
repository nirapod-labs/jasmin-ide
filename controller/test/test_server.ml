let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let create () = Jide_controller.Server.create ~log:(fun _ -> ())

let request ?(params = `Assoc []) id method_ =
  Jsonrpc.Packet.Request
    (Jsonrpc.Request.create ~id:(`Int id) ~method_
       ~params:(Jsonrpc.Structured.t_of_yojson params)
       ())

let notification ?(params = `Assoc []) method_ =
  Jsonrpc.Packet.Notification
    (Jsonrpc.Notification.create ~method_
       ~params:(Jsonrpc.Structured.t_of_yojson params)
       ())

let error_code packet =
  match packet with
  | Jsonrpc.Packet.Response { result = Error error; _ } ->
    Some error.Jsonrpc.Response.Error.code
  | _ -> None

let result_json packet =
  match packet with
  | Jsonrpc.Packet.Response { result = Ok json; _ } -> Some json
  | _ -> None

let initialize_params = `Assoc [ ("capabilities", `Assoc []) ]

let test_initialize () =
  let server = create () in
  let replies =
    Jide_controller.Server.handle_packet server
      (request ~params:initialize_params 1 "initialize")
  in
  check "initialize: one reply" (List.length replies = 1);
  match replies with
  | [ reply ] -> (
    match result_json reply with
    | Some json ->
      check "initialize: capabilities"
        (match json with
         | `Assoc fields -> List.mem_assoc "capabilities" fields
         | _ -> false);
      check "initialize: server info"
        (match json with
         | `Assoc fields -> List.mem_assoc "serverInfo" fields
         | _ -> false)
    | None -> check "initialize: result shape" false)
  | _ -> check "initialize: reply count" false

let test_second_initialize () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  let replies = Jide_controller.Server.handle_packet server (request ~params:initialize_params 2 "initialize") in
  check "second initialize: invalid request"
    (List.exists
       (fun reply ->
         error_code reply = Some Jsonrpc.Response.Error.Code.InvalidRequest)
       replies)

let test_request_before_initialize () =
  let server = create () in
  let replies =
    Jide_controller.Server.handle_packet server (request 1 "textDocument/hover")
  in
  check "not initialized: error code"
    (List.exists
       (fun reply ->
         error_code reply = Some Jsonrpc.Response.Error.Code.ServerNotInitialized)
       replies)

let test_unknown_request () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  let replies =
    Jide_controller.Server.handle_packet server (request 2 "no/such/method")
  in
  check "unknown request: method not found"
    (List.exists
       (fun reply ->
         error_code reply = Some Jsonrpc.Response.Error.Code.MethodNotFound)
       replies)

let test_unknown_notification () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  let replies =
    Jide_controller.Server.handle_packet server (notification "no/such/notification")
  in
  check "unknown notification: no reply" (replies = [])

let test_shutdown_sequence () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  let replies = Jide_controller.Server.handle_packet server (request 2 "shutdown") in
  check "shutdown: one reply" (List.length replies = 1);
  check "shutdown: result is null"
    (List.exists (fun reply -> result_json reply = Some `Null) replies);
  check "shutdown: state"
    (Jide_controller.Server.state server
     = Jide_controller.Server.Shutting_down);
  let replies = Jide_controller.Server.handle_packet server (notification "exit") in
  check "exit: no reply" (replies = []);
  check "exit: code 0" (Jide_controller.Server.exit_code server = 0);
  check "exit: exited" (Jide_controller.Server.exited server)

let test_exit_without_shutdown () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  ignore (Jide_controller.Server.handle_packet server (notification "exit"));
  check "exit without shutdown: code 1"
    (Jide_controller.Server.exit_code server = 1)

let test_shutdown_before_initialize () =
  let server = create () in
  let replies = Jide_controller.Server.handle_packet server (request 1 "shutdown") in
  check "shutdown before initialize: error"
    (List.exists
       (fun reply ->
         error_code reply = Some Jsonrpc.Response.Error.Code.ServerNotInitialized)
       replies)

let test_document_sync () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  let uri = "file:///tmp/a.jazz" in
  let open_params =
    `Assoc
      [ ( "textDocument",
          `Assoc
            [ ("uri", `String uri); ("languageId", `String "jasmin-crypto");
              ("version", `Int 1);
              ("text", `String "fn f() -> reg u32 { return 0; }") ] ) ]
  in
  ignore
    (Jide_controller.Server.handle_packet server
       (notification ~params:open_params "textDocument/didOpen"));
  let engine = Jide_controller.Server.engine server in
  check "didOpen: document stored"
    (Jide_engine.Engine.document engine ~uri <> None);
  let change_params =
    `Assoc
      [ ( "textDocument",
          `Assoc [ ("uri", `String uri); ("version", `Int 2) ] );
        ( "contentChanges",
          `List [ `Assoc [ ("text", `String "fn g() -> reg u32 { return 1; }") ] ] ) ]
  in
  ignore
    (Jide_controller.Server.handle_packet server
       (notification ~params:change_params "textDocument/didChange"));
  (match Jide_engine.Engine.document engine ~uri with
   | Some document ->
     check "didChange: version" (Jide_engine.Document.version document = 2);
     check "didChange: text"
       (Jide_engine.Document.text document = "fn g() -> reg u32 { return 1; }")
   | None -> check "didChange: document kept" false);
  let close_params =
    `Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ]
  in
  ignore
    (Jide_controller.Server.handle_packet server
       (notification ~params:close_params "textDocument/didClose"));
  check "didClose: document removed"
    (Jide_engine.Engine.document engine ~uri = None)

let test_malformed_params () =
  let server = create () in
  ignore (Jide_controller.Server.handle_packet server (request ~params:initialize_params 1 "initialize"));
  let bad =
    Jsonrpc.Packet.Request
      (Jsonrpc.Request.create ~id:(`Int 2) ~method_:"textDocument/hover"
         ~params:(`Assoc [ ("not", `String "a hover") ]) ())
  in
  let replies = Jide_controller.Server.handle_packet server bad in
  check "malformed params: one error reply" (List.length replies = 1)

let () =
  test_initialize ();
  test_second_initialize ();
  test_request_before_initialize ();
  test_unknown_request ();
  test_unknown_notification ();
  test_shutdown_sequence ();
  test_exit_without_shutdown ();
  test_shutdown_before_initialize ();
  test_document_sync ();
  test_malformed_params ();
  if !failures = 0 then print_endline "all server tests passed"
  else (
    Printf.printf "%d server test(s) failed\n" !failures;
    exit 1)
