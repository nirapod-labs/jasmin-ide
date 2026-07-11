let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then false
    else if String.equal (String.sub text index fragment_length) fragment then
      true
    else from (index + 1)
  in
  fragment_length = 0 || from 0

let create () = Jide_controller.Server.create ~log:(fun _ -> ())

let create_logging () =
  let messages = ref [] in
  let log message = messages := message :: !messages in
  (Jide_controller.Server.create ~log, messages)

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

let send server packet = Jide_controller.Server.handle_packet server packet

let error_code packet =
  match packet with
  | Jsonrpc.Packet.Response { result = Error error; _ } ->
    Some error.Jsonrpc.Response.Error.code
  | _ -> None

let result_json packet =
  match packet with
  | Jsonrpc.Packet.Response { result = Ok json; _ } -> Some json
  | _ -> None

(* A reply is only correct when it is the only packet and carries the code
   the protocol names for the case: an extra packet or a different code is
   a failure, not a pass. *)
let check_single_error name code replies =
  match replies with
  | [ reply ] -> check (name ^ ": error code") (error_code reply = Some code)
  | _ -> check (name ^ ": exactly one reply") false

let initialize_params = `Assoc [ ("capabilities", `Assoc []) ]

let initialize server =
  ignore (send server (request ~params:initialize_params 1 "initialize"))

let shutdown server = ignore (send server (request 2 "shutdown"))

let send_exit server = ignore (send server (notification "exit"))

let open_params ~uri ~version ~text =
  `Assoc
    [ ( "textDocument",
        `Assoc
          [ ("uri", `String uri); ("languageId", `String "jasmin-crypto");
            ("version", `Int version); ("text", `String text) ] ) ]

let change_params ~uri ~version ~text =
  `Assoc
    [ ("textDocument", `Assoc [ ("uri", `String uri); ("version", `Int version) ]);
      ("contentChanges", `List [ `Assoc [ ("text", `String text) ] ]) ]

let close_params ~uri =
  `Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ]

let did_open server ~uri ~text =
  ignore
    (send server
       (notification ~params:(open_params ~uri ~version:1 ~text)
          "textDocument/didOpen"))

let did_change server ~uri ~version ~text =
  ignore
    (send server
       (notification ~params:(change_params ~uri ~version ~text)
          "textDocument/didChange"))

let did_close server ~uri =
  ignore
    (send server
       (notification ~params:(close_params ~uri) "textDocument/didClose"))

let document server ~uri =
  Jide_engine.Engine.document (Jide_controller.Server.engine server) ~uri

let stored_text server ~uri =
  match document server ~uri with
  | Some document -> Some (Jide_engine.Document.text document)
  | None -> None

let test_initialize () =
  let server = create () in
  let replies = send server (request ~params:initialize_params 1 "initialize") in
  check "initialize: one reply" (List.length replies = 1);
  match replies with
  | [ reply ] -> (
    match result_json reply with
    | Some (`Assoc fields) ->
      check "initialize: capabilities are exactly full text sync"
        (List.assoc_opt "capabilities" fields
        = Some (`Assoc [ ("textDocumentSync", `Int 1) ]));
      check "initialize: serverInfo names the server"
        (match List.assoc_opt "serverInfo" fields with
         | Some (`Assoc info) ->
           List.assoc_opt "name" info
           = Some (`String Jide_controller.Version.server_name)
         | _ -> false)
    | Some _ -> check "initialize: result is an object" false
    | None -> check "initialize: result shape" false)
  | _ -> check "initialize: reply count" false

let test_second_initialize () =
  let server = create () in
  initialize server;
  let replies = send server (request ~params:initialize_params 2 "initialize") in
  check_single_error "second initialize"
    Jsonrpc.Response.Error.Code.InvalidRequest replies

let test_request_before_initialize () =
  let server = create () in
  let replies = send server (request 1 "textDocument/hover") in
  check_single_error "request before initialize"
    Jsonrpc.Response.Error.Code.ServerNotInitialized replies

let test_unknown_request () =
  let server = create () in
  initialize server;
  let replies = send server (request 2 "no/such/method") in
  check_single_error "unknown request"
    Jsonrpc.Response.Error.Code.MethodNotFound replies

let test_unknown_notification () =
  let server = create () in
  initialize server;
  let replies = send server (notification "no/such/notification") in
  check "unknown notification: no reply" (replies = [])

let test_shutdown_sequence () =
  let server = create () in
  initialize server;
  let replies = send server (request 2 "shutdown") in
  (match replies with
   | [ reply ] -> check "shutdown: result is null" (result_json reply = Some `Null)
   | _ -> check "shutdown: one reply" false);
  check "shutdown: state"
    (Jide_controller.Server.state server = Jide_controller.Server.Shutting_down);
  let replies = send server (notification "exit") in
  check "exit: no reply" (replies = []);
  check "exit: code 0" (Jide_controller.Server.exit_code server = 0);
  check "exit: exited" (Jide_controller.Server.exited server)

let test_exit_without_shutdown () =
  let server = create () in
  initialize server;
  send_exit server;
  check "exit without shutdown: code 1"
    (Jide_controller.Server.exit_code server = 1);
  check "exit without shutdown: exited" (Jide_controller.Server.exited server)

let test_exit_before_initialize () =
  let server = create () in
  let replies = send server (notification "exit") in
  check "exit before initialize: no reply" (replies = []);
  check "exit before initialize: exited" (Jide_controller.Server.exited server);
  check "exit before initialize: code 1"
    (Jide_controller.Server.exit_code server = 1)

let test_cancel_request_is_not_gated () =
  let cancel = notification ~params:(`Assoc [ ("id", `Int 7) ]) "$/cancelRequest" in
  let in_state name state server =
    let replies = send server cancel in
    check (name ^ ": no reply") (replies = []);
    check (name ^ ": state unchanged")
      (Jide_controller.Server.state server = state)
  in
  let uninitialized, uninitialized_log = create_logging () in
  in_state "cancel before initialize" Jide_controller.Server.Uninitialized
    uninitialized;
  let running, running_log = create_logging () in
  initialize running;
  in_state "cancel while running" Jide_controller.Server.Running running;
  let shutting_down, shutting_down_log = create_logging () in
  initialize shutting_down;
  shutdown shutting_down;
  in_state "cancel while shutting down" Jide_controller.Server.Shutting_down
    shutting_down;
  let exited, exited_log = create_logging () in
  initialize exited;
  shutdown exited;
  send_exit exited;
  in_state "cancel after exit" Jide_controller.Server.Exited exited;
  check "cancel after exit: code unchanged"
    (Jide_controller.Server.exit_code exited = 0);
  List.iter
    (fun (name, messages) ->
      check (name ^ ": the handler ran") (List.mem "cancel request" !messages))
    [ ("cancel before initialize", uninitialized_log);
      ("cancel while running", running_log);
      ("cancel while shutting down", shutting_down_log);
      ("cancel after exit", exited_log) ]

let test_shutdown_before_initialize () =
  let server = create () in
  let replies = send server (request 1 "shutdown") in
  check_single_error "shutdown before initialize"
    Jsonrpc.Response.Error.Code.ServerNotInitialized replies

let test_document_sync () =
  let server = create () in
  initialize server;
  let uri = "file:///tmp/a.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  check "didOpen: document stored" (document server ~uri <> None);
  did_change server ~uri ~version:2 ~text:"fn g() -> reg u32 { return 1; }";
  (match document server ~uri with
   | Some document ->
     check "didChange: version" (Jide_engine.Document.version document = 2);
     check "didChange: text"
       (Jide_engine.Document.text document = "fn g() -> reg u32 { return 1; }")
   | None -> check "didChange: document kept" false);
  did_close server ~uri;
  check "didClose: document removed" (document server ~uri = None)

let test_multiple_changes_use_the_last () =
  let server = create () in
  initialize server;
  let uri = "file:///tmp/multi.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  let change =
    `Assoc
      [ ( "textDocument",
          `Assoc [ ("uri", `String uri); ("version", `Int 2) ] );
        ( "contentChanges",
          `List
            [ `Assoc [ ("text", `String "fn first() -> reg u32 { return 1; }") ];
              `Assoc [ ("text", `String "fn last() -> reg u32 { return 2; }") ] ] )
      ]
  in
  ignore
    (send server
       (notification ~params:change "textDocument/didChange"));
  check "two changes: the last one is applied"
    (stored_text server ~uri = Some "fn last() -> reg u32 { return 2; }")

let test_malformed_params () =
  let server = create () in
  initialize server;
  let bad =
    Jsonrpc.Packet.Request
      (Jsonrpc.Request.create ~id:(`Int 2) ~method_:"textDocument/hover"
         ~params:(`Assoc [ ("not", `String "a hover") ]) ())
  in
  let replies = send server bad in
  check_single_error "malformed params"
    Jsonrpc.Response.Error.Code.InvalidRequest replies

let test_empty_change_keeps_document () =
  let server = create () in
  initialize server;
  let uri = "file:///tmp/b.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  let change =
    `Assoc
      [ ( "textDocument",
          `Assoc [ ("uri", `String uri); ("version", `Int 2) ] );
        ("contentChanges", `List []) ]
  in
  ignore (send server (notification ~params:change "textDocument/didChange"));
  check "empty change keeps the text"
    (stored_text server ~uri = Some "fn f() -> reg u32 { return 0; }")

let test_ranged_change_ignored () =
  let server = create () in
  initialize server;
  let uri = "file:///tmp/c.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  let change =
    `Assoc
      [ ( "textDocument",
          `Assoc [ ("uri", `String uri); ("version", `Int 2) ] );
        ( "contentChanges",
          `List
            [ `Assoc
                [ ( "range",
                    `Assoc
                      [ ( "start",
                          `Assoc [ ("line", `Int 0); ("character", `Int 0) ] );
                        ( "end",
                          `Assoc [ ("line", `Int 0); ("character", `Int 5) ] ) ] );
                  ("text", `String "fn g") ] ] ) ]
  in
  ignore (send server (notification ~params:change "textDocument/didChange"));
  check "ranged change ignored"
    (stored_text server ~uri = Some "fn f() -> reg u32 { return 0; }")

let test_change_for_unopened_document_is_dropped () =
  let server = create () in
  initialize server;
  let unopened = "file:///tmp/never-opened.jazz" in
  did_change server ~uri:unopened ~version:1 ~text:"fn f() -> reg u32 { return 0; }";
  check "didChange without didOpen: no document created"
    (document server ~uri:unopened = None);
  let closed = "file:///tmp/closed.jazz" in
  did_open server ~uri:closed ~text:"fn f() -> reg u32 { return 0; }";
  did_close server ~uri:closed;
  did_change server ~uri:closed ~version:2 ~text:"fn g() -> reg u32 { return 1; }";
  check "didChange after didClose: the document stays closed"
    (document server ~uri:closed = None)

let test_notification_before_initialize_is_dropped () =
  let server = create () in
  let uri = "file:///tmp/before.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  check "didOpen before initialize: no document"
    (document server ~uri = None);
  check "didOpen before initialize: still uninitialized"
    (Jide_controller.Server.state server = Jide_controller.Server.Uninitialized)

let test_notification_after_shutdown_is_dropped () =
  let server = create () in
  initialize server;
  let kept = "file:///tmp/kept.jazz" in
  let added = "file:///tmp/added.jazz" in
  let closed = "file:///tmp/closed-after-shutdown.jazz" in
  did_open server ~uri:kept ~text:"fn kept() -> reg u32 { return 0; }";
  did_open server ~uri:closed ~text:"fn closed() -> reg u32 { return 0; }";
  shutdown server;
  did_change server ~uri:kept ~version:2
    ~text:"fn replaced() -> reg u32 { return 1; }";
  did_open server ~uri:added ~text:"fn added() -> reg u32 { return 0; }";
  did_close server ~uri:closed;
  check "after shutdown: state"
    (Jide_controller.Server.state server = Jide_controller.Server.Shutting_down);
  check "after shutdown: didChange dropped"
    (stored_text server ~uri:kept = Some "fn kept() -> reg u32 { return 0; }");
  check "after shutdown: didOpen dropped" (document server ~uri:added = None);
  check "after shutdown: didClose dropped" (document server ~uri:closed <> None)

let test_notification_parse_error_names_the_state () =
  let server, messages = create_logging () in
  initialize server;
  shutdown server;
  send_exit server;
  let replies =
    send server
      (notification ~params:(`Assoc [ ("textDocument", `Assoc []) ])
         "textDocument/didOpen")
  in
  check "parse error after exit: no reply" (replies = []);
  check "parse error after exit: the log names the state"
    (List.exists (fun message -> contains message "exited") !messages);
  check "parse error after exit: the drop is not reported as pre-initialize"
    (not (List.mem "notification ignored before initialize" !messages))

let () =
  test_initialize ();
  test_second_initialize ();
  test_request_before_initialize ();
  test_unknown_request ();
  test_unknown_notification ();
  test_shutdown_sequence ();
  test_exit_without_shutdown ();
  test_exit_before_initialize ();
  test_cancel_request_is_not_gated ();
  test_shutdown_before_initialize ();
  test_document_sync ();
  test_multiple_changes_use_the_last ();
  test_malformed_params ();
  test_empty_change_keeps_document ();
  test_ranged_change_ignored ();
  test_change_for_unopened_document_is_dropped ();
  test_notification_before_initialize_is_dropped ();
  test_notification_after_shutdown_is_dropped ();
  test_notification_parse_error_names_the_state ();
  if !failures = 0 then print_endline "all server tests passed"
  else (
    Printf.printf "%d server test(s) failed\n" !failures;
    exit 1)
