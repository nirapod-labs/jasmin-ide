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

let create () = Jide_controller.Server.create ~log:(fun _ -> ()) ()

let create_logging () =
  let messages = ref [] in
  let log message = messages := message :: !messages in
  (Jide_controller.Server.create ~log (), messages)

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
    [
      ( "textDocument",
        `Assoc
          [
            ("uri", `String uri);
            ("languageId", `String "jasmin-crypto");
            ("version", `Int version);
            ("text", `String text);
          ] );
    ]

let change_params ~uri ~version ~text =
  `Assoc
    [
      ( "textDocument",
        `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
      ("contentChanges", `List [ `Assoc [ ("text", `String text) ] ]);
    ]

let close_params ~uri =
  `Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ]

let did_open server ~uri ~text =
  ignore
    (send server
       (notification
          ~params:(open_params ~uri ~version:1 ~text)
          "textDocument/didOpen"))

let did_change server ~uri ~version ~text =
  ignore
    (send server
       (notification
          ~params:(change_params ~uri ~version ~text)
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

(* The capabilities the server advertises, exactly: full text sync, the four
   navigation requests and the seven editing requests of [Features] with the
   shape each needs. The legend is read from the converter rather than written
   twice, because its order is the encoding and a test that spelled it out
   again could agree with a server that had drifted from the encoder. *)
let expected_capabilities () =
  `Assoc
    [
      ("textDocumentSync", `Int 1);
      ("definitionProvider", `Bool true);
      ("referencesProvider", `Bool true);
      ("hoverProvider", `Bool true);
      ("documentSymbolProvider", `Bool true);
      ( "completionProvider",
        `Assoc
          [
            ("triggerCharacters", `List [ `String "#"; `String ":" ]);
            ("resolveProvider", `Bool false);
          ] );
      ( "signatureHelpProvider",
        `Assoc [ ("triggerCharacters", `List [ `String "("; `String "," ]) ] );
      (* No renameProvider: the engine has no rename, so advertising it
         would offer the user a rename that can only fail. It returns with
         the engine's rename. *)
      ("workspaceSymbolProvider", `Bool true);
      ("documentHighlightProvider", `Bool true);
      ( "semanticTokensProvider",
        `Assoc
          [
            ( "legend",
              `Assoc
                [
                  ( "tokenTypes",
                    `List
                      (List.map
                         (fun name -> `String name)
                         Jide_controller.Json_conv.semantic_token_types) );
                  ( "tokenModifiers",
                    `List
                      (List.map
                         (fun name -> `String name)
                         Jide_controller.Json_conv.semantic_token_modifiers) );
                ] );
            ("full", `Bool true);
          ] );
    ]

let test_initialize () =
  let server = create () in
  let replies =
    send server (request ~params:initialize_params 1 "initialize")
  in
  check "initialize: one reply" (List.length replies = 1);
  match replies with
  | [ reply ] -> (
      match result_json reply with
      | Some (`Assoc fields) ->
          check "initialize: capabilities are exactly the advertised set"
            (List.assoc_opt "capabilities" fields
            = Some (expected_capabilities ()));
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
  let replies =
    send server (request ~params:initialize_params 2 "initialize")
  in
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
  | [ reply ] ->
      check "shutdown: result is null" (result_json reply = Some `Null)
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
  let cancel =
    notification ~params:(`Assoc [ ("id", `Int 7) ]) "$/cancelRequest"
  in
  let in_state name state server =
    let replies = send server cancel in
    check (name ^ ": no reply") (replies = []);
    check
      (name ^ ": state unchanged")
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
    [
      ("cancel before initialize", uninitialized_log);
      ("cancel while running", running_log);
      ("cancel while shutting down", shutting_down_log);
      ("cancel after exit", exited_log);
    ]

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
      [
        ("textDocument", `Assoc [ ("uri", `String uri); ("version", `Int 2) ]);
        ( "contentChanges",
          `List
            [
              `Assoc [ ("text", `String "fn first() -> reg u32 { return 1; }") ];
              `Assoc [ ("text", `String "fn last() -> reg u32 { return 2; }") ];
            ] );
      ]
  in
  ignore (send server (notification ~params:change "textDocument/didChange"));
  check "two changes: the last one is applied"
    (stored_text server ~uri = Some "fn last() -> reg u32 { return 2; }")

(* A known method's parameters that do not parse are InvalidParams: the
   server understood the method and refused the call, which the protocol
   separates from a method it does not have. *)
let test_malformed_params () =
  let server = create () in
  initialize server;
  let bad =
    Jsonrpc.Packet.Request
      (Jsonrpc.Request.create ~id:(`Int 2) ~method_:"textDocument/hover"
         ~params:(`Assoc [ ("not", `String "a hover") ])
         ())
  in
  let replies = send server bad in
  check_single_error "malformed params"
    Jsonrpc.Response.Error.Code.InvalidParams replies

let test_empty_change_keeps_document () =
  let server = create () in
  initialize server;
  let uri = "file:///tmp/b.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  let change =
    `Assoc
      [
        ("textDocument", `Assoc [ ("uri", `String uri); ("version", `Int 2) ]);
        ("contentChanges", `List []);
      ]
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
      [
        ("textDocument", `Assoc [ ("uri", `String uri); ("version", `Int 2) ]);
        ( "contentChanges",
          `List
            [
              `Assoc
                [
                  ( "range",
                    `Assoc
                      [
                        ( "start",
                          `Assoc [ ("line", `Int 0); ("character", `Int 0) ] );
                        ( "end",
                          `Assoc [ ("line", `Int 0); ("character", `Int 5) ] );
                      ] );
                  ("text", `String "fn g");
                ];
            ] );
      ]
  in
  ignore (send server (notification ~params:change "textDocument/didChange"));
  check "ranged change ignored"
    (stored_text server ~uri = Some "fn f() -> reg u32 { return 0; }")

let test_change_for_unopened_document_is_dropped () =
  let server = create () in
  initialize server;
  let unopened = "file:///tmp/never-opened.jazz" in
  did_change server ~uri:unopened ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  check "didChange without didOpen: no document created"
    (document server ~uri:unopened = None);
  let closed = "file:///tmp/closed.jazz" in
  did_open server ~uri:closed ~text:"fn f() -> reg u32 { return 0; }";
  did_close server ~uri:closed;
  did_change server ~uri:closed ~version:2
    ~text:"fn g() -> reg u32 { return 1; }";
  check "didChange after didClose: the document stays closed"
    (document server ~uri:closed = None)

let test_notification_before_initialize_is_dropped () =
  let server = create () in
  let uri = "file:///tmp/before.jazz" in
  did_open server ~uri ~text:"fn f() -> reg u32 { return 0; }";
  check "didOpen before initialize: no document" (document server ~uri = None);
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
      (notification
         ~params:(`Assoc [ ("textDocument", `Assoc []) ])
         "textDocument/didOpen")
  in
  check "parse error after exit: no reply" (replies = []);
  check "parse error after exit: the log names the state"
    (List.exists (fun message -> contains message "exited") !messages);
  check "parse error after exit: the drop is not reported as pre-initialize"
    (not (List.mem "notification ignored before initialize" !messages))

(* The publish path. The analysis and the clock come from the test, so what
   the server does at a deadline is decided by a number the test writes and
   never by a sleep. The analysis stores, for each document, the version of
   the text it read plus whatever offset the test set, which is how a result
   for a superseded version is produced on demand. *)

let publish_method = "textDocument/publishDiagnostics"

type probe = {
  mutable checks : string list;  (** The documents analysed, in order. *)
  mutable checked_version : int;
      (** The version of the text the last check read, 0 before the first. *)
  mutable forgotten : string list;  (** The documents the analysis dropped. *)
  mutable raises : bool;  (** Whether the analysis fails instead. *)
  mutable version_offset : int;  (** Added to the version it read. *)
  mutable diagnostics : Jide_lang.Diagnostic.t list;  (** What it reports. *)
  mutable required : Jide_controller.Server.required list;
      (** The files it reports errors in, each with its own text. *)
}

let probe () =
  {
    checks = [];
    checked_version = 0;
    forgotten = [];
    raises = false;
    version_offset = 0;
    diagnostics = [];
    required = [];
  }

let sample_diagnostic message =
  Jide_lang.Diagnostic.make ~severity:Jide_lang.Diagnostic.Error
    (Jide_lang.Span.make ~start_byte:0 ~end_byte:1)
    message

let probe_analysis (probe : probe) engine : Jide_controller.Server.analysis =
  let stored = Hashtbl.create 4 in
  {
    Jide_controller.Server.check =
      (fun ~uri ->
        probe.checks <- probe.checks @ [ uri ];
        if probe.raises then raise (Failure "the analysis exploded");
        match Jide_engine.Engine.document engine ~uri with
        | None -> Hashtbl.remove stored uri
        | Some document ->
            probe.checked_version <- Jide_engine.Document.version document;
            Hashtbl.replace stored uri
              ( Jide_engine.Document.version document + probe.version_offset,
                probe.diagnostics ));
    diagnostics = (fun ~uri -> Hashtbl.find_opt stored uri);
    required =
      (fun ~uri ->
        match Hashtbl.find_opt stored uri with
        | Some _ -> probe.required
        | None -> []);
    forget =
      (fun ~uri ->
        probe.forgotten <- probe.forgotten @ [ uri ];
        Hashtbl.remove stored uri);
  }

let clock = ref 0.0
let advance seconds = clock := !clock +. seconds

let create_probe ?(log = fun _ -> ()) ?locate probe =
  let engine = Jide_engine.Engine.create () in
  let server =
    Jide_controller.Server.create ~log ?engine:(Some engine)
      ~analysis:(probe_analysis probe engine)
      ?locate
      ~now:(fun () -> !clock)
      ()
  in
  (server, engine)

let analyse_due server = Jide_controller.Server.analyse_due server

let json_of_params = function
  | Some (`Assoc fields) -> `Assoc fields
  | Some (`List items) -> `List items
  | None -> `Null

let publishes packets =
  List.filter_map
    (function
      | Jsonrpc.Packet.Notification notification
        when String.equal notification.Jsonrpc.Notification.method_
               publish_method ->
          Some (json_of_params notification.params)
      | _ -> None)
    packets

let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let int_field name json =
  match field name json with Some (`Int value) -> Some value | _ -> None

let string_field name json =
  match field name json with Some (`String value) -> Some value | _ -> None

let diagnostics_field json =
  match field "diagnostics" json with
  | Some (`List items) -> Some items
  | _ -> None

let check_int name actual expected =
  if actual <> expected then (
    incr failures;
    Printf.printf "FAIL %s (expected %d, got %d)\n%!" name expected actual)

let test_open_waits_for_the_delay () =
  let probe = probe () in
  probe.diagnostics <- [ sample_diagnostic "unexpected token" ];
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/late.jazz" in
  did_open server ~uri ~text:"fn broken( -> { return }";
  check "open: nothing analysed before the delay" (probe.checks = []);
  check "open: nothing published before the delay" (analyse_due server = []);
  advance 0.2;
  check "open: still inside the delay" (analyse_due server = []);
  advance 0.1;
  let packets = analyse_due server in
  check "open: one analysis" (probe.checks = [ uri ]);
  check_int "open: one publish" (List.length (publishes packets)) 1;
  match publishes packets with
  | [ params ] ->
      check "open: the file is named" (string_field "uri" params = Some uri);
      check "open: the document's version" (int_field "version" params = Some 1);
      check_int "open: the full set is published"
        (match diagnostics_field params with
        | Some items -> List.length items
        | None -> 0)
        1
  | _ -> check "open: the publish carries its parameters" false

let close_enough actual expected = Float.abs (actual -. expected) < 0.0001

let test_time_to_next () =
  let probe = probe () in
  let server, _ = create_probe probe in
  let wait_is name expected =
    match (Jide_controller.Server.time_to_next server, expected) with
    | None, None -> ()
    | Some actual, Some expected -> check name (close_enough actual expected)
    | None, Some _ | Some _, None -> check name false
  in
  wait_is "no wait before initialize" None;
  initialize server;
  wait_is "no wait while no document is open" None;
  let uri = "file:///tmp/timed.jazz" in
  did_open server ~uri ~text:"fn f() {\n}\n";
  wait_is "an open document asks the loop to wait for the delay" (Some 0.25);
  advance 0.1;
  wait_is "the wait shrinks as the clock moves" (Some 0.15);
  advance 0.2;
  wait_is "a deadline that has passed asks for no wait" (Some 0.0);
  ignore (analyse_due server);
  wait_is "a published document asks for no wait" None;
  did_change server ~uri ~version:2 ~text:"fn g() {\n}\n";
  wait_is "a change asks for the wait again" (Some 0.25);
  shutdown server;
  wait_is "a server that is shutting down asks for no wait" None

let test_publish_carries_the_full_set () =
  let probe = probe () in
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/two.jazz" in
  did_open server ~uri ~text:"fn f() {\n}\n";
  probe.diagnostics <- [ sample_diagnostic "first"; sample_diagnostic "second" ];
  advance 0.3;
  match publishes (analyse_due server) with
  | [ params ] ->
      check_int "two diagnostics"
        (match diagnostics_field params with
        | Some items -> List.length items
        | None -> 0)
        2;
      check "the message survives"
        (match diagnostics_field params with
        | Some (_ :: second :: _) ->
            field "message" second = Some (`String "second")
        | _ -> false)
  | _ -> check "two diagnostics: one publish" false

let range_start json =
  match field "range" json with
  | Some range -> (
      match field "start" range with
      | Some start -> (int_field "line" start, int_field "character" start)
      | None -> (None, None))
  | None -> (None, None)

(* An error in a file the document requires is published against that file's
   own uri, with the range its own text gives it, and cleared when the
   document no longer reports it. The file is not open in the engine, so the
   clear has to be published: nothing else will ever publish for it. *)
let test_a_required_file_is_published_against_its_own_uri () =
  let probe = probe () in
  let server, _ = create_probe probe in
  initialize server;
  let document = "file:///tmp/entry.jazz" in
  let included = "file:///tmp/consts.jinc" in
  let included_text = "u16 x = MLKEM_NOPE;\n" in
  let report () =
    [
      {
        Jide_controller.Server.uri = included;
        text = included_text;
        diagnostics =
          [
            Jide_lang.Diagnostic.make ~code:"typing"
              ~severity:Jide_lang.Diagnostic.Error
              (Jide_lang.Span.make ~start_byte:8 ~end_byte:18)
              "unknown variable: `MLKEM_NOPE'";
          ];
      };
    ]
  in
  did_open server ~uri:document ~text:"require \"consts.jinc\"\n";
  probe.required <- report ();
  advance 0.3;
  (match publishes (analyse_due server) with
  | [ own; file ] ->
      check "required: the document is published first"
        (string_field "uri" own = Some document);
      check "required: the file is published against its own uri"
        (string_field "uri" file = Some included);
      check "required: the file's set carries no version"
        (int_field "version" file = None);
      check "required: one diagnostic at the file's own range"
        (match diagnostics_field file with
        | Some [ diagnostic ] -> range_start diagnostic = (Some 0, Some 8)
        | _ -> false)
  | _ -> check "required: the file is published too" false);
  (* The file is fixed: the next analysis of the document reports none, and
     the set published for the file is cleared. *)
  probe.required <- [];
  did_change server ~uri:document ~version:2
    ~text:"require \"consts.jinc\"\n\nfn f() -> reg u32 { return 0; }\n";
  advance 0.3;
  match publishes (analyse_due server) with
  | [ own; file ] ->
      check "required: the document is published again"
        (string_field "uri" own = Some document);
      check "required: the cleared file is named"
        (string_field "uri" file = Some included);
      check "required: the cleared set is empty"
        (diagnostics_field file = Some [])
  | _ -> check "required: a fixed include is cleared" false

let test_a_change_restarts_the_wait () =
  let probe = probe () in
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/restart.jazz" in
  did_open server ~uri ~text:"fn f() {\n}\n";
  advance 0.1;
  did_change server ~uri ~version:2 ~text:"fn g() {\n}\n";
  advance 0.2;
  check "a change restarts the wait, so the first deadline does not fire"
    (analyse_due server = []);
  advance 0.1;
  (match publishes (analyse_due server) with
  | [ params ] ->
      check "the restarted deadline publishes the new version"
        (int_field "version" params = Some 2)
  | _ -> check "the restarted deadline publishes once" false);
  check_int "one analysis for one settled document" (List.length probe.checks) 1

let test_two_changes_are_one_publish () =
  let probe = probe () in
  probe.diagnostics <- [ sample_diagnostic "still broken" ];
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/burst.jazz" in
  did_open server ~uri ~text:"fn broken( -> { return }";
  advance 0.3;
  let first = publishes (analyse_due server) in
  check_int "the open published once" (List.length first) 1;
  (* Two keystrokes inside one delay are one analysis, of the last text. *)
  did_change server ~uri ~version:2 ~text:"fn broken2( -> { return }";
  advance 0.05;
  did_change server ~uri ~version:3 ~text:"fn broken3( -> { return }";
  advance 0.1;
  check "two changes inside the delay publish nothing yet"
    (analyse_due server = []);
  advance 0.2;
  let second = publishes (analyse_due server) in
  check_int "two changes publish once" (List.length second) 1;
  (match second with
  | [ params ] ->
      check "the publish is for the last text"
        (int_field "version" params = Some 3)
  | _ -> ());
  check_int "one analysis per settled burst" (List.length probe.checks) 2

let test_close_clears () =
  let probe = probe () in
  probe.diagnostics <- [ sample_diagnostic "unexpected token" ];
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/closing.jazz" in
  did_open server ~uri ~text:"fn broken( -> { return }";
  advance 0.3;
  ignore (analyse_due server);
  let packets =
    send server
      (notification ~params:(close_params ~uri) "textDocument/didClose")
  in
  (match publishes packets with
  | [ params ] ->
      check "close: the file is named" (string_field "uri" params = Some uri);
      check "close: the set is empty" (diagnostics_field params = Some []);
      check "close: the version is the last one"
        (int_field "version" params = Some 1)
  | _ -> check "close: one publish, and it clears" false);
  check "close: the analysis is dropped" (probe.forgotten = [ uri ]);
  advance 1.0;
  check "close: nothing waits any more" (analyse_due server = [])

let test_a_superseded_result_is_dropped () =
  let probe = probe () in
  probe.diagnostics <- [ sample_diagnostic "unexpected token" ];
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/racing.jazz" in
  did_open server ~uri ~text:"fn broken( -> { return }";
  probe.version_offset <- -1;
  advance 0.3;
  check "a result for an older version is not sent" (analyse_due server = []);
  check_int "the analysis still ran" (List.length probe.checks) 1;
  (* The document waits again, and the next deadline publishes the version
     the analysis now reports. *)
  probe.version_offset <- 0;
  advance 0.3;
  match publishes (analyse_due server) with
  | [ params ] ->
      check "the fresh result is sent" (int_field "version" params = Some 1)
  | _ -> check "the fresh result is published once" false

let test_ensure_checked_forces_a_check () =
  let probe = probe () in
  probe.diagnostics <- [ sample_diagnostic "unexpected token" ];
  let server, _ = create_probe probe in
  initialize server;
  let uri = "file:///tmp/requested.jazz" in
  did_open server ~uri ~text:"fn broken( -> { return }";
  let packets = Jide_controller.Server.ensure_checked server ~uri in
  check_int "a request forces one publish" (List.length (publishes packets)) 1;
  check_int "a request forces one analysis" (List.length probe.checks) 1;
  check "a second request finds the document fresh"
    (Jide_controller.Server.ensure_checked server ~uri = []);
  did_change server ~uri ~version:2 ~text:"fn broken2( -> { return }";
  (match publishes (Jide_controller.Server.ensure_checked server ~uri) with
  | [ params ] ->
      check "a changed document is checked on demand"
        (int_field "version" params = Some 2)
  | _ -> check "a changed document is published once" false);
  check "the forced check ends the wait, so nothing is published twice"
    (analyse_due server = [])

let test_without_analysis_nothing_is_published () =
  let server = create () in
  initialize server;
  let uri = "file:///tmp/plain.jazz" in
  did_open server ~uri ~text:"fn f() {\n}\n";
  advance 1.0;
  check "no analysis: nothing to publish" (analyse_due server = []);
  let packets =
    send server
      (notification ~params:(close_params ~uri) "textDocument/didClose")
  in
  check "no analysis: nothing to clear" (packets = [])

let test_a_failing_analysis_keeps_serving () =
  let messages = ref [] in
  let log message = messages := message :: !messages in
  let probe = probe () in
  probe.raises <- true;
  let server, _ = create_probe ~log probe in
  initialize server;
  did_open server ~uri:"file:///tmp/failing.jazz" ~text:"fn f() {\n}\n";
  advance 0.3;
  check "a failing analysis publishes nothing" (analyse_due server = []);
  check "a failing analysis is logged"
    (List.exists (fun message -> contains message "analysis failed") !messages);
  check_int "the server still answers"
    (List.length (send server (request 2 "shutdown")))
    1

let test_delay_is_clamped () =
  let delay_of server = Jide_controller.Server.delay server in
  check_int "the default delay is 250 ms" (delay_of (create ())) 250;
  check_int "a delay below the range is raised"
    (delay_of (Jide_controller.Server.create ~log:(fun _ -> ()) ~delay:1 ()))
    50;
  check_int "a delay above the range is lowered"
    (delay_of (Jide_controller.Server.create ~log:(fun _ -> ()) ~delay:9000 ()))
    1000;
  check_int "a delay inside the range is kept"
    (delay_of (Jide_controller.Server.create ~log:(fun _ -> ()) ~delay:400 ()))
    400

let test_settings_set_the_delay () =
  let section delay =
    `Assoc [ ("diagnostics", `Assoc [ ("delay", `Int delay) ]) ]
  in
  let server = create () in
  Jide_controller.Server.apply_settings server
    (`Assoc [ ("jasminIde", section 600) ]);
  check_int "the settings set the delay"
    (Jide_controller.Server.delay server)
    600;
  Jide_controller.Server.apply_settings server (section 10);
  check_int "a setting below the range is raised"
    (Jide_controller.Server.delay server)
    50;
  Jide_controller.Server.apply_settings server
    (`Assoc [ ("capabilities", `Assoc []) ]);
  check_int "a settings object without a delay changes nothing"
    (Jide_controller.Server.delay server)
    50

let test_initialize_options_set_the_delay () =
  let server = create () in
  let params =
    `Assoc
      [
        ("capabilities", `Assoc []);
        ( "initializationOptions",
          `Assoc [ ("diagnostics", `Assoc [ ("delay", `Int 320) ]) ] );
      ]
  in
  ignore (send server (request ~params 1 "initialize"));
  check_int "the initialization options set the delay"
    (Jide_controller.Server.delay server)
    320

(* The request has one shape, so a request carrying one of the names the
   reader used to take as well must leave the delay where it was. Each shape
   gets its own server, because the second initialize of a session is
   refused. *)
let test_initialize_options_take_one_name () =
  let delay_after options =
    let server = create () in
    let params =
      `Assoc [ ("capabilities", `Assoc []); ("initializationOptions", options) ]
    in
    ignore (send server (request ~params 1 "initialize"));
    Jide_controller.Server.delay server
  in
  check_int "a flat delay in the initialization options is not read"
    (delay_after (`Assoc [ ("delay", `Int 500) ]))
    250;
  check_int "the configuration section's name in the options is not read"
    (delay_after
       (`Assoc
         [
           ( "jasminIde",
             `Assoc [ ("diagnostics", `Assoc [ ("delay", `Int 620) ]) ] );
         ]))
    250;
  check_int "the old flat name in the initialization options is not read"
    (delay_after (`Assoc [ ("diagnosticsDelay", `Int 500) ]))
    250;
  check_int "an architecture and an include map alone change no delay"
    (delay_after
       (`Assoc
         [
           ("arch", `String "x86-64");
           ("include", `Assoc [ ("SECP", `String "src/jasmin") ]);
         ]))
    250

let test_configuration_sets_the_delay () =
  let server = create () in
  initialize server;
  let params =
    `Assoc
      [
        ("settings", `Assoc [ ("diagnostics", `Assoc [ ("delay", `Int 700) ]) ]);
      ]
  in
  ignore (send server (notification ~params "workspace/didChangeConfiguration"));
  check_int "a change of configuration sets the delay"
    (Jide_controller.Server.delay server)
    700

let test_nothing_is_published_after_shutdown () =
  let probe = probe () in
  probe.diagnostics <- [ sample_diagnostic "unexpected token" ];
  let server, _ = create_probe probe in
  initialize server;
  did_open server ~uri:"file:///tmp/short.jazz" ~text:"fn broken( -> { return }";
  shutdown server;
  advance 1.0;
  check "after shutdown no check runs" (analyse_due server = []);
  check "after shutdown nothing was analysed" (probe.checks = [])

(* The four navigation requests. The answers come from a locate the test
   writes down, so the encoding of a location, of a hover and of a symbol tree
   is checked without an engine that can find one, and every call is recorded
   so a request that must not reach the engine can be seen not to have. The
   analysis probe is the same one the publish path uses, so the forced check a
   request runs is the check a real one runs. *)

type locate_probe = {
  mutable definition_calls : (string * Jide_lang.Position.t) list;
  mutable reference_calls : (string * Jide_lang.Position.t * bool) list;
  mutable hover_calls : (string * Jide_lang.Position.t) list;
  mutable symbol_calls : string list;
  mutable seen_at_call : (int * int) list;
      (** What the check had done when the answer was asked for: how many
          analyses had run, and the version of the text the last one read.
          [(0, 0)] is a request that answered before its check. *)
  mutable definitions : Jide_engine.Locate.location list;
  mutable references : Jide_engine.Locate.location list;
  mutable hover : Jide_engine.Locate.hover option;
  mutable symbols : Jide_engine.Locate.symbol list;
}

let locate_probe () =
  {
    definition_calls = [];
    reference_calls = [];
    hover_calls = [];
    symbol_calls = [];
    seen_at_call = [];
    definitions = [];
    references = [];
    hover = None;
    symbols = [];
  }

let fake_locate ~analysis (probe : locate_probe) : Jide_controller.Server.locate
    =
  let seen () =
    let checks, version =
      (List.length analysis.checks, analysis.checked_version)
    in
    probe.seen_at_call <- probe.seen_at_call @ [ (checks, version) ]
  in
  {
    Jide_controller.Server.definition =
      (fun ~uri ~position ->
        seen ();
        probe.definition_calls <- probe.definition_calls @ [ (uri, position) ];
        probe.definitions);
    references =
      (fun ~uri ~position ~include_declaration ->
        seen ();
        probe.reference_calls <-
          probe.reference_calls @ [ (uri, position, include_declaration) ];
        probe.references);
    hover =
      (fun ~uri ~position ->
        seen ();
        probe.hover_calls <- probe.hover_calls @ [ (uri, position) ];
        probe.hover);
    symbols =
      (fun ~uri ->
        seen ();
        probe.symbol_calls <- probe.symbol_calls @ [ uri ];
        probe.symbols);
  }

let position line character = Jide_lang.Position.make ~line ~character

let locate_range ~start_line ~start_character ~end_line ~end_character =
  {
    Jide_engine.Locate.start_ = position start_line start_character;
    end_ = position end_line end_character;
  }

let locate_location ~uri ~start_line ~start_character ~end_line ~end_character =
  {
    Jide_engine.Locate.uri;
    range = locate_range ~start_line ~start_character ~end_line ~end_character;
  }

let range_json ~start_line ~start_character ~end_line ~end_character =
  `Assoc
    [
      ( "start",
        `Assoc
          [ ("line", `Int start_line); ("character", `Int start_character) ] );
      ( "end",
        `Assoc [ ("line", `Int end_line); ("character", `Int end_character) ] );
    ]

let location_json ~uri ~start_line ~start_character ~end_line ~end_character =
  `Assoc
    [
      ("uri", `String uri);
      ("range", range_json ~start_line ~start_character ~end_line ~end_character);
    ]

let target_params ~uri ~line ~character =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
    ]

let references_params ~uri ~line ~character ~include_declaration =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
      ("context", `Assoc [ ("includeDeclaration", `Bool include_declaration) ]);
    ]

let document_symbol_params ~uri =
  `Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ]

(* A request whose parameters are absent rather than wrong: the protocol
   allows the member to be missing, and the server has to refuse that too. *)
let request_without_params id method_ =
  Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id:(`Int id) ~method_ ())

let markdown value = { Jide_engine.Types.value }

(* A reply to a navigation request is the second packet: the forced check's
   publish comes first. Both are checked, so a request that skipped the check
   and a request that sent the reply first are both failures. *)
let check_checked_reply name ~version reply =
  match reply with
  | [ publish; reply ] -> (
      (match publishes [ publish ] with
      | [ params ] ->
          check
            (name ^ ": the check publishes first")
            (int_field "version" params = Some version)
      | _ -> check (name ^ ": one publish first") false);
      match result_json reply with
      | Some result -> result
      | None ->
          check (name ^ ": the reply carries a result") false;
          `Null)
  | _ ->
      check (name ^ ": a publish and a reply") false;
      `Null

let test_definition_answer () =
  let uri = "file:///tmp/definition.jazz" in
  let locate = locate_probe () in
  locate.definitions <-
    [
      locate_location ~uri:"file:///tmp/other.jazz" ~start_line:2
        ~start_character:3 ~end_line:2 ~end_character:10;
    ];
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:1 ~character:2)
         2 "textDocument/definition")
  in
  let result = check_checked_reply "definition" ~version:1 replies in
  check "definition: a location array"
    (result
    = `List
        [
          location_json ~uri:"file:///tmp/other.jazz" ~start_line:2
            ~start_character:3 ~end_line:2 ~end_character:10;
        ]);
  check "definition: the engine was asked where the request said"
    (locate.definition_calls = [ (uri, position 1 2) ])

let test_definition_without_an_answer () =
  let uri = "file:///tmp/nothing.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:1 ~character:2)
         2 "textDocument/definition")
  in
  let result =
    check_checked_reply "definition without an answer" ~version:1 replies
  in
  check "definition without an answer: null, not an error" (result = `Null)

let test_references_answer () =
  let uri = "file:///tmp/references.jazz" in
  let locate = locate_probe () in
  locate.references <-
    [
      locate_location ~uri ~start_line:1 ~start_character:1 ~end_line:1
        ~end_character:2;
      locate_location ~uri ~start_line:1 ~start_character:9 ~end_line:1
        ~end_character:10;
    ];
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:
           (references_params ~uri ~line:1 ~character:2
              ~include_declaration:true)
         2 "textDocument/references")
  in
  let result = check_checked_reply "references" ~version:1 replies in
  check "references: every location, in order"
    (result
    = `List
        [
          location_json ~uri ~start_line:1 ~start_character:1 ~end_line:1
            ~end_character:2;
          location_json ~uri ~start_line:1 ~start_character:9 ~end_line:1
            ~end_character:10;
        ]);
  check "references: the context reaches the engine"
    (locate.reference_calls = [ (uri, position 1 2, true) ])

let test_references_without_an_answer () =
  let uri = "file:///tmp/no-references.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:
           (references_params ~uri ~line:1 ~character:2
              ~include_declaration:false)
         2 "textDocument/references")
  in
  let result =
    check_checked_reply "references without an answer" ~version:1 replies
  in
  check "references without an answer: an empty list, not an error"
    (result = `List []);
  check "references: the default is not assumed"
    (locate.reference_calls = [ (uri, position 1 2, false) ])

let test_hover_answer () =
  let uri = "file:///tmp/hover.jazz" in
  let locate = locate_probe () in
  locate.hover <-
    Some
      {
        Jide_engine.Locate.range =
          Some
            (locate_range ~start_line:1 ~start_character:2 ~end_line:1
               ~end_character:3);
        contents = markdown "reg u32";
      };
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:1 ~character:2)
         2 "textDocument/hover")
  in
  let result = check_checked_reply "hover" ~version:1 replies in
  check "hover: markup content and range"
    (result
    = `Assoc
        [
          ( "contents",
            `Assoc
              [ ("kind", `String "markdown"); ("value", `String "reg u32") ] );
          ( "range",
            range_json ~start_line:1 ~start_character:2 ~end_line:1
              ~end_character:3 );
        ])

let test_hover_without_an_answer () =
  let uri = "file:///tmp/no-hover.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:1 ~character:2)
         2 "textDocument/hover")
  in
  let result =
    check_checked_reply "hover without an answer" ~version:1 replies
  in
  check "hover without an answer: null, not an error" (result = `Null)

let test_document_symbols () =
  let uri = "file:///tmp/symbols.jazz" in
  let locate = locate_probe () in
  locate.symbols <-
    [
      {
        Jide_engine.Locate.name = "f";
        detail = Some "fn f() -> reg u32";
        kind = Jide_engine.Types.Function;
        range =
          locate_range ~start_line:0 ~start_character:0 ~end_line:2
            ~end_character:1;
        selection_range =
          locate_range ~start_line:0 ~start_character:3 ~end_line:0
            ~end_character:4;
        children =
          [
            {
              Jide_engine.Locate.name = "x";
              detail = None;
              kind = Jide_engine.Types.Variable;
              range =
                locate_range ~start_line:1 ~start_character:2 ~end_line:1
                  ~end_character:3;
              selection_range =
                locate_range ~start_line:1 ~start_character:2 ~end_line:1
                  ~end_character:3;
              children = [];
            };
          ];
      };
    ];
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  let replies =
    send server
      (request
         ~params:(document_symbol_params ~uri)
         2 "textDocument/documentSymbol")
  in
  let result = check_checked_reply "documentSymbol" ~version:1 replies in
  check "documentSymbol: the hierarchy, with both ranges"
    (result
    = `List
        [
          `Assoc
            [
              ("name", `String "f");
              ("kind", `Int 12);
              ( "range",
                range_json ~start_line:0 ~start_character:0 ~end_line:2
                  ~end_character:1 );
              ( "selectionRange",
                range_json ~start_line:0 ~start_character:3 ~end_line:0
                  ~end_character:4 );
              ("detail", `String "fn f() -> reg u32");
              ( "children",
                `List
                  [
                    `Assoc
                      [
                        ("name", `String "x");
                        ("kind", `Int 13);
                        ( "range",
                          range_json ~start_line:1 ~start_character:2
                            ~end_line:1 ~end_character:3 );
                        ( "selectionRange",
                          range_json ~start_line:1 ~start_character:2
                            ~end_line:1 ~end_character:3 );
                      ];
                  ] );
            ];
        ])

let test_document_symbols_without_items () =
  let uri = "file:///tmp/empty-symbols.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"\n\n";
  let replies =
    send server
      (request
         ~params:(document_symbol_params ~uri)
         2 "textDocument/documentSymbol")
  in
  let result =
    check_checked_reply "documentSymbol without items" ~version:1 replies
  in
  check "documentSymbol without items: an empty list, not an error"
    (result = `List [])

(* Whitespace and past the end of the file are positions, not errors: the
   request is answered, the engine is asked at the position the client wrote,
   and the empty answer it gives is encoded as null or []. *)
let test_positions_outside_the_text () =
  let uri = "file:///tmp/outside.jazz" in
  let text = "fn f() -> reg u32 {\n  return 0;\n}\n" in
  let run method_ ~params =
    let locate = locate_probe () in
    let probe = probe () in
    let server, _ =
      create_probe ~locate:(fake_locate ~analysis:probe locate) probe
    in
    initialize server;
    did_open server ~uri ~text;
    let replies = send server (request ~params 2 method_) in
    ( locate,
      check_checked_reply (method_ ^ " outside the text") ~version:1 replies )
  in
  (* Line 1 is "  return 0;": character 1 is a space. *)
  let locate, result =
    run "textDocument/definition"
      ~params:(target_params ~uri ~line:1 ~character:1)
  in
  check "definition in whitespace: null" (result = `Null);
  check "definition in whitespace: the engine sees the position"
    (locate.definition_calls = [ (uri, position 1 1) ]);
  let locate, result =
    run "textDocument/hover" ~params:(target_params ~uri ~line:1 ~character:1)
  in
  check "hover in whitespace: null" (result = `Null);
  check "hover in whitespace: the engine sees the position"
    (locate.hover_calls = [ (uri, position 1 1) ]);
  let locate, result =
    run "textDocument/definition"
      ~params:(target_params ~uri ~line:99 ~character:0)
  in
  check "definition past the end: null" (result = `Null);
  check "definition past the end: the engine sees the position"
    (locate.definition_calls = [ (uri, position 99 0) ]);
  let locate, result =
    run "textDocument/references"
      ~params:
        (references_params ~uri ~line:99 ~character:0 ~include_declaration:true)
  in
  check "references past the end: an empty list" (result = `List []);
  check "references past the end: the engine sees the position"
    (locate.reference_calls = [ (uri, position 99 0, true) ])

(* A document the server does not hold has no stale text, so the request is
   answered without a publish and without an error. *)
let test_navigation_for_an_unopened_document () =
  let uri = "file:///tmp/never-opened.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:0 ~character:0)
         2 "textDocument/definition")
  in
  check "an unopened document: one reply, no publish"
    (match replies with
    | [ reply ] -> result_json reply = Some `Null
    | _ -> false);
  check "an unopened document: nothing was analysed" (probe.checks = [])

(* The answer is for the text the client has: a change the debounce has not
   published yet is checked before the reply, and the publish names the new
   version. *)
let test_a_navigation_request_checks_stale_text () =
  let uri = "file:///tmp/stale.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  did_change server ~uri ~version:2
    ~text:"fn g() -> reg u32 {\n  return 1;\n}\n";
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:1 ~character:2)
         2 "textDocument/hover")
  in
  let result = check_checked_reply "a stale document" ~version:2 replies in
  check "a stale document: the answer is for the new text" (result = `Null);
  check_int "a stale document: the request ran the one check"
    (List.length probe.checks) 1;
  (* The check has to have run before the engine was asked, and the text it
     read has to be the text the answer is about: one check, of version 2.
     [(0, 0)] is an answer computed from the index the previous text left. *)
  check "a stale document: the engine was asked after the check"
    (locate.seen_at_call = [ (1, 2) ]);
  check "a stale document: no work is left for the loop"
    (analyse_due server = [])

let test_a_dropped_check_still_answers () =
  let uri = "file:///tmp/dropped.jazz" in
  let locate = locate_probe () in
  let probe = probe () in
  let server, _ =
    create_probe ~locate:(fake_locate ~analysis:probe locate) probe
  in
  initialize server;
  did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
  probe.version_offset <- -1;
  let replies =
    send server
      (request
         ~params:(target_params ~uri ~line:1 ~character:2)
         2 "textDocument/hover")
  in
  check "a dropped check: one reply, no publish"
    (match replies with
    | [ reply ] -> result_json reply = Some `Null
    | _ -> false);
  check_int "a dropped check: the request ran the one check"
    (List.length probe.checks) 1;
  check "a dropped check: the engine was asked after the check"
    (locate.seen_at_call = [ (1, 1) ])

(* A parameters object that does not parse is InvalidParams, and it never
   reaches the engine or costs a check. *)
let test_navigation_rejects_bad_params () =
  let uri = "file:///tmp/strict.jazz" in
  let bad_params =
    [
      ( "definition without params",
        "textDocument/definition",
        None,
        "params must be an object" );
      ( "definition without position",
        "textDocument/definition",
        Some (`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ]),
        "position is missing" );
      ( "definition with a string line",
        "textDocument/definition",
        Some
          (`Assoc
            [
              ("textDocument", `Assoc [ ("uri", `String uri) ]);
              ( "position",
                `Assoc [ ("line", `String "0"); ("character", `Int 0) ] );
            ]),
        "position.line must be an integer" );
      ( "definition with a negative character",
        "textDocument/definition",
        Some
          (`Assoc
            [
              ("textDocument", `Assoc [ ("uri", `String uri) ]);
              ("position", `Assoc [ ("line", `Int 0); ("character", `Int (-1)) ]);
            ]),
        "position.character must not be negative" );
      ( "definition without a uri",
        "textDocument/definition",
        Some
          (`Assoc
            [
              ("textDocument", `Assoc []);
              ("position", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ]),
        "textDocument.uri is missing" );
      ( "hover with a non-object position",
        "textDocument/hover",
        Some
          (`Assoc
            [
              ("textDocument", `Assoc [ ("uri", `String uri) ]);
              ("position", `Int 0);
            ]),
        "position must be an object" );
      ( "references without context",
        "textDocument/references",
        Some (target_params ~uri ~line:0 ~character:0),
        "context is missing" );
      ( "references with a string context",
        "textDocument/references",
        Some
          (`Assoc
            [
              ("textDocument", `Assoc [ ("uri", `String uri) ]);
              ("position", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("context", `Assoc [ ("includeDeclaration", `String "yes") ]);
            ]),
        "context.includeDeclaration must be a boolean" );
      ( "documentSymbol without a text document",
        "textDocument/documentSymbol",
        Some (`Assoc []),
        "textDocument is missing" );
    ]
  in
  List.iter
    (fun (name, method_, params, message) ->
      let locate = locate_probe () in
      let probe = probe () in
      let server, _ =
        create_probe ~locate:(fake_locate ~analysis:probe locate) probe
      in
      initialize server;
      did_open server ~uri ~text:"fn f() -> reg u32 {\n  return 0;\n}\n";
      let packet =
        match params with
        | None -> request_without_params 2 method_
        | Some params -> request ~params 2 method_
      in
      let replies = send server packet in
      check_single_error name Jsonrpc.Response.Error.Code.InvalidParams replies;
      (match replies with
      | [ reply ] ->
          check
            (name ^ ": the message names the member")
            (match
               match reply with
               | Jsonrpc.Packet.Response { result = Error error; _ } ->
                   Some error
               | _ -> None
             with
            | Some error ->
                String.equal error.Jsonrpc.Response.Error.message message
            | None -> false)
      | _ -> ());
      check
        (name ^ ": the engine was not asked")
        (locate.definition_calls = []
        && locate.reference_calls = []
        && locate.hover_calls = [] && locate.symbol_calls = []);
      check (name ^ ": nothing was checked") (probe.checks = []))
    bad_params

(* The eleven are added, not the protocol's whole list: a request this server
   does not implement is still MethodNotFound. A method it does route — the
   four navigation ones and the seven editing ones — is answered, and the
   tests above and beside this one pin those; a code lens is in the protocol
   and in the `lsp` package's request type, and this server answers nothing
   for it. *)
let test_other_protocol_requests_are_still_unknown () =
  let server = create () in
  initialize server;
  let replies =
    send server
      (request
         ~params:(document_symbol_params ~uri:"file:///tmp/a.jazz")
         2 "textDocument/codeLens")
  in
  check_single_error "code lens is not advertised or answered"
    Jsonrpc.Response.Error.Code.MethodNotFound replies

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
  test_open_waits_for_the_delay ();
  test_time_to_next ();
  test_publish_carries_the_full_set ();
  test_a_required_file_is_published_against_its_own_uri ();
  test_a_change_restarts_the_wait ();
  test_two_changes_are_one_publish ();
  test_close_clears ();
  test_a_superseded_result_is_dropped ();
  test_ensure_checked_forces_a_check ();
  test_without_analysis_nothing_is_published ();
  test_a_failing_analysis_keeps_serving ();
  test_delay_is_clamped ();
  test_settings_set_the_delay ();
  test_initialize_options_set_the_delay ();
  test_initialize_options_take_one_name ();
  test_configuration_sets_the_delay ();
  test_nothing_is_published_after_shutdown ();
  test_definition_answer ();
  test_definition_without_an_answer ();
  test_references_answer ();
  test_references_without_an_answer ();
  test_hover_answer ();
  test_hover_without_an_answer ();
  test_document_symbols ();
  test_document_symbols_without_items ();
  test_positions_outside_the_text ();
  test_navigation_for_an_unopened_document ();
  test_a_navigation_request_checks_stale_text ();
  test_a_dropped_check_still_answers ();
  test_navigation_rejects_bad_params ();
  test_other_protocol_requests_are_still_unknown ();
  if !failures = 0 then print_endline "all server tests passed"
  else (
    Printf.printf "%d server test(s) failed\n" !failures;
    exit 1)
