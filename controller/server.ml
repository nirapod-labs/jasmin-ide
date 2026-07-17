type state = Uninitialized | Running | Shutting_down | Exited

type t = {
  engine : Jide_engine.Engine.t;
  log : string -> unit;
  mutable state : state;
  mutable exit_code : int;
  mutable shutdown_requested : bool;
}

let create ~log =
  {
    engine = Jide_engine.Engine.create ();
    log;
    state = Uninitialized;
    exit_code = 1;
    shutdown_requested = false;
  }

let normalize_uri uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_string uri)

(* The per-handler catch-all: a handler that raises must not end the server,
   and the log line must be able to say what failed, so the exception is
   returned as text rather than dropped. *)
let guarded f = try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let state_name = function
  | Uninitialized -> "uninitialized"
  | Running -> "running"
  | Shutting_down -> "shutting down"
  | Exited -> "exited"

let state t = t.state
let exit_code t = t.exit_code
let exited t = t.state = Exited
let engine t = t.engine
let reply_ok id json = Jsonrpc.Packet.Response (Jsonrpc.Response.ok id json)

let reply_error id code message =
  Jsonrpc.Packet.Response
    (Jsonrpc.Response.error id (Jsonrpc.Response.Error.make ~code ~message ()))

let error_code = function
  | `MethodNotFound -> Jsonrpc.Response.Error.Code.MethodNotFound
  | `InvalidRequest -> Jsonrpc.Response.Error.Code.InvalidRequest
  | `InvalidParams -> Jsonrpc.Response.Error.Code.InvalidParams
  | `InternalError -> Jsonrpc.Response.Error.Code.InternalError
  | `ServerNotInitialized -> Jsonrpc.Response.Error.Code.ServerNotInitialized
  | `RequestCancelled -> Jsonrpc.Response.Error.Code.RequestCancelled

let server_info () =
  `Assoc
    [
      ("name", `String Version.server_name); ("version", `String Version.version);
    ]

let capabilities () = `Assoc [ ("textDocumentSync", `Int 1) ]

let initialize_result () =
  `Assoc [ ("capabilities", capabilities ()); ("serverInfo", server_info ()) ]

let root_of_initialize (params : Lsp.Types.InitializeParams.t) =
  match params.rootUri with
  | Some uri -> (
      match
        Jide_lang.Uri.path (Jide_lang.Uri.of_string (Lsp.Uri.to_string uri))
      with
      | Some path -> Some path
      | None -> Option.join params.rootPath)
  | None -> Option.join params.rootPath

let handle_initialize t (params : Lsp.Types.InitializeParams.t) =
  match t.state with
  | Uninitialized ->
      Jide_engine.Engine.set_root t.engine (root_of_initialize params);
      t.state <- Running;
      t.log "initialized";
      Some (initialize_result ())
  | Running | Shutting_down | Exited -> None

let handle_shutdown t =
  match t.state with
  | Uninitialized -> Error `ServerNotInitialized
  | Running ->
      t.state <- Shutting_down;
      t.shutdown_requested <- true;
      t.log "shutdown requested";
      Ok ()
  | Shutting_down | Exited -> Error `InvalidRequest

let handle_exit t =
  t.exit_code <- (if t.shutdown_requested then 0 else 1);
  t.state <- Exited;
  t.log "exiting"

(* Everything below changes state, so the caller runs it only while the
   server is [Running]: before [initialize] and after [shutdown] the client
   has no business sending it, and applying it would leave the store holding
   documents the client believes it closed. [exit] and [$/cancelRequest] are
   the two notifications the protocol allows in every state, and they are
   handled before this point. *)
let dispatch_notification t (notification : Lsp.Client_notification.t) =
  match notification with
  | Lsp.Client_notification.TextDocumentDidOpen params ->
      let doc = params.textDocument in
      let uri = normalize_uri (Lsp.Uri.to_string doc.uri) in
      Jide_engine.Engine.open_document t.engine ~uri ~version:doc.version
        ~text:doc.text;
      []
  | Lsp.Client_notification.TextDocumentDidChange params ->
      let doc = params.textDocument in
      let uri = normalize_uri (Lsp.Uri.to_string doc.uri) in
      (match List.rev params.contentChanges with
      | [] -> t.log "didChange without changes: the document is untouched"
      | change :: _ -> (
          match change.range with
          | Some _ ->
              t.log
                "didChange with a range ignored: the client promised full sync"
          | None -> (
              match Jide_engine.Engine.document t.engine ~uri with
              | None -> t.log ("didChange for an unopened document: " ^ uri)
              | Some _ ->
                  Jide_engine.Engine.change_document t.engine ~uri
                    ~version:doc.version ~text:change.text)));
      []
  | Lsp.Client_notification.TextDocumentDidClose params ->
      Jide_engine.Engine.close_document t.engine
        ~uri:(normalize_uri (Lsp.Uri.to_string params.textDocument.uri));
      []
  | Lsp.Client_notification.Initialized -> []
  | Lsp.Client_notification.DidSaveTextDocument _
  | Lsp.Client_notification.ChangeConfiguration _
  | Lsp.Client_notification.SetTrace _ ->
      []
  | Lsp.Client_notification.UnknownNotification { method_; _ } ->
      t.log ("unknown notification: " ^ method_);
      []
  | unhandled ->
      t.log
        ("notification not handled: "
       ^ (Lsp.Client_notification.to_jsonrpc unhandled).method_);
      []

let handle_notification t (notification : Jsonrpc.Notification.t) =
  match Lsp.Client_notification.of_jsonrpc notification with
  | Error message ->
      t.log
        ("ignored notification: " ^ message ^ " (the server is "
       ^ state_name t.state ^ ")");
      []
  | Ok Lsp.Client_notification.Exit ->
      handle_exit t;
      []
  | Ok (Lsp.Client_notification.CancelRequest _) ->
      t.log "cancel request";
      []
  | Ok _ when t.state <> Running ->
      t.log
        ("notification dropped: the server is " ^ state_name t.state
       ^ ", not running: " ^ notification.method_);
      []
  | Ok client_notification -> (
      match guarded (fun () -> dispatch_notification t client_notification) with
      | Ok packets -> packets
      | Error message ->
          t.log
            ("notification handler failed: " ^ notification.method_ ^ ": "
           ^ message);
          [])

let handle_initialized_request t request =
  match Lsp.Client_request.of_jsonrpc request with
  | Error message ->
      [ reply_error request.id (error_code `InvalidRequest) message ]
  | Ok (E (Lsp.Client_request.Initialize _)) ->
      [
        reply_error request.id
          (error_code `InvalidRequest)
          "the server is already initialized";
      ]
  | Ok (E Lsp.Client_request.Shutdown) -> (
      match handle_shutdown t with
      | Ok () -> [ reply_ok request.id `Null ]
      | Error `ServerNotInitialized ->
          [
            reply_error request.id
              (error_code `ServerNotInitialized)
              "the server is not initialized";
          ]
      | Error `InvalidRequest ->
          [
            reply_error request.id
              (error_code `InvalidRequest)
              "the server is shutting down";
          ]
      | Error _ ->
          [
            reply_error request.id
              (error_code `InternalError)
              "unexpected shutdown state";
          ])
  | Ok (E (Lsp.Client_request.UnknownRequest { meth; _ })) ->
      [
        reply_error request.id
          (error_code `MethodNotFound)
          ("unknown request: " ^ meth);
      ]
  | Ok (E _) ->
      [
        reply_error request.id
          (error_code `MethodNotFound)
          ("unsupported request: " ^ request.method_);
      ]

let dispatch_request t (request : Jsonrpc.Request.t) =
  if String.equal request.method_ "initialize" then
    match Lsp.Client_request.of_jsonrpc request with
    | Error message ->
        [ reply_error request.id (error_code `InvalidRequest) message ]
    | Ok (E (Lsp.Client_request.Initialize params)) -> (
        match handle_initialize t params with
        | Some result -> [ reply_ok request.id result ]
        | None ->
            [
              reply_error request.id
                (error_code `InvalidRequest)
                "the server is already initialized";
            ])
    | Ok (E _) ->
        [
          reply_error request.id
            (error_code `InvalidRequest)
            "not an initialize";
        ]
  else
    match t.state with
    | Uninitialized ->
        [
          reply_error request.id
            (error_code `ServerNotInitialized)
            "the server is not initialized";
        ]
    | Shutting_down | Exited ->
        [
          reply_error request.id
            (error_code `InvalidRequest)
            "the server is shutting down";
        ]
    | Running -> handle_initialized_request t request

let handle_request t (request : Jsonrpc.Request.t) =
  match guarded (fun () -> dispatch_request t request) with
  | Ok packets -> packets
  | Error message ->
      t.log ("request handler failed: " ^ request.method_ ^ ": " ^ message);
      [
        reply_error request.id
          (error_code `InternalError)
          "the request handler failed";
      ]

let handle_packet t packet =
  match packet with
  | Jsonrpc.Packet.Request request -> handle_request t request
  | Jsonrpc.Packet.Notification notification ->
      handle_notification t notification
  | Jsonrpc.Packet.Response _ ->
      t.log "ignored response packet";
      []
  | Jsonrpc.Packet.Batch_call _ | Jsonrpc.Packet.Batch_response _ ->
      t.log "ignored batch packet";
      []

let eof_exit_code t = if t.shutdown_requested then 0 else 1
