type state = Uninitialized | Running | Shutting_down | Exited

type analysis = {
  check : uri:string -> unit;
  diagnostics : uri:string -> (int * Jide_lang.Diagnostic.t list) option;
  forget : uri:string -> unit;
}
(** What the server asks for when it analyses a document.

    [check ~uri] analyses the document's current text and stores the result;
    [diagnostics ~uri] is that result, the version of the text it describes
    and its full set of diagnostics, or [None] when nothing is stored;
    [forget ~uri] drops what a closed document left behind. The engine
    satisfies this, and so does the stand-in the executable wires today, so
    the server itself has no opinion about how a document is analysed. *)

type locate = {
  definition :
    uri:string ->
    position:Jide_lang.Position.t ->
    Jide_engine.Locate.location list;
  references :
    uri:string ->
    position:Jide_lang.Position.t ->
    include_declaration:bool ->
    Jide_engine.Locate.location list;
  hover :
    uri:string ->
    position:Jide_lang.Position.t ->
    Jide_engine.Locate.hover option;
  symbols : uri:string -> Jide_engine.Locate.symbol list;
}
(** How the four navigation requests are answered; the interface's comment
    says what each answer is. *)

let engine_locate (engine : Jide_engine.Engine.t) : locate =
  {
    definition =
      (fun ~uri ~position ->
        Jide_engine.Locate.definition engine ~uri ~position);
    references =
      (fun ~uri ~position ~include_declaration ->
        Jide_engine.Locate.references engine ~uri ~position ~include_declaration);
    hover =
      (fun ~uri ~position -> Jide_engine.Locate.hover engine ~uri ~position);
    symbols = (fun ~uri -> Jide_engine.Locate.symbols engine ~uri);
  }

type t = {
  engine : Jide_engine.Engine.t;
  features : Features.t;
  log : string -> unit;
  analysis : analysis option;
  locate : locate;
  debounce : Debounce.t;
  mutable delay : int;
  now : unit -> float;
  mutable state : state;
  mutable exit_code : int;
  mutable shutdown_requested : bool;
}

let default_delay = 250
let min_delay = 50
let max_delay = 1000
let clamp_delay delay = max min_delay (min max_delay delay)

let create ~log ?engine ?(features = Features.unavailable) ?analysis ?locate
    ?(delay = default_delay) ?(now = fun () -> Unix.gettimeofday ()) () =
  let engine =
    match engine with
    | Some engine -> engine
    | None -> Jide_engine.Engine.create ()
  in
  {
    engine;
    features;
    log;
    analysis;
    locate =
      (match locate with Some locate -> locate | None -> engine_locate engine);
    debounce = Debounce.create ();
    delay = clamp_delay delay;
    now;
    state = Uninitialized;
    exit_code = 1;
    shutdown_requested = false;
  }

let delay t = t.delay
let normalize_uri uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_string uri)

(* The packet's own JSON, back as the plain value the settings reader takes.
   A request's parameters are held in a value type of the protocol package's
   own, which wraps plain JSON and nothing else; a packet that carries none
   reads as null. *)
let json_of_parameters (parameters : Jsonrpc.Structured.t option) :
    Yojson.Safe.t =
  match parameters with
  | None -> `Null
  | Some (`Assoc fields) -> `Assoc fields
  | Some (`List items) -> `List items

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
  | `RequestFailed -> Jsonrpc.Response.Error.Code.RequestFailed

let server_info () =
  `Assoc
    [
      ("name", `String Version.server_name); ("version", `String Version.version);
    ]

(* What this server answers, and nothing else: the four navigation requests
   and the seven editing requests of [Features]. Each navigation provider is a
   plain [true]: the protocol lets a server give an object with options, and
   this one has no option to give — a definition is a location, a hover is
   markdown, and neither has a registered kind or a trigger character to
   declare. The seven editing requests are advertised with the shape each one
   needs: completion triggers on [#] and [:] because a primitive and a
   qualified name are written that way, signature help on [(] and [,] because
   those open and separate a call's arguments, rename has a prepare step, and
   semantic tokens need the legend the encoder numbers the tokens by. The
   capabilities are written here rather than through the protocol package's
   own types, so the JSON the client reads is this list and nothing else. *)
let semantic_tokens_capability () =
  `Assoc
    [
      ( "legend",
        `Assoc
          [
            ( "tokenTypes",
              `List
                (List.map
                   (fun name -> `String name)
                   Json_conv.semantic_token_types) );
            ( "tokenModifiers",
              `List
                (List.map
                   (fun name -> `String name)
                   Json_conv.semantic_token_modifiers) );
          ] );
      ("full", `Bool true);
    ]

let capabilities () =
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
      (* Rename is routed and answers a refusal with a reason, because a
         rename has no empty form in the protocol. It is NOT advertised
         while the engine has no rename: a client that sees the capability
         registers a provider and offers the user a rename that can only
         fail, and VS Code rewrites the refusal into its own "The element
         can't be renamed", which hides why. Advertising it belongs to the
         change that lands the engine's rename. *)
      ("workspaceSymbolProvider", `Bool true);
      ("documentHighlightProvider", `Bool true);
      ("semanticTokensProvider", semantic_tokens_capability ());
    ]

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

(* The debounce. A document that was opened or changed waits until no further
   change has arrived for [delay] milliseconds. The loop that owns the input
   waits for it no longer than the earliest deadline and analyses what the
   wait ended for ([time_to_next], [analyse_due]); the wait is what makes a
   check run at all, so the last change of a session is published as well.
   Marking again restarts the wait, which is what makes a burst of keystrokes
   one analysis instead of one per keystroke. *)

let mark_stale t ~uri =
  match t.analysis with
  | None -> ()
  | Some _ ->
      Debounce.mark t.debounce ~uri
        ~deadline:(t.now () +. (float_of_int t.delay /. 1000.))

let publish t ~uri ~version ~lines ~diagnostics =
  match t.analysis with
  | None -> []
  | Some _ ->
      let params = Json_conv.publish_params ~uri ~version ~lines diagnostics in
      [
        Jsonrpc.Packet.Notification
          (Jsonrpc.Notification.create
             ~method_:"textDocument/publishDiagnostics"
             ~params:(Jsonrpc.Structured.t_of_yojson params)
             ());
      ]

(* Analyse one document and return the packet that publishes its result. A
   result for a version the document no longer has is dropped, never sent,
   and the document waits again: the client must not see a set that describes
   text it has already replaced. Only the publish is dropped; a request that
   forced this check is still answered, because the protocol requires one
   response to every request. *)
let analyse t ~uri =
  match t.analysis with
  | None -> []
  | Some analysis -> (
      match
        guarded (fun () ->
            analysis.check ~uri;
            analysis.diagnostics ~uri)
      with
      | Error message ->
          t.log ("analysis failed: " ^ uri ^ ": " ^ message);
          []
      | Ok None -> []
      | Ok (Some (version, diagnostics)) -> (
          match Jide_engine.Engine.document t.engine ~uri with
          | None -> []
          | Some document ->
              if Jide_engine.Document.version document <> version then (
                mark_stale t ~uri;
                [])
              else
                publish t ~uri ~version:(Some version)
                  ~lines:(Jide_engine.Document.lines document)
                  ~diagnostics))

let analyse_due t =
  match t.state with
  | Uninitialized | Shutting_down | Exited -> []
  | Running ->
      let ready = Debounce.due t.debounce ~now:(t.now ()) in
      List.concat_map (fun uri -> analyse t ~uri) ready

let time_to_next t =
  match t.state with
  | Uninitialized | Shutting_down | Exited -> None
  | Running -> Debounce.next_deadline t.debounce ~now:(t.now ())

let ensure_checked t ~uri =
  match t.state with
  | Uninitialized | Shutting_down | Exited -> []
  | Running ->
      if not (Debounce.stale t.debounce ~uri) then []
      else (
        Debounce.clear t.debounce ~uri;
        analyse t ~uri)

let set_delay t delay =
  let delay = clamp_delay delay in
  if delay <> t.delay then (
    t.delay <- delay;
    t.log ("diagnostics delay is now " ^ string_of_int delay ^ " ms"))

let apply_settings t settings =
  match Json_conv.delay_of_settings settings with
  | None -> ()
  | Some delay -> set_delay t delay

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
let dispatch_notification t ~parameters
    (notification : Lsp.Client_notification.t) =
  match notification with
  | Lsp.Client_notification.TextDocumentDidOpen params ->
      let doc = params.textDocument in
      let uri = normalize_uri (Lsp.Uri.to_string doc.uri) in
      Jide_engine.Engine.open_document t.engine ~uri ~version:doc.version
        ~text:doc.text;
      mark_stale t ~uri;
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
                    ~version:doc.version ~text:change.text;
                  mark_stale t ~uri)));
      []
  | Lsp.Client_notification.TextDocumentDidClose params -> (
      let uri = normalize_uri (Lsp.Uri.to_string params.textDocument.uri) in
      let closed = Jide_engine.Engine.document t.engine ~uri in
      Jide_engine.Engine.close_document t.engine ~uri;
      Debounce.clear t.debounce ~uri;
      (match t.analysis with
      | None -> ()
      | Some analysis -> (
          match guarded (fun () -> analysis.forget ~uri) with
          | Ok () -> ()
          | Error message ->
              t.log ("analysis drop failed: " ^ uri ^ ": " ^ message)));
      (* An empty set clears the file's diagnostics in the client. The
         version is the last one the document had, which is the text the
         client saw; a document the server never held is not published. *)
      match closed with
      | None -> []
      | Some document ->
          publish t ~uri
            ~version:(Some (Jide_engine.Document.version document))
            ~lines:(Jide_engine.Document.lines document)
            ~diagnostics:[])
  | Lsp.Client_notification.Initialized -> []
  | Lsp.Client_notification.ChangeConfiguration _ ->
      apply_settings t parameters;
      []
  | Lsp.Client_notification.DidSaveTextDocument _
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
      match
        guarded (fun () ->
            dispatch_notification t
              ~parameters:(json_of_parameters notification.params)
              client_notification)
      with
      | Ok packets -> packets
      | Error message ->
          t.log
            ("notification handler failed: " ^ notification.method_ ^ ": "
           ^ message);
          [])

(* The four navigation requests. The parameters are read before anything is
   analysed: a request the server cannot understand must not cost a check,
   and a request it can understand is answered from text the client has not
   replaced — [published] runs the check the debounce has not run yet, so its
   publish packet goes out ahead of the reply and the reply describes the same
   text as the diagnostics beside it. The answer is computed from [published],
   never inline: a list literal evaluates its elements right to left, so an
   answer written in one would run before the check it depends on. *)
let answer_navigation t (request : Jsonrpc.Request.t)
    (lookup : unit -> Yojson.Safe.t) ~uri =
  let published = ensure_checked t ~uri in
  let answer = lookup () in
  published @ [ reply_ok request.id answer ]

let handle_navigation t (request : Jsonrpc.Request.t) =
  let params = json_of_parameters request.params in
  match request.method_ with
  | "textDocument/definition" -> (
      match Navigation.target params with
      | Error message ->
          Some [ reply_error request.id (error_code `InvalidParams) message ]
      | Ok target ->
          let answer () =
            Navigation.definition_json
              (t.locate.definition ~uri:target.uri ~position:target.position)
          in
          Some (answer_navigation t request answer ~uri:target.uri))
  | "textDocument/references" -> (
      match Navigation.reference params with
      | Error message ->
          Some [ reply_error request.id (error_code `InvalidParams) message ]
      | Ok reference ->
          let answer () =
            Navigation.references_json
              (t.locate.references ~uri:reference.target.uri
                 ~position:reference.target.position
                 ~include_declaration:reference.include_declaration)
          in
          Some (answer_navigation t request answer ~uri:reference.target.uri))
  | "textDocument/hover" -> (
      match Navigation.target params with
      | Error message ->
          Some [ reply_error request.id (error_code `InvalidParams) message ]
      | Ok target ->
          let answer () =
            Navigation.hover_json
              (t.locate.hover ~uri:target.uri ~position:target.position)
          in
          Some (answer_navigation t request answer ~uri:target.uri))
  | "textDocument/documentSymbol" -> (
      match Navigation.document params with
      | Error message ->
          Some [ reply_error request.id (error_code `InvalidParams) message ]
      | Ok uri ->
          let answer () = Navigation.symbols_json (t.locate.symbols ~uri) in
          Some (answer_navigation t request answer ~uri))
  | _ -> None

(* The seven editing requests. Each one parses its parameters through the
   protocol package, so a parameter object of the wrong shape is refused
   before anything else happens; converts the client's position to a byte of
   the open document; forces a check when the document's text is newer than
   its last analysis, because an answer about text the client no longer has
   is wrong whatever it says; and encodes the engine's answer.

   An engine with nothing to say is answered with the empty answer the
   protocol has for the request: [null] where the result may be null, an
   empty list where it may be a list. That is not an error, and neither is a
   document that was never opened: both mean the same thing to a client,
   that there is nothing here. Rename has no empty answer, so a rename the
   engine will not make is refused with RequestFailed and the engine's own
   reason, never with an empty edit that would read as a rename that
   succeeded and changed nothing. *)

type editing_answer = Answer of Yojson.Safe.t | Refused of string

let reply_editing (request : Jsonrpc.Request.t) = function
  | Answer json -> reply_ok request.id json
  | Refused message ->
      reply_error request.id (error_code `RequestFailed) message

let editing_method = function
  | "textDocument/completion" | "textDocument/signatureHelp"
  | "textDocument/prepareRename" | "textDocument/rename" | "workspace/symbol"
  | "textDocument/documentHighlight" | "textDocument/semanticTokens/full" ->
      true
  | _ -> false

let uri_of_text_document (text_document : Lsp.Types.TextDocumentIdentifier.t) =
  normalize_uri (Lsp.Uri.to_string text_document.uri)

(* One editing request about an open document: the packets that publish the
   check it forced, then the one reply. A document the server does not hold
   is not checked: there is nothing to check, and the answer is the empty
   one. *)
let editing_answer t (request : Jsonrpc.Request.t) ~feature ~uri ~empty answer =
  match Jide_engine.Engine.document t.engine ~uri with
  | None ->
      t.log (feature ^ ": no open document " ^ uri ^ ", so the answer is empty");
      [ reply_editing request empty ]
  | Some document ->
      (* The check has to run before the answer is computed, not merely be
         concatenated in front of it: OCaml evaluates list elements right to
         left, so [checks @ [ reply_editing request (answer document) ]]
         would build the answer first and publish diagnostics for a text the
         answer never saw. *)
      let checks = ensure_checked t ~uri in
      let reply = reply_editing request (answer document) in
      checks @ [ reply ]

(* The same, for the requests that name a position. The document's line
   index clamps a position past the end of the file to the end of the file,
   so only a position that cannot exist at all is refused. *)
let editing_answer_at_position t (request : Jsonrpc.Request.t) ~feature ~uri
    ~(position : Lsp.Types.Position.t) ~empty answer =
  if position.line < 0 || position.character < 0 then
    [
      reply_error request.id
        (error_code `InvalidParams)
        "position.line and position.character must not be negative";
    ]
  else
    editing_answer t request ~feature ~uri ~empty (fun document ->
        let byte =
          Jide_engine.Document.byte_of_position document
            (Jide_lang.Position.make ~line:position.line
               ~character:position.character)
        in
        answer document byte)

let answer_completion t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.CompletionParams.t) =
  let uri = uri_of_text_document params.textDocument in
  editing_answer_at_position t request ~feature:"completion" ~uri
    ~position:params.position ~empty:(Answer `Null) (fun _document byte ->
      match Features.completion t.features ~uri ~byte with
      | None ->
          t.log
            (Printf.sprintf "completion: no answer at %s byte %d (snapshot %s)"
               uri byte
               (match Jide_engine.Engine.snapshot t.engine ~uri with
               | Some _ -> "present"
               | None -> "absent"));
          Answer `Null
      | Some items -> Answer (Json_conv.completion_json items))

let answer_signature_help t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.SignatureHelpParams.t) =
  let uri = uri_of_text_document params.textDocument in
  editing_answer_at_position t request ~feature:"signature help" ~uri
    ~position:params.position ~empty:(Answer `Null) (fun _document byte ->
      match Features.signature_help t.features ~uri ~byte with
      | Some help when help.Jide_engine.Types.signatures <> [] ->
          Answer (Json_conv.signature_help_json help)
      | Some _ | None -> Answer `Null)

let answer_prepare_rename t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.PrepareRenameParams.t) =
  let uri = uri_of_text_document params.textDocument in
  editing_answer_at_position t request ~feature:"prepare rename" ~uri
    ~position:params.position ~empty:(Answer `Null) (fun document byte ->
      match Features.prepare_rename t.features ~uri ~byte with
      | Some span ->
          Answer
            (Json_conv.range_json (Jide_engine.Document.lines document) span)
      | None -> Answer `Null)

let is_blank = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let answer_rename t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.RenameParams.t) =
  let uri = uri_of_text_document params.textDocument in
  let new_name = params.newName in
  if String.trim new_name = "" || String.exists is_blank new_name then
    [
      reply_error request.id
        (error_code `InvalidParams)
        "rename: newName must be one name, with nothing blank in it";
    ]
  else
    editing_answer_at_position t request ~feature:"rename" ~uri
      ~position:params.position
      ~empty:(Refused ("rename: the document is not open: " ^ uri))
      (fun _document byte ->
        match Features.rename t.features ~uri ~byte ~new_name with
        | None -> Refused "rename is unavailable: the engine has no rename"
        | Some (Error message) -> Refused message
        | Some (Ok []) -> Refused "rename: the engine named no edit to make"
        | Some (Ok edits) -> Answer (Json_conv.workspace_edit_json edits))

(* A workspace symbol answer is about every open document, so each one whose
   text is newer than its analysis is checked first. *)
let answer_workspace_symbol t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.WorkspaceSymbolParams.t) =
  let checks =
    List.concat_map
      (fun document ->
        ensure_checked t ~uri:(Jide_engine.Document.uri document))
      (Jide_engine.Engine.documents t.engine)
  in
  let answer =
    match Features.workspace_symbols t.features ~query:params.query with
    | None -> `List []
    | Some symbols -> Json_conv.symbols_json symbols
  in
  checks @ [ reply_ok request.id answer ]

let answer_document_highlight t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.DocumentHighlightParams.t) =
  let uri = uri_of_text_document params.textDocument in
  editing_answer_at_position t request ~feature:"document highlight" ~uri
    ~position:params.position
    ~empty:(Answer (`List []))
    (fun document byte ->
      match Features.highlights t.features ~uri ~byte with
      | None -> Answer (`List [])
      | Some highlights ->
          Answer
            (Json_conv.highlights_json
               (Jide_engine.Document.lines document)
               highlights))

let answer_semantic_tokens t (request : Jsonrpc.Request.t)
    (params : Lsp.Types.SemanticTokensParams.t) =
  let uri = uri_of_text_document params.textDocument in
  editing_answer t request ~feature:"semantic tokens" ~uri ~empty:(Answer `Null)
    (fun document ->
      match Features.semantic_tokens t.features ~uri with
      | None -> Answer `Null
      | Some tokens ->
          Answer
            (Json_conv.semantic_tokens_json
               (Jide_engine.Document.lines document)
               tokens))

let handle_initialized_request t request =
  match handle_navigation t request with
  | Some packets -> packets
  | None -> (
      match Lsp.Client_request.of_jsonrpc request with
      | Error message when editing_method request.method_ ->
          (* An editing request that this server serves, with parameters the
             protocol package would not read: the method exists, so the
             request is right and its parameters are not. *)
          [ reply_error request.id (error_code `InvalidParams) message ]
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
      | Ok (E (Lsp.Client_request.TextDocumentCompletion params)) ->
          answer_completion t request params
      | Ok (E (Lsp.Client_request.SignatureHelp params)) ->
          answer_signature_help t request params
      | Ok (E (Lsp.Client_request.TextDocumentPrepareRename params)) ->
          answer_prepare_rename t request params
      | Ok (E (Lsp.Client_request.TextDocumentRename params)) ->
          answer_rename t request params
      | Ok (E (Lsp.Client_request.WorkspaceSymbol params)) ->
          answer_workspace_symbol t request params
      | Ok (E (Lsp.Client_request.TextDocumentHighlight params)) ->
          answer_document_highlight t request params
      | Ok (E (Lsp.Client_request.SemanticTokensFull params)) ->
          answer_semantic_tokens t request params
      | Ok (E _) ->
          [
            reply_error request.id
              (error_code `MethodNotFound)
              ("unsupported request: " ^ request.method_);
          ])

let dispatch_request t (request : Jsonrpc.Request.t) =
  (let f =
     open_out_gen [ Open_append; Open_creat ] 0o644 "/tmp/jasmin-inbound.log"
   in
   output_string f ("request: " ^ request.method_ ^ "\n");
   flush f;
   close_out f);
  if String.equal request.method_ "initialize" then
    match Lsp.Client_request.of_jsonrpc request with
    | Error message ->
        [ reply_error request.id (error_code `InvalidRequest) message ]
    | Ok (E (Lsp.Client_request.Initialize params)) -> (
        (* A client may send the debounce with its initialization options,
           which the raw parameters carry as JSON. *)
        apply_settings t (json_of_parameters request.params);
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
