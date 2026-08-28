(** The edit surface: random protocol sessions through the state machine.

    A case is one client's session over a handful of documents, driven
    straight into [Jide_controller.Server.handle_packet]: opens, changes,
    closes, requests the server does not serve, a request it does serve whose
    parameters do not parse, prepareRename and rename at random positions
    with random names, notifications it does not know, and a shutdown and an
    exit at the end. The server answers rename from the engine, as the native
    server does. The properties are the ones a client depends on:

    - a full-sync change leaves exactly the text that was sent, at the
      version that was sent, and the versions only grow;
    - a request gets exactly one reply, carrying the id it was sent with and
      the code the protocol names for that state;
    - a notification gets no reply, and one the server cannot parse leaves
      the document store alone;
    - every diagnostic the analysis produces lies inside the document;
    - a prepareRename is a range inside the document, [null] or a refusal,
      and a rename is a workspace edit whose every range lies inside the
      document it names, or a refusal (RequestFailed, or InvalidParams for a
      name that is not one).

    A case reports the first property that does not hold, in one line. *)

exception Found of string

let fail format = Printf.ksprintf (fun message -> raise (Found message)) format

(* The reply a request may get. A request whose method the server does not
   serve is answered with one of two codes depending on whether the library
   can parse its parameters, so both are named here rather than guessed. *)
type expectation =
  | Any_result
  | Any_error of Jsonrpc.Response.Error.Code.t list
  | Result_or_error of Jsonrpc.Response.Error.Code.t list

let send server packet = Jide_controller.Server.handle_packet server packet

let request ?(params = `Assoc []) ~id method_ =
  Jsonrpc.Packet.Request
    (Jsonrpc.Request.create ~id:(`Int id) ~method_
       ~params:(Jsonrpc.Structured.t_of_yojson params)
       ())

let notification ?(params = `Assoc []) method_ =
  Jsonrpc.Packet.Notification
    (Jsonrpc.Notification.create ~method_
       ~params:(Jsonrpc.Structured.t_of_yojson params)
       ())

let initialize_params = `Assoc [ ("capabilities", `Assoc []) ]

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

let expect_no_reply what replies =
  match replies with
  | [] -> ()
  | packets ->
      fail "%s got %d reply packet(s) where none belongs" what
        (List.length packets)

let expect_reply ~what ~id ~expect replies =
  match replies with
  | [] -> fail "%s got no reply" what
  | _ :: _ :: _ ->
      fail "%s got %d replies where one belongs" what (List.length replies)
  | [ Jsonrpc.Packet.Response response ] -> (
      if response.Jsonrpc.Response.id <> `Int id then
        fail "%s answered an id it was not given" what;
      match (expect, response.Jsonrpc.Response.result) with
      | Any_result, Ok _ | Result_or_error _, Ok _ -> ()
      | Result_or_error codes, Error error ->
          if not (List.mem error.Jsonrpc.Response.Error.code codes) then
            fail "%s failed with another code than the protocol names: %s" what
              error.Jsonrpc.Response.Error.message
      | Any_result, Error error ->
          fail "%s failed with %s" what error.Jsonrpc.Response.Error.message
      | Any_error _, Ok _ ->
          fail "%s succeeded where the protocol names an error" what
      | Any_error codes, Error error ->
          if not (List.mem error.Jsonrpc.Response.Error.code codes) then
            fail "%s failed with another code than the protocol names" what)
  | [ _ ] -> fail "%s got a packet that is not a response" what

let engine server = Jide_controller.Server.engine server

(* The result of a reply, for the checks that read it. *)
let result_of replies =
  match replies with
  | [ Jsonrpc.Packet.Response { Jsonrpc.Response.result = Ok json; _ } ] ->
      Some json
  | _ -> None

let position_params ~uri ~line ~character =
  [
    ("textDocument", `Assoc [ ("uri", `String uri) ]);
    ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
  ]

(* A range of an answer lies inside the text it is about: its lines exist and
   its start is not after its end. *)
let check_range ~what ~text (range : Yojson.Safe.t) =
  let lines = Jide_lang.Line_index.create text in
  let count = Jide_lang.Line_index.line_count lines in
  let point = function
    | `Assoc fields -> (
        match
          (List.assoc_opt "line" fields, List.assoc_opt "character" fields)
        with
        | Some (`Int line), Some (`Int character) -> (line, character)
        | _ -> fail "%s answered a position that is not one" what)
    | _ -> fail "%s answered a position that is not one" what
  in
  match range with
  | `Assoc fields -> (
      match (List.assoc_opt "start" fields, List.assoc_opt "end" fields) with
      | Some start, Some stop ->
          let start = point start and stop = point stop in
          if fst stop >= count || fst start < 0 || compare start stop > 0 then
            fail "%s answered a range outside the document" what
      | _ -> fail "%s answered a range that is not one" what)
  | _ -> fail "%s answered a range that is not one" what

(* The names a rename is asked for: a word the document may hold, a fresh one,
   a keyword, a qualified name and one with a blank in it. *)
let new_names = [| "x"; "y"; "f"; "fz_new"; "fn"; "A::b"; "a b"; "_EQ"; "N" |]
let stored server uri = Jide_engine.Engine.document (engine server) ~uri

(** The store's own answer after an open or a change, then the analysis over
    it: the text and the version are the client's, and every span is inside
    the text. *)
let check_document server ~what ~uri ~version ~text =
  match stored server uri with
  | None -> fail "%s left no document for %s" what uri
  | Some document -> (
      let stored_version = Jide_engine.Document.version document in
      if stored_version <> version then
        fail "%s stored version %d where %d was sent" what stored_version
          version;
      let stored_text = Jide_engine.Document.text document in
      if not (String.equal stored_text text) then
        fail "%s stored %d bytes where %d were sent" what
          (String.length stored_text)
          (String.length text);
      match Jide_engine.Engine.check (engine server) ~uri with
      | None -> fail "%s left nothing to analyse for %s" what uri
      | Some analysis ->
          if analysis.Jide_engine.Engine.version <> version then
            fail "%s analysed version %d where %d was stored" what
              analysis.Jide_engine.Engine.version version;
          let length = String.length text in
          List.iter
            (fun (diagnostic : Jide_lang.Diagnostic.t) ->
              let span = diagnostic.Jide_lang.Diagnostic.span in
              if
                span.Jide_lang.Span.start_byte < 0
                || span.Jide_lang.Span.end_byte > length
                || span.Jide_lang.Span.start_byte > span.Jide_lang.Span.end_byte
              then
                fail
                  "the analysis of %s reported the span %d..%d in a text of %d \
                   bytes"
                  uri span.Jide_lang.Span.start_byte
                  span.Jide_lang.Span.end_byte length)
            analysis.Jide_engine.Engine.diagnostics)

let uris =
  [|
    "untitled:fuzz/one.jazz";
    "untitled:fuzz/two.jazz";
    "untitled:fuzz/three.jazz";
    "file:///fuzz/four.jazz";
  |]

let case ?(corpus = [||]) (st : Gen.state) : string =
  let engine_ = Jide_engine.Engine.create () in
  let features =
    Jide_controller.Features.make
      ~completion:(fun ~uri:_ ~byte:_ -> None)
      ~signature_help:(fun ~uri:_ ~byte:_ -> None)
      ~prepare_rename:(fun ~uri ~byte ->
        Jide_controller.Rename_features.prepare engine_ ~uri ~byte)
      ~rename:(fun ~uri ~byte ~new_name ->
        Jide_controller.Rename_features.rename engine_ ~uri ~byte ~new_name)
      ~workspace_symbols:(fun ~query:_ -> None)
      ~highlights:(fun ~uri:_ ~byte:_ -> None)
      ~semantic_tokens:(fun ~uri:_ -> None)
      ()
  in
  let server =
    Jide_controller.Server.create
      ~log:(fun _ -> ())
      ~engine:engine_ ~features ()
  in
  let open_documents : (string, int * string) Hashtbl.t = Hashtbl.create 8 in
  let next_id = ref 0 in
  let new_id () =
    incr next_id;
    !next_id
  in
  let document_id = ref 0 in
  let source () =
    incr document_id;
    Gen.document_text ~corpus st ^ string_of_int !document_id
  in
  let open_one uri =
    let version = 1 in
    let text = source () in
    expect_no_reply "a didOpen"
      (send server
         (notification
            ~params:(open_params ~uri ~version ~text)
            "textDocument/didOpen"));
    Hashtbl.replace open_documents uri (version, text);
    check_document server ~what:"a didOpen" ~uri ~version ~text
  in
  let change_one uri =
    match Hashtbl.find_opt open_documents uri with
    | None -> open_one uri
    | Some (version, _) ->
        let version = version + 1 in
        let text = source () in
        expect_no_reply "a didChange"
          (send server
             (notification
                ~params:(change_params ~uri ~version ~text)
                "textDocument/didChange"));
        Hashtbl.replace open_documents uri (version, text);
        check_document server ~what:"a didChange" ~uri ~version ~text
  in
  let close_one uri =
    match Hashtbl.find_opt open_documents uri with
    | None -> ()
    | Some _ ->
        expect_no_reply "a didClose"
          (send server
             (notification ~params:(close_params ~uri) "textDocument/didClose"));
        Hashtbl.remove open_documents uri;
        if Option.is_some (stored server uri) then
          fail "a didClose left %s in the store" uri
  in
  (* A position somewhere in a document, or past its end, which the server
     clamps. *)
  let position_in uri =
    let text =
      match Hashtbl.find_opt open_documents uri with
      | Some (_, text) -> text
      | None -> ""
    in
    let lines = Jide_lang.Line_index.create text in
    let line = Gen.int st (Jide_lang.Line_index.line_count lines + 1) in
    (text, line, Gen.int st 40)
  in
  let prepare_rename uri =
    let text, line, character = position_in uri in
    let id = new_id () in
    let replies =
      send server
        (request ~id
           ~params:(`Assoc (position_params ~uri ~line ~character))
           "textDocument/prepareRename")
    in
    expect_reply ~what:"a prepareRename" ~id
      ~expect:(Result_or_error [ Jsonrpc.Response.Error.Code.RequestFailed ])
      (List.filter
         (function Jsonrpc.Packet.Response _ -> true | _ -> false)
         replies);
    match
      result_of
        (List.filter
           (function Jsonrpc.Packet.Response _ -> true | _ -> false)
           replies)
    with
    | Some `Null | None -> ()
    | Some range -> check_range ~what:"a prepareRename" ~text range
  in
  let rename uri =
    let text, line, character = position_in uri in
    let new_name = Gen.pick st new_names in
    let id = new_id () in
    let replies =
      List.filter
        (function Jsonrpc.Packet.Response _ -> true | _ -> false)
        (send server
           (request ~id
              ~params:
                (`Assoc
                  (position_params ~uri ~line ~character
                  @ [ ("newName", `String new_name) ]))
              "textDocument/rename"))
    in
    expect_reply ~what:"a rename" ~id
      ~expect:
        (Result_or_error
           [
             Jsonrpc.Response.Error.Code.RequestFailed;
             Jsonrpc.Response.Error.Code.InvalidParams;
           ])
      replies;
    match result_of replies with
    | Some (`Assoc [ ("changes", `Assoc changes) ]) ->
        List.iter
          (fun (target, edits) ->
            let target_text =
              if String.equal target uri then text
              else
                match Hashtbl.find_opt open_documents target with
                | Some (_, other) -> other
                | None -> fail "a rename edited %s, which is not open" target
            in
            match edits with
            | `List edits ->
                List.iter
                  (function
                    | `Assoc fields -> (
                        match List.assoc_opt "range" fields with
                        | Some range ->
                            check_range ~what:"a rename" ~text:target_text range
                        | None -> fail "a rename edit has no range")
                    | _ -> fail "a rename edit is not an object")
                  edits
            | _ -> fail "a rename's edits are not a list")
          changes
    | Some _ -> fail "a rename answered something that is not a workspace edit"
    | None -> ()
  in
  let open_another () =
    let free =
      Array.to_list uris
      |> List.filter (fun uri -> not (Hashtbl.mem open_documents uri))
    in
    match free with
    | [] -> change_one (Gen.pick st uris)
    | uri :: _ -> open_one uri
  in
  try
    (* Before initialize: one answer the protocol names and one notification
       the server must not act on. *)
    let id = new_id () in
    expect_reply ~what:"a request before initialize" ~id
      ~expect:(Any_error [ Jsonrpc.Response.Error.Code.ServerNotInitialized ])
      (send server (request ~id "textDocument/hover"));
    expect_no_reply "a didOpen before initialize"
      (send server
         (notification
            ~params:(open_params ~uri:uris.(0) ~version:1 ~text:"fn f() {\n}\n")
            "textDocument/didOpen"));
    if List.length (Jide_engine.Engine.documents (engine server)) > 0 then
      fail "a notification before initialize reached the document store";
    let id = new_id () in
    expect_reply ~what:"initialize" ~id ~expect:Any_result
      (send server (request ~id ~params:initialize_params "initialize"));
    if Jide_controller.Server.state server <> Jide_controller.Server.Running
    then fail "initialize did not leave the server running";
    expect_no_reply "an initialized notification"
      (send server (notification "initialized"));
    (* The session proper. *)
    for _ = 1 to Gen.between st 4 24 do
      match Gen.int st 12 with
      | 0 | 1 | 2 -> open_another ()
      | 3 | 4 | 5 -> change_one (Gen.pick st uris)
      | 6 -> close_one (Gen.pick st uris)
      | 7 ->
          (* A protocol method this server does not implement. Completion
             used to stand here and no longer can: the server answers it now,
             with the snapshot's candidates. Folding ranges is in the
             library's request type and outside everything the server
             answers, so it is the stand-in. *)
          let id = new_id () in
          expect_reply ~what:"an unsupported request" ~id
            ~expect:
              (Any_error
                 [
                   Jsonrpc.Response.Error.Code.InvalidRequest;
                   Jsonrpc.Response.Error.Code.MethodNotFound;
                 ])
            (send server (request ~id "textDocument/foldingRange"))
      | 8 ->
          (* A request the server does serve, with parameters that do not
             parse: the protocol names InvalidParams, not MethodNotFound. *)
          let id = new_id () in
          expect_reply ~what:"a request with malformed params" ~id
            ~expect:(Any_error [ Jsonrpc.Response.Error.Code.InvalidParams ])
            (send server
               (request ~id
                  ~params:(`Assoc [ ("textDocument", `Int 1) ])
                  "textDocument/hover"))
      | 9 -> prepare_rename (Gen.pick st uris)
      | 10 -> rename (Gen.pick st uris)
      | _ ->
          expect_no_reply "a notification the server does not know"
            (send server (notification "fuzz/unknown"));
          expect_no_reply "a didChange with malformed params"
            (send server
               (notification
                  ~params:(`Assoc [ ("textDocument", `String "not an object") ])
                  "textDocument/didChange"))
    done;
    (* The store holds every document still open, at the version last sent. *)
    Hashtbl.iter
      (fun uri (version, text) ->
        match stored server uri with
        | None -> fail "the store lost %s, which was never closed" uri
        | Some document ->
            if Jide_engine.Document.version document <> version then
              fail "the store holds %s at another version" uri;
            if not (String.equal (Jide_engine.Document.text document) text) then
              fail "the store holds other text for %s" uri)
      open_documents;
    (* Shutdown, then the states the protocol names after it. *)
    let id = new_id () in
    expect_reply ~what:"shutdown" ~id ~expect:Any_result
      (send server (request ~id "shutdown"));
    if
      Jide_controller.Server.state server
      <> Jide_controller.Server.Shutting_down
    then fail "shutdown did not leave the server shutting down";
    let id = new_id () in
    expect_reply ~what:"a request after shutdown" ~id
      ~expect:(Any_error [ Jsonrpc.Response.Error.Code.InvalidRequest ])
      (send server (request ~id "shutdown"));
    let before = List.length (Jide_engine.Engine.documents (engine server)) in
    expect_no_reply "a didOpen after shutdown"
      (send server
         (notification
            ~params:
              (open_params ~uri:"untitled:fuzz/after.jazz" ~version:1
                 ~text:"fn f() {\n}\n")
            "textDocument/didOpen"));
    if List.length (Jide_engine.Engine.documents (engine server)) <> before then
      fail "a notification after shutdown reached the document store";
    expect_no_reply "exit" (send server (notification "exit"));
    if not (Jide_controller.Server.exited server) then
      fail "exit did not leave the server exited";
    if Jide_controller.Server.exit_code server <> 0 then
      fail "the exit code after a shutdown is not 0";
    ""
  with Found report -> report
