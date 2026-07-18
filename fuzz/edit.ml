(** The edit surface: random protocol sessions through the state machine.

    A case is one client's session over a handful of documents, driven
    straight into [Jide_controller.Server.handle_packet]: opens, changes,
    closes, requests the server does not serve, notifications it does not
    know, and a shutdown and an exit at the end. The properties are the ones
    a client depends on:

    - a full-sync change leaves exactly the text that was sent, at the
      version that was sent, and the versions only grow;
    - a request gets exactly one reply, carrying the id it was sent with and
      the code the protocol names for that state;
    - a notification gets no reply, and one the server cannot parse leaves
      the document store alone;
    - every diagnostic the analysis produces lies inside the document.

    A case reports the first property that does not hold, in one line. *)

exception Found of string

let fail format = Printf.ksprintf (fun message -> raise (Found message)) format

(* The reply a request may get. A request whose method the server does not
   serve is answered with one of two codes depending on whether the library
   can parse its parameters, so both are named here rather than guessed. *)
type expectation =
  | Any_result
  | Any_error of Jsonrpc.Response.Error.Code.t list

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
      | Any_result, Ok _ -> ()
      | Any_result, Error error ->
          fail "%s failed with %s" what error.Jsonrpc.Response.Error.message
      | Any_error _, Ok _ ->
          fail "%s succeeded where the protocol names an error" what
      | Any_error codes, Error error ->
          if not (List.mem error.Jsonrpc.Response.Error.code codes) then
            fail "%s failed with another code than the protocol names" what)
  | [ _ ] -> fail "%s got a packet that is not a response" what

let engine server = Jide_controller.Server.engine server
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

