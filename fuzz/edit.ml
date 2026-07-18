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

