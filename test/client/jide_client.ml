(* The driver: it sends the steps of a session over a transport, prints one
   normalised line per message and turns every failure into an exit code. *)

type transport = {
  send : string -> unit;
  recv : timeout_ms:int -> (string, Jide_framing.error) result;
  close_input : unit -> unit;
  shutdown : unit -> unit;
}

type failure = { code : int; message : string }

let failure code message = { code; message }

(* A message asks for a reply when it carries an id that is not null. *)
let is_request = function
  | `Assoc fields -> (
      match List.assoc_opt "id" fields with
      | None | Some `Null -> false
      | Some _ -> true)
  | _ -> false

(* The client's four exit codes, and nothing else: 0 every reply arrived,
   1 a timeout, 2 anything the client cannot accept (a malformed frame, a
   reply that is not JSON-RPC, a malformed session or script file, a usage
   error, or a reply where the session forbade one), 3 the server left
   before replying. *)
let code_of_error = function
  | Jide_framing.Timeout -> 1
  | Jide_framing.End_of_input -> 3
  | Jide_framing.Malformed _ -> 2

let error_failure error =
  failure (code_of_error error) (Jide_framing.error_message error)

let send transport body =
  try Ok (transport.send body) with
  | Sys_error message -> Error message
  | Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)

(* Read one message and print its line. A server may send notifications
   while a reply is outstanding: those are printed too, and reading goes on
   until the reply itself arrives. *)
let rec receive ~out transport ~timeout_ms =
  match transport.recv ~timeout_ms with
  | Error error -> Error (error_failure error)
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error message ->
          Error (failure 2 ("the message is not JSON: " ^ message))
      | json -> (
          match Jide_normalise.message_of_json json with
          | Error message -> Error (failure 2 message)
          | Ok message ->
              output_string out (Jide_normalise.line message ^ "\n");
              flush out;
              if Jide_normalise.is_reply json then Ok ()
              else receive ~out transport ~timeout_ms))

let run ~out ~transport ~timeout_ms steps =
  let rec go = function
    | [] -> Ok ()
    | Jide_session.Sleep milliseconds :: rest ->
        Unix.sleepf (float_of_int milliseconds /. 1000.);
        go rest
    | Jide_session.Close :: rest ->
        transport.close_input ();
        go rest
    | Jide_session.Message json :: rest -> (
        match send transport (Yojson.Safe.to_string json) with
        | Error message ->
            Error (failure 3 ("cannot write to the server: " ^ message))
        | Ok () -> (
            if not (is_request json) then go rest
            else
              match receive ~out transport ~timeout_ms with
              | Error failure -> Error failure
              | Ok () -> go rest))
    | Jide_session.Expect_timeout milliseconds :: rest -> (
        match transport.recv ~timeout_ms:milliseconds with
        | Error Jide_framing.Timeout -> go rest
        | Error error -> Error (error_failure error)
        (* A message where the session promised silence breaks the session's
           contract, so it is reported as a malformed input (2). There is no
           fifth code, and reusing 1 would read as a timeout that did not
           happen. *)
        | Ok body ->
            Error
              (failure 2
                 ("a reply arrived where a timeout was expected: " ^ body)))
  in
  go steps

(* How long a server gets to end by itself once its input has closed, and how
   long it then gets to act on a signal. Both are short: a server asked to
   shut down ends at once, and a server that reads its input to the end ends
   when the end arrives. *)
let grace_seconds = 2.0

(* wait_for pid seconds: the child's exit status once it has ended, or None
   while it is still running when the time is up. Only a wait reaps a child,
   and until it is reaped the process is a zombie: still in the table, so
   still something a test can see. *)
let wait_for pid seconds =
  let deadline = Unix.gettimeofday () +. seconds in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () < deadline then (
          Unix.sleepf 0.01;
          loop ())
        else None
    | _, status -> Some status
  in
  loop ()

(* end_server pid: do not return until the process the client started is
   gone. The client ends no server, but it does not leave one running either:
   a server still there after the session has ended did not end at the end of
   its input, so it is asked to end, then told to, and only then does this
   return. Without this the client exits first, the server is re-parented to
   pid 1 and it keeps running with the pipes of a client that no longer
   exists. *)
let end_server pid =
  match wait_for pid grace_seconds with
  | Some status -> status
  | None -> (
      let signal signal =
        try Unix.kill pid signal with Unix.Unix_error _ -> ()
      in
      signal Sys.sigterm;
      match wait_for pid grace_seconds with
      | Some status -> status
      | None -> (
          signal Sys.sigkill;
          (* A killed process ends at once: this wait blocks only until the
             kernel has done it, and it is the wait that reaps the child. *)
          try snd (Unix.waitpid [] pid)
          with Unix.Unix_error _ -> Unix.WSIGNALED Sys.sigkill))

let spawn ~(server : string) : (transport, string) result =
  let to_server_read, to_server_write = Unix.pipe () in
  let from_server_read, from_server_write = Unix.pipe () in
  let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> () in
  (* Every end of both pipes belongs to the client. A descriptor that is not
     close-on-exec is inherited by the server, so the server would hold the
     write end of its own standard input: the pipe keeps a writer after the
     client has closed its half, the end of input never arrives, and the
     server waits for input nobody will send. The three standard descriptors
     are made from these when the process is created and a duplicate is not
     close-on-exec, so the session itself still works. *)
  List.iter Unix.set_close_on_exec
    [ to_server_read; to_server_write; from_server_read; from_server_write ];
  match
    Unix.create_process server [| server |] to_server_read from_server_write
      Unix.stderr
  with
  | exception Unix.Unix_error (error, _, _) ->
      List.iter close_noerr
        [ to_server_read; to_server_write; from_server_read; from_server_write ];
      Error
        (Printf.sprintf "cannot start %s: %s" server (Unix.error_message error))
  | pid ->
      Unix.close to_server_read;
      Unix.close from_server_write;
      let to_server = Unix.out_channel_of_descr to_server_write in
      let reader =
        Jide_framing.make_reader (Jide_framing.fd_input from_server_read)
      in
      let transport =
        {
          send = (fun body -> Jide_framing.write to_server body);
          recv =
            (fun ~timeout_ms -> Jide_framing.read_message reader ~timeout_ms);
          close_input = (fun () -> close_out_noerr to_server);
          shutdown =
            (fun () ->
              (* The closed input is the end of the session for the server:
                 a server that follows the protocol ends on it. Then the
                 process the client started is waited for, and the end of its
                 output is closed last, once nothing is reading it. *)
              close_out_noerr to_server;
              ignore (end_server pid);
              close_noerr from_server_read);
        }
      in
      Ok transport
