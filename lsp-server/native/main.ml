let log message = prerr_endline ("[jasmin-lsp] " ^ message)
let write channels packets = List.iter (Jide_lsp.Framing.write channels) packets

(* The wait the debounce is built on. The loop waits for input, but never
   longer than the time left on the earliest deadline: the end of that wait is
   what analyses a document whose last change was followed by no packet at
   all. A negative timeout waits indefinitely, which is what the server asks
   for when no document is waiting. *)
let run channels server =
  let publish_due () =
    write channels (Jide_controller.Server.analyse_due server)
  in
  let rec loop () =
    let timeout =
      match Jide_controller.Server.time_to_next server with
      | Some seconds -> seconds
      | None -> -1.
    in
    if not (Jide_lsp.Framing.wait channels ~timeout) then (
      publish_due ();
      loop ())
    else
      match Jide_lsp.Framing.read channels with
      | None ->
          if not (Jide_controller.Server.exited server) then log "input closed";
          Jide_controller.Server.eof_exit_code server
      | Some (Error message) ->
          log ("malformed frame dropped: " ^ message);
          (* A frame the reader cannot frame leaves the stream poisoned, and
             a poisoned stream is ready to read: the turn after this one
             reads end of input and ends the session here. *)
          loop ()
      | Some (Ok packet) ->
          (* A document whose debounce delay has passed is analysed before the
             packet is served, and its diagnostics are written first. *)
          publish_due ();
          write channels (Jide_controller.Server.handle_packet server packet);
          if Jide_controller.Server.exited server then
            Jide_controller.Server.exit_code server
          else loop ()
  in
  loop ()

(** The protocol loop, which is the mode when the command line names no
    other. *)
let serve () =
  let engine = Jide_engine.Engine.create () in
  let server =
    Jide_controller.Server.create ?engine:(Some engine)
      ?analysis:(Analysis.create engine) ~log ()
  in
  let channels = Jide_lsp.Framing.create ~input:stdin ~output:stdout in
  try run channels server with
  | Sys_error message ->
      log ("input or output closed: " ^ message);
      Jide_controller.Server.eof_exit_code server
  | exn ->
      log ("fatal: " ^ Printexc.to_string exn);
      1

let () =
  (* `check` is read before the server is created, so the batch mode writes
     nothing but its own lines to stdout and never touches the protocol. *)
  let code =
    match Array.to_list Sys.argv with
    | _ :: "check" :: arguments -> Jide_check.Cli.main arguments
    | _ -> serve ()
  in
  exit code
