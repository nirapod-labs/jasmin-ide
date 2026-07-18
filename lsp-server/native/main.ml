let log message = prerr_endline ("[jasmin-lsp] " ^ message)

let run channels server =
  let rec loop () =
    match Jide_lsp.Framing.read channels with
    | None ->
        if not (Jide_controller.Server.exited server) then log "input closed";
        Jide_controller.Server.eof_exit_code server
    | Some (Error message) ->
        log ("malformed frame dropped: " ^ message);
        loop ()
    | Some (Ok packet) ->
        let replies = Jide_controller.Server.handle_packet server packet in
        List.iter (Jide_lsp.Framing.write channels) replies;
        if Jide_controller.Server.exited server then
          Jide_controller.Server.exit_code server
        else loop ()
  in
  loop ()

let () =
  let server = Jide_controller.Server.create ~log in
  let channels = Jide_lsp.Framing.{ input = stdin; output = stdout } in
  let code =
    try run channels server with
    | Sys_error message ->
        log ("input or output closed: " ^ message);
        Jide_controller.Server.eof_exit_code server
    | exn ->
        log ("fatal: " ^ Printexc.to_string exn);
        1
  in
  exit code
