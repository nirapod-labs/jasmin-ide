(* jide-test-client: drive a language server over pipes and print one
   normalised line per reply, or run the same protocol against a scripted
   fake server. *)

let usage =
  "usage: jide-test-client --server <path> [--timeout <ms>] <session-file>\n\
  \       jide-test-client --fake-server <script-file> [--timeout <ms>] <session-file>"

let default_timeout_ms = 5000

type config = {
  server : string option;
  fake_script : string option;
  timeout_ms : int;
  session : string;
}

let parse_arguments argv =
  let rec go server fake_script timeout_ms session = function
    | [] -> (
      match (server, fake_script, session) with
      | Some server, None, Some session -> Ok { server = Some server; fake_script; timeout_ms; session }
      | None, Some fake_script, Some session ->
        Ok { server; fake_script = Some fake_script; timeout_ms; session }
      | Some _, Some _, _ -> Error "give either --server or --fake-server, not both"
      | None, None, _ -> Error "give --server or --fake-server"
      | _, _, None -> Error "give a session file")
    | "--server" :: value :: rest -> go (Some value) fake_script timeout_ms session rest
    | "--fake-server" :: value :: rest -> go server (Some value) timeout_ms session rest
    | "--timeout" :: value :: rest -> (
      match int_of_string_opt value with
      | Some milliseconds when milliseconds > 0 -> go server fake_script milliseconds session rest
      | Some _ | None ->
        Error (Printf.sprintf "--timeout needs a positive number of milliseconds, got %S" value))
    | [ ("--server" | "--fake-server" | "--timeout") as option ] ->
      Error (Printf.sprintf "%s needs a value" option)
    | option :: _ when String.length option > 1 && option.[0] = '-' ->
      Error (Printf.sprintf "unknown option %s" option)
    | path :: rest -> (
      match session with
      | Some _ -> Error "give exactly one session file"
      | None -> go server fake_script timeout_ms (Some path) rest)
  in
  go None None default_timeout_ms None (List.tl (Array.to_list argv))

let load parse path kind =
  match parse path with
  | Ok steps -> Ok steps
  | Error message -> Error (Printf.sprintf "%s %s: %s" kind path message)

let finish ~timeout_ms transport steps =
  let outcome = Jide_client.run ~out:stdout ~transport ~timeout_ms steps in
  transport.Jide_client.shutdown ();
  match outcome with
  | Ok () -> Ok ()
  | Error failure -> Error (failure.Jide_client.code, failure.Jide_client.message)

let run config =
  match load Jide_session.parse_file config.session "session file" with
  | Error message -> Error (2, message)
  | Ok steps -> (
    match config.fake_script with
    | Some path -> (
      match load Jide_session.parse_script_file path "script file" with
      | Error message -> Error (2, message)
      | Ok script -> finish ~timeout_ms:config.timeout_ms (Jide_fake.transport ~script) steps)
    | None -> (
      match config.server with
      | None -> Error (2, "give --server or --fake-server")
      | Some server -> (
        match Jide_client.spawn ~server with
        | Error message -> Error (2, message)
        | Ok transport -> finish ~timeout_ms:config.timeout_ms transport steps)))

let () =
  match parse_arguments Sys.argv with
  | Error message ->
    Printf.eprintf "jide-test-client: %s\n%s\n" message usage;
    Stdlib.exit 2
  | Ok config ->
    let code =
      match run config with
      | Ok () -> 0
      | Error (code, message) ->
        Printf.eprintf "jide-test-client: %s\n" message;
        code
    in
    Stdlib.exit code
