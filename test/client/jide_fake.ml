(* A scripted server for testing the client itself. It speaks the same
   framing as a real server, over in-memory pipes: each request that arrives
   is answered with the next step of the script, [Sleep] delays the answer,
   [Close] closes the server's output, and a script that runs out leaves the
   request unanswered. *)

let now = Unix.gettimeofday

let pop_bytes (buffer : Buffer.t) (bytes : Bytes.t) (count : int) =
  let available = Buffer.length buffer in
  if available = 0 then 0
  else begin
    let take = min count available in
    Buffer.blit buffer 0 bytes 0 take;
    let rest = Buffer.sub buffer take (available - take) in
    Buffer.clear buffer;
    Buffer.add_string buffer rest;
    take
  end

let transport ~(script : Jide_session.step list) : Jide_client.transport =
  let inbox = Buffer.create 256 in
  let outbox = Buffer.create 256 in
  let pending = ref script in
  let ready_at = ref 0.0 in
  let input_closed = ref false in
  let output_closed = ref false in
  let outstanding = ref 0 in

  (* Take script steps until there is something to read or the next step is
     still in the future. *)
  let rec produce () =
    if !outstanding > 0 && Buffer.length outbox = 0 && not !output_closed && now () >= !ready_at
    then
      match !pending with
      | [] -> ()
      | Jide_session.Sleep milliseconds :: rest ->
        pending := rest;
        ready_at := now () +. (float_of_int milliseconds /. 1000.);
        produce ()
      | Jide_session.Expect_timeout _ :: rest ->
        pending := rest;
        produce ()
      | Jide_session.Close :: rest ->
        pending := rest;
        output_closed := true
      | Jide_session.Message json :: rest ->
        pending := rest;
        Buffer.add_string outbox (Jide_framing.encode (Yojson.Safe.to_string json));
        if Jide_normalise.is_reply json then decr outstanding
  in

  let wait_readable milliseconds =
    let deadline = now () +. (float_of_int milliseconds /. 1000.) in
    let rec loop () =
      produce ();
      if Buffer.length outbox > 0 || !output_closed then true
      else
        let remaining = deadline -. now () in
        if remaining <= 0.0 then false
        else begin
          (match !pending with
          | [] -> Unix.sleepf remaining
          | _ ->
            let until_ready = !ready_at -. now () in
            Unix.sleepf (if until_ready > 0.0 then min remaining until_ready else remaining));
          loop ()
        end
    in
    loop ()
  in

  let request_reader =
    Jide_framing.make_reader
      {
        Jide_framing.fill = (fun bytes count -> pop_bytes inbox bytes count);
        wait_readable = (fun _ -> Buffer.length inbox > 0 || !input_closed);
      }
  in
  let reply_reader =
    Jide_framing.make_reader
      { Jide_framing.fill = (fun bytes count -> pop_bytes outbox bytes count); wait_readable }
  in

  let send body =
    Buffer.add_string inbox (Jide_framing.encode body);
    match Jide_framing.read_message request_reader ~timeout_ms:0 with
    | Error _ -> ()
    | Ok received -> (
      match Yojson.Safe.from_string received with
      | json ->
        if Jide_client.is_request json then begin
          incr outstanding;
          produce ()
        end
      | exception Yojson.Json_error _ ->
        incr outstanding;
        produce ())
  in

  {
    Jide_client.send;
    recv = (fun ~timeout_ms -> Jide_framing.read_message reply_reader ~timeout_ms);
    close_input =
      (fun () ->
        (* a server exits when its input closes, and that closes its output *)
        input_closed := true;
        output_closed := true);
    shutdown = (fun () -> ());
  }
