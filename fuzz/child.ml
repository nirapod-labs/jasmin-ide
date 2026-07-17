(** One fuzz case, run in a child process.

    A fuzz case must not be able to take the run down with it: a check that
    raises its way out of the library's own handler, one that dies on a
    signal, and one that never returns are each a result the harness reports
    rather than a reason for the harness to stop. The case runs in a child
    with a wall clock over it; the child writes one line back over a pipe and
    the parent reads it after the child is gone.

    The child is what makes a hang observable. In this process the Jasmin
    library catches every exception and turns it into a diagnostic, so a case
    that never returns would be indistinguishable from a slow one. *)

type outcome =
  | Passed
  | Failed of string  (** the case's own description of what went wrong *)
  | Hung  (** the child outlived the wall clock and was killed *)
  | Crashed of string  (** the child died on a signal or wrote no report *)

let read_all (descriptor : Unix.file_descr) : string =
  let buffer = Buffer.create 128 in
  let bytes = Bytes.create 512 in
  let rec loop () =
    match Unix.read descriptor bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let write_all (descriptor : Unix.file_descr) (text : string) : unit =
  let bytes = Bytes.of_string text in
  let rec loop offset =
    if offset < Bytes.length bytes then
      match
        Unix.write descriptor bytes offset (Bytes.length bytes - offset)
      with
      | count -> loop (offset + count)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

exception Timeout

let wait_for_child ~(seconds : float) (pid : int) =
  let off = { Unix.it_interval = 0.0; it_value = 0.0 } in
  let wait () =
    ignore
      (Unix.setitimer Unix.ITIMER_REAL { off with Unix.it_value = seconds });
    Fun.protect
      ~finally:(fun () -> ignore (Unix.setitimer Unix.ITIMER_REAL off))
      (fun () ->
        match Unix.waitpid [] pid with
        | _, status -> `Status status
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> raise Timeout)
  in
  match wait () with
  | outcome -> outcome
  | exception Timeout -> (
      (* The alarm can fire as the child exits, so ask again before calling the
         case a hang. *)
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ ->
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
          `Hung
      | _, status -> `Status status)

let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))

(** [run ~timeout case] runs [case] with [timeout] seconds over it. The case
    returns [""] when it holds, and a one-line description of the first
    property that does not. *)
let run ~(timeout : float) (case : unit -> string) : outcome =
  let read_end, write_end = Unix.pipe () in
  match Unix.fork () with
  | exception Unix.Unix_error (code, _, _) ->
      Unix.close read_end;
      Unix.close write_end;
      Crashed ("cannot fork: " ^ Unix.error_message code)
  | 0 ->
      Unix.close read_end;
      let line =
        match case () with
        | report -> report
        | exception exn ->
            let message =
              String.map
                (fun character ->
                  match character with
                  | '\n' | '\r' -> ' '
                  | character -> character)
                (Printexc.to_string exn)
            in
            "raised " ^ message
      in
      (try write_all write_end line with Unix.Unix_error _ -> ());
      (try Unix.close write_end with Unix.Unix_error _ -> ());
      Unix._exit 0
  | pid -> (
      Unix.close write_end;
      let outcome = wait_for_child ~seconds:timeout pid in
      let line = try read_all read_end with Unix.Unix_error _ -> "" in
      Unix.close read_end;
      match outcome with
      | `Hung -> Hung
      | `Status (Unix.WEXITED 0) -> (
          match String.trim line with "" -> Passed | report -> Failed report)
      | `Status (Unix.WEXITED code) ->
          Crashed (Printf.sprintf "the case exited %d" code)
      | `Status (Unix.WSIGNALED signal) ->
          Crashed (Printf.sprintf "the case died on signal %d" signal)
      | `Status (Unix.WSTOPPED signal) ->
          Crashed (Printf.sprintf "the case stopped on signal %d" signal))
