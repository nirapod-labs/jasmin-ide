(** One check in a child process, with a wall-clock budget.

    A hang is a case the criteria count, so a check that never returns must
    not stop the run: every check happens in a process of its own, the
    parent waits for it with a deadline, and a child still alive at the
    deadline is killed and reported as a hang. A crash the process cannot
    survive, a segmentation fault for instance, is reported the same way and
    for the same reason: the harness has to finish the sample even when one
    input kills the checker.

    The child sends one marshalled {!Probe.result} back; an exception is
    caught in the child and sent as its printed form. The parent measures
    the wall clock around the whole exchange, which is the per-file figure
    the 5 s bound is about. *)

type outcome =
  | Returned of { elapsed : float; result : Probe.result }
  | Raised of { elapsed : float; message : string }
  | Timed_out of { elapsed : float }
  | Crashed of { elapsed : float; detail : string }

type payload = (Probe.result, string) result

let describe_status = function
  | Unix.WEXITED code -> Printf.sprintf "exited with status %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "was killed by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "was stopped by signal %d" signal

let run ~budget probe =
  flush stdout;
  flush stderr;
  let read_end, write_end = Unix.pipe () in
  let started = Unix.gettimeofday () in
  let deadline = started +. budget in
  match Unix.fork () with
  | 0 ->
      Unix.close read_end;
      let payload =
        match probe () with
        | result -> Ok result
        | exception exn -> Error (Printexc.to_string exn)
      in
      let channel = Unix.out_channel_of_descr write_end in
      (try
         Marshal.to_channel channel payload [];
         flush channel
       with _ -> ());
      close_out_noerr channel;
      Unix._exit 0
  | pid ->
      Unix.close write_end;
      let collected = Buffer.create 4096 in
      let scratch = Bytes.create 65536 in
      let rec drain () =
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then false
        else
          match Unix.select [ read_end ] [] [] remaining with
          | [], _, _ -> false
          | _ -> (
              match Unix.read read_end scratch 0 (Bytes.length scratch) with
              | 0 -> true
              | count ->
                  Buffer.add_subbytes collected scratch 0 count;
                  drain ()
              | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain ())
      in
      let finished = drain () in
      if not finished then (
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
        Unix.close read_end;
        Timed_out { elapsed = Unix.gettimeofday () -. started })
      else
        let _, status =
          try Unix.waitpid [] pid
          with Unix.Unix_error _ -> (pid, Unix.WEXITED 0)
        in
        Unix.close read_end;
        let elapsed = Unix.gettimeofday () -. started in
        let reply = Buffer.contents collected in
        if String.length reply > 0 then
          match Marshal.from_string reply 0 with
          | Ok result -> Returned { elapsed; result }
          | Error message -> Raised { elapsed; message }
          | exception _ ->
              Crashed
                {
                  elapsed;
                  detail =
                    "the reply could not be decoded (" ^ describe_status status
                    ^ ")";
                }
        else
          Crashed
            {
              elapsed;
              detail =
                "the check " ^ describe_status status ^ " and printed nothing";
            }
