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

