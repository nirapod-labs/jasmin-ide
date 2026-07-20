(** JSON-RPC framing over a pair of channels.

    This module owns the header parser. It does not use the framing helper
    of the [lsp] package, because that helper treats the value -1 as "no
    header seen", so a peer that sends [Content-Length: -1] followed by a
    second [Content-Length] gets the second one honoured. Here a header
    error poisons the stream: every later read returns end of input, so a
    frame that cannot be framed is never half-decoded. *)

type t

val create : input:in_channel -> output:out_channel -> t
(** One stream. [input] and [output] are usually stdin and stdout. *)

type packet = Jsonrpc.Packet.t

type read_result = (packet, string) result option
(** The result of one read.

    [None] is end of input, either because the peer closed the stream or
    because an earlier frame was unreadable and the stream was poisoned.
    [Some (Error _)] is a frame that could not be decoded; the caller logs
    it and carries on, but a poisoned stream ends on the next read. *)

val read : t -> read_result

val write : t -> packet -> unit
(** Write one packet and flush. Raises [Sys_error] only when the output
    channel is closed, which the caller treats as end of input. *)

val max_frame_bytes : int
(** The largest body accepted, in bytes. *)

val max_json_depth : int
(** The deepest JSON nesting accepted. A recursive decoder walks the
    nesting on the OCaml stack, and a deep enough body can kill the process
    with a signal instead of raising [Stack_overflow]; the body is scanned
    before it reaches the decoder. *)
