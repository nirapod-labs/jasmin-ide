(** JSON-RPC framing over a pair of channels.

    This module owns the header parser and the input buffer. It does not use
    the framing helper of the [lsp] package, because that helper treats the
    value -1 as "no header seen", so a peer that sends [Content-Length: -1]
    followed by a second [Content-Length] gets the second one honoured. Here a
    header error poisons the stream: every later read returns end of input, so
    a frame that cannot be framed is never half-decoded. *)

type t

val create : input:in_channel -> output:out_channel -> t
(** One stream. [input] and [output] are usually stdin and stdout.

    The module reads [input] through a buffer of its own, from the descriptor
    under it, so a caller must not read that channel itself: bytes taken from
    the channel would not be seen here. *)

type packet = Jsonrpc.Packet.t

type read_result = (packet, string) result option
(** The result of one read.

    [None] is end of input, either because the peer closed the stream or
    because an earlier frame was unreadable and the stream was poisoned.
    [Some (Error _)] is a frame that could not be decoded; the caller logs
    it and carries on, but a poisoned stream ends on the next read. *)

val read : t -> read_result
(** Read one packet, waiting for as long as the peer takes to finish it.

    A [params] member whose value is JSON null is read as an absent member, so
    a request or notification that writes ["params": null] means what the
    protocol says it means: no parameters. JSON-RPC 2.0 allows the value and
    LSP clients do send it, for [shutdown] and [exit] among others. Only null
    is rewritten; a [params] of any other type is still refused, because a
    frame whose member has the wrong type is malformed and is dropped. *)

val wait : t -> timeout:float -> bool
(** [wait t ~timeout] is [true] when a read has something to do: bytes are
    already buffered, the input has some within [timeout] seconds, or the
    stream was poisoned, in which case the next read is end of input at once
    and the wait must not park the caller on the descriptor. A negative
    [timeout] waits indefinitely, and a time-out is [false], so a caller with
    work of its own can do it and wait again.

    The three answers rest on one rule: [wait] is [true] exactly when the
    read that follows is not going to block. A caller that waits before
    every read therefore always reaches the read that reports a poisoned
    stream, instead of waiting for a byte that stream will never deliver. *)

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
