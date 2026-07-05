(** JSON-RPC framing over a pair of channels.

    This module is the only place in the server that knows how a packet is
    written on the wire. It wraps the framing implementation of the [lsp]
    package, so a malformed header, a truncated body or invalid JSON is
    reported as a value rather than an exception escaping the loop. *)

type channels = { input : in_channel; output : out_channel }

type packet = Jsonrpc.Packet.t

(** The result of one read attempt.

    [None] is end of input: the peer closed the stream. [Some (Error _)] is
    a frame that could not be decoded; the caller logs it and carries on,
    because the request id, if any, is unknown. *)
type read_result = (packet, string) result option

val read : channels -> read_result

(** Write one packet and flush. Raises [Sys_error] only when the output
    channel is closed, which the caller treats as end of input. *)
val write : channels -> packet -> unit
