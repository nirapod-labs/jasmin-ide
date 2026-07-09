(** The protocol state machine.

    It owns the lifecycle rules of LSP 3.17: an [initialize] before
    anything else, one answer to a second [initialize], [shutdown] then
    [exit], requests before [initialize] answered with -32002, unknown
    requests with [MethodNotFound], unknown notifications logged and
    ignored. It writes nothing: it returns the packets the loop must send,
    so the whole machine is testable without pipes.

    Notifications that change the document store are applied only while
    the server is [Running]; an [exit] and a cancel request are the two
    the protocol allows in every state. Every handler, request or
    notification, runs under a catch-all that logs the failure and keeps
    the server serving. *)

type state = Uninitialized | Running | Shutting_down | Exited

type t

val create : log:(string -> unit) -> t
val state : t -> state
val exit_code : t -> int
val exited : t -> bool

(** The code to exit with when the input stream closes: 0 when [shutdown]
    was answered, 1 otherwise (the rules are in the protocol). *)
val eof_exit_code : t -> int

(** Handle one incoming packet. Returns the packets to write, in order. *)
val handle_packet : t -> Jide_lsp.Framing.packet -> Jide_lsp.Framing.packet list

(** The engine, so that tests and the batch checker can read documents. *)
val engine : t -> Jide_engine.Engine.t
