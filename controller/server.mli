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
    the server serving.

    It also owns the publish path. A document that was opened or changed
    waits for [delay] milliseconds without another change; a document that
    closes is cleared at once. The analysis itself is not the server's: it
    comes from the [analysis] the caller passes, so the server decides when
    to analyse and what to publish, and the engine decides how. *)

type state = Uninitialized | Running | Shutting_down | Exited

type analysis = {
  check : uri:string -> unit;
      (** Analyse the document's current text and store the result. *)
  diagnostics : uri:string -> (int * Jide_lang.Diagnostic.t list) option;
      (** The stored result: the version of the text it describes and the
          full set of its diagnostics, or [None] when nothing is stored. *)
  forget : uri:string -> unit;  (** Drop what a closed document left behind. *)
}
(** How a document is analysed. The engine satisfies this interface; a
    caller that has no analysis passes none, and then nothing is analysed
    and nothing is published. *)

type t

val default_delay : int
(** The debounce when neither the caller nor the client names one: 250
    milliseconds. *)

val min_delay : int
(** The shortest debounce accepted, 50 milliseconds. *)

val max_delay : int
(** The longest debounce accepted, 1,000 milliseconds. *)

val create :
  log:(string -> unit) ->
  ?engine:Jide_engine.Engine.t ->
  ?analysis:analysis ->
  ?delay:int ->
  ?now:(unit -> float) ->
  unit ->
  t
(** [create ~log ?engine ?analysis ?delay ?now ()] is a server that has not
    been initialized. [delay] is the debounce in milliseconds, clamped to
    [min_delay] to [max_delay] (and 250 when it is not given); a client can
    change it later with the [diagnostics.delay] setting. [now] is the clock
    in seconds, [Unix.gettimeofday] unless a test supplies its own. *)

val delay : t -> int
(** The debounce in force, in milliseconds. *)

val state : t -> state
val exit_code : t -> int
val exited : t -> bool

val eof_exit_code : t -> int
(** The code to exit with when the input stream closes: 0 when [shutdown]
    was answered, 1 otherwise (the rules are in the protocol). *)

val handle_packet : t -> Jide_lsp.Framing.packet -> Jide_lsp.Framing.packet list
(** Handle one incoming packet. Returns the packets to write, in order. *)

val analyse_due : t -> Jide_lsp.Framing.packet list
(** Analyse every document whose debounce delay has passed and return the
    packets that publish the results, the earliest deadline first. A loop
    that waits for input calls it whenever that wait ends, so a document
    whose last change was followed by no packet at all is still published.
    Nothing is analysed unless the server is [Running]. *)

val time_to_next : t -> float option
(** [time_to_next t] is how long a loop may wait for input before it has to
    call {!analyse_due}: the time left on the earliest debounce, in seconds,
    [Some 0.] when a document is already due, and [None] when no document
    waits, so the loop may block. It is [None] as well while the server is
    not [Running], because nothing is analysed then. *)

val ensure_checked : t -> uri:string -> Jide_lsp.Framing.packet list
(** Analyse the document now when its text is newer than its last analysis,
    and return the packet that publishes the result. A request that needs
    fresh data, such as definition or hover, calls this before it answers.
    Nothing is analysed unless the server is [Running]. *)

val apply_settings : t -> Yojson.Safe.t -> unit
(** Read the [diagnostics.delay] setting out of a settings object and clamp
    it. A settings object that does not carry one leaves the delay alone. *)

val engine : t -> Jide_engine.Engine.t
(** The engine, so that tests and the batch checker can read documents. *)
