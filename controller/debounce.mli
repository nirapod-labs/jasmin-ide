(** Which documents are waiting to be analysed, and when their wait ends.

    A document that was opened or changed is stale: it carries a deadline,
    the time by which no further change is expected. Marking it again
    restarts the wait, which is the whole of the debounce. Nothing here
    reads a clock: the caller passes the time, so the rules are the same in
    a test as in the server.

    Times are seconds from any fixed origin, in the unit [Unix.gettimeofday]
    returns; only differences between them matter. *)

type t
(** The waiting documents of one server. *)

val create : unit -> t

val mark : t -> uri:string -> deadline:float -> unit
(** [mark t ~uri ~deadline] makes [uri] stale until [deadline], replacing an
    earlier deadline for the same document. *)

val stale : t -> uri:string -> bool
(** [stale t ~uri] is true while [uri] waits, whether or not its deadline has
    passed. A stale document is one whose last analysis is older than its
    text. *)

val clear : t -> uri:string -> unit
(** [clear t ~uri] stops waiting for [uri], whether because it was analysed
    or because it closed. Clearing a document that does not wait is
    harmless. *)

val due : t -> now:float -> string list
(** [due t ~now] is the documents whose deadline has passed, in deadline
    order, the earlier one first and ties broken by URI so the order is the
    same on every run. They stop waiting: analysing one clears it. *)
