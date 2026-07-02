(** A diagnostic: the message, the place, and where it came from. *)

(** How serious a diagnostic is, ordered as the protocol orders it. *)
type severity = Error | Warning | Information | Hint

type t = {
  span : Span.t;  (** The bytes the diagnostic is about. *)
  severity : severity;  (** How serious it is. *)
  message : string;  (** The text shown to the reader. *)
  code : string option;  (** The compiler's code for it, when there is one. *)
  source : string;  (** The producer of the diagnostic. *)
  related : (Span.t * string) list;
      (** Further places and the message to show at each. *)
}

val make :
  ?code:string ->
  ?source:string ->
  ?related:(Span.t * string) list ->
  severity:severity ->
  Span.t ->
  string ->
  t
(** [make ?code ?source ?related ~severity span message] is the diagnostic for
    [span] with [message]. [source] defaults to ["jasmin"], [code] to [None]
    and [related] to the empty list. *)

val compare : t -> t -> int
(** [compare a b] orders by the start of the span, then by its end, then by
    the message. *)
