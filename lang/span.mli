(** A range of bytes in a text.

    The range is half open: [start_byte] is included and [end_byte] is
    excluded, which is how the compiler reports its locations. *)

type t = { start_byte : int; end_byte : int }
(** [start_byte] is the first byte of the range and [end_byte] the first byte
    after it. *)

val make : start_byte:int -> end_byte:int -> t
(** [make ~start_byte ~end_byte] is the range of those two offsets. They are
    stored as given; a range whose end is not after its start is empty. *)

val is_empty : t -> bool
(** [is_empty span] is true when the range holds no byte, which includes a
    range whose end lies before its start. *)

val compare : t -> t -> int
(** [compare a b] orders by start first and by end second. *)

val contains : t -> byte:int -> bool
(** [contains span ~byte] is true when [byte] lies in the range. The end is
    excluded, so a byte at [end_byte] is not contained. *)

val overlaps : t -> t -> bool
(** [overlaps a b] is true when the two ranges share at least one byte. An
    empty range overlaps nothing. *)

val merge : t -> t -> t
(** [merge a b] is the smallest range that contains both. *)

val shift : t -> int -> t
(** [shift span delta] moves both ends by [delta] bytes. It does not clamp,
    so a negative [delta] can move a range below zero. *)
