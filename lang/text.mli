(** Measurements on the text of a document. *)

val strip_bom : string -> string
(** [strip_bom text] is [text] without a leading UTF-8 byte-order mark, the
    three bytes the lexer rejects; it is [text] itself when there is none. *)

val utf8_byte_length : string -> int -> int -> int
(** [utf8_byte_length text start stop] is the number of bytes of the range
    [start, stop), with both ends clamped to the text. It is zero when the
    clamped end does not lie after the clamped start. *)

val is_utf8 : string -> bool
(** [is_utf8 text] is true when [text] is well-formed UTF-8: no truncated
    sequence, no overlong form, no surrogate and nothing above the last code
    point. The empty string is well-formed. *)
