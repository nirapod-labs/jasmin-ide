(** A position in a text.

    Both components are zero based. [character] counts UTF-16 code units
    from the start of the line, the unit the Language Server Protocol uses
    and the only one both editor clients agree on. *)

type t = { line : int; character : int }
(** [line] is the zero-based line number and [character] is the zero-based
    offset of a UTF-16 code unit within that line. *)

val make : line:int -> character:int -> t
(** [make ~line ~character] is the position of the code unit [character] of
    line [line]. *)

val compare : t -> t -> int
(** [compare a b] orders by line first and by character second. *)
