(** The line structure of a text, and the conversion between byte offsets and
    UTF-16 positions.

    Lines end at a line feed. A carriage return that comes immediately before
    a line feed belongs to the line terminator; a carriage return anywhere
    else is content. The last line needs no terminator, so an empty text has
    one empty line and a text ending in a line feed has a final empty line.

    The index keeps the text it was built from, and every function in this
    module is total: offsets outside the text and line numbers outside the
    index are clamped, and a text that is not valid UTF-8 is measured one
    byte at a time. *)

type t
(** An index over one text. *)

val create : string -> t
(** [create text] indexes [text]. It accepts any string; a byte that does not
    begin a well-formed UTF-8 sequence counts as one character made of one
    UTF-16 code unit. *)

val line_count : t -> int
(** The number of lines, which is at least one. *)

val line_start : t -> int -> int
(** [line_start t line] is the byte offset of the first byte of [line]. [line]
    is clamped to the lines that exist, so a negative number means the first
    line and a large one the last. *)

val line_end : t -> int -> int
(** [line_end t line] is the byte offset just past the last byte of the
    content of [line], before its terminator. The line number is clamped like
    [line_start]. *)

val position_of_byte : t -> int -> Position.t
(** [position_of_byte t byte] is the position of [byte], clamped to the text.
    A byte inside a multi-byte character maps to the position of the first
    byte of that character, and a byte inside a line terminator maps to the
    end of the line. *)

val byte_of_position : t -> Position.t -> int
(** [byte_of_position t position] is the offset of the character holding the
    code unit [position.character] of [position.line]. The line is clamped to
    the lines that exist and the character to the length of that line, so a
    character past the end of the line maps to the end of the line. A code
    unit in the middle of a surrogate pair maps to the first byte of the
    character that holds it. *)

val utf16_length : t -> Span.t -> int
(** [utf16_length t span] is the number of UTF-16 code units in the part of
    the text that lies wholly inside [span], after clamping the span to the
    text. A character that [span] cuts in half is not counted. *)
