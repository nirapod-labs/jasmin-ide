(** One document: its text, the index over that text, and where it came
    from.

    A document the editor opened keeps the version the client gave it. A
    document read from a file has version 0. Neither path changes the
    bytes: a byte-order mark stays, because stripping it belongs to the
    analysis, which is the step that needs to. *)

type t
(** A document. *)

val create : uri:string -> version:int -> text:string -> t
(** [create ~uri ~version ~text] is an open document. [uri] identifies it,
    and its path is the one the URI names when the URI carries a file path
    and [None] otherwise. *)

val with_text : t -> version:int -> string -> t
(** [with_text document ~version text] is [document] with a new text, a new
    version and a new line index. *)

val max_file_bytes : int
(** The largest file [read_file] and [of_file] accept: 8 MiB. A larger file
    is refused with a message instead of being read and discarded, so a
    request does no unbounded work on one. *)

val readable : string -> bool
(** [readable path] is true when [read_file path] would succeed: [path] is a
    regular file no larger than [max_file_bytes]. It looks at the file's
    status and reads no bytes, so a version request can answer without
    reading the whole file. *)

val read_file : string -> (string, string) result
(** [read_file path] is the whole content of [path], or [Error message] when
    it cannot be read. Only a regular file of at most [max_file_bytes] is
    read: a directory, a FIFO and a device are refused before the read, so a
    caller never waits on one. It is the one reader the file loader and the
    engine share. *)

val of_file : uri:string -> string -> (t, string) result
(** [of_file ~uri path] reads [path] and returns it as a document with
    version 0 and [path] recorded. [uri] identifies the document. The text
    is the bytes of the file, a byte-order mark included. A file that
    cannot be read is [Error message], never an exception. *)

val uri : t -> string
(** The URI the document was created with. *)

val path : t -> string option
(** The file path recorded for the document. *)

val version : t -> int
(** The version the client gave the document, or 0 for a file read from
    disk. *)

val text : t -> string
(** The whole text. *)

val lines : t -> Jide_lang.Line_index.t
(** The line index over the text. *)

val byte_of_position : t -> Jide_lang.Position.t -> int
(** [byte_of_position document position] is the byte offset of [position]. *)

val position_of_byte : t -> int -> Jide_lang.Position.t
(** [position_of_byte document byte] is the UTF-16 position of [byte]. *)

val span_of_bytes : t -> start_byte:int -> end_byte:int -> Jide_lang.Span.t
(** [span_of_bytes document ~start_byte ~end_byte] is the span between two
    byte offsets, clamped to the text and never inverted. *)

val line_text : t -> int -> string
(** [line_text document line] is the content of [line] without its
    terminator, or the empty string for a line outside the index. *)
