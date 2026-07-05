(** Uniform resource identifiers, as far as an editor needs them.

    Only what identifies a document is kept: the scheme, the authority and
    the path. A query and a fragment are dropped. The path is stored decoded,
    so [path] returns the filesystem path as the reader sees it and
    [to_string] writes it back with percent escapes. *)

type t
(** A parsed identifier. *)

val of_string : string -> t
(** [of_string text] parses [text]. A string without a valid scheme is taken
    as a path with no scheme, and it round-trips through [to_string]
    unchanged. Percent escapes are decoded when they are well formed and kept
    as literal text when they are not. *)

val to_string : t -> string
(** [to_string uri] is the identifier as text, with every byte of the path
    that is not unreserved or a slash percent-encoded. *)

val path : t -> string option
(** [path uri] is the decoded filesystem path of a [file] identifier, and
    [None] for any other scheme and for a file identifier that names a host
    other than [localhost]. The path need not exist. *)

val of_path : string -> t
(** [of_path path] is the [file] identifier of a filesystem path, without
    asking the filesystem whether it exists. An absolute path becomes
    [file:///path]; a relative one becomes [file:path]. *)

val equal : t -> t -> bool
(** [equal a b] is true when the two identify the same document: the same
    scheme, the same authority and the same path. Schemes and hosts are
    compared without case and an absent authority equals an empty one. *)
