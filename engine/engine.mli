type t

val create : unit -> t

val set_root : t -> string option -> unit
val root : t -> string option

val open_document : t -> uri:string -> version:int -> text:string -> unit

(** Replace the text of an open document.

    A change is applied to a document that [open_document] created and only
    to it: a change for a uri that was never opened, or was closed, is
    ignored and never creates the document. The client owns the set of open
    documents, so the store must not invent one from a change. *)
val change_document : t -> uri:string -> version:int -> text:string -> unit

val close_document : t -> uri:string -> unit
val document : t -> uri:string -> Document.t option
val documents : t -> Document.t list
