type t

val create : unit -> t

val set_root : t -> string option -> unit
val root : t -> string option

val open_document : t -> uri:string -> version:int -> text:string -> unit
val change_document : t -> uri:string -> version:int -> text:string -> unit
val close_document : t -> uri:string -> unit
val document : t -> uri:string -> Document.t option
val documents : t -> Document.t list
