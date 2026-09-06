(** The editing features the engine answers, in the protocol layer's terms.

    Seven requests need an answer the protocol layer cannot work out for
    itself: completion, signature help, prepare rename, rename, workspace
    symbols, document highlights and semantic tokens. This module is the
    shape of those answers, so the layer that serves them and the layer that
    computes them agree on one interface and neither has to know the other's
    types. The answers themselves are the engine's value types
    ({!Jide_engine.Types}), never JSON, and nothing here mentions the
    protocol.

    A position is a byte offset in the document's text, never a protocol
    position. The server converts the client's UTF-16 position with the
    document's line index before it asks, and converts every span of the
    answer back the same way, so no UTF-16 offset ever reaches the engine.

    An answer that names another file carries that file's line index, because
    the server writes a range from one and holds the index of the open
    documents only. {!file} is the pair.

    A query answers [None] when the engine has nothing to say, and the server
    then answers the request with the empty answer the protocol has for it:
    [null] where the result may be null, an empty list where it may be a
    list, and never an error. {!unavailable} answers [None] to every query:
    it is what a server passes while the engine has no implementation of
    these features, and every request is then answered with that empty
    answer. Rename is the one request with no empty answer in the protocol,
    so it is refused as an error instead: an empty edit would tell the client
    that the rename succeeded and changed nothing. *)

type file = {
  uri : string;  (** the file as the client names it *)
  lines : Jide_lang.Line_index.t;
      (** its text, indexed: every span of the answer is a byte range in it *)
}

type edit = {
  file : file;
  edits : Jide_engine.Types.text_edit list;
      (** in source order, all of them applied together *)
}
(** The edits one file of a rename takes. *)

type located_symbol = { file : file; symbol : Jide_engine.Types.symbol }
(** A symbol of a file that is not the one the request named; its spans are
    bytes of [file]. *)

type t
(** The editing features of one server. *)

val unavailable : t
(** The features of an engine that has none: every query answers [None], so
    every request is answered with its empty answer. *)

val available : t -> bool
(** [available t] is false only for {!unavailable}, so a caller can say once
    that the engine answers none of these requests yet. *)

val make :
  completion:
    (uri:string -> byte:int -> Jide_engine.Types.completion_item list option) ->
  signature_help:
    (uri:string -> byte:int -> Jide_engine.Types.signature_help option) ->
  prepare_rename:(uri:string -> byte:int -> Jide_lang.Span.t option) ->
  rename:
    (uri:string ->
    byte:int ->
    new_name:string ->
    (edit list, string) result option) ->
  workspace_symbols:(query:string -> located_symbol list option) ->
  highlights:(uri:string -> byte:int -> Jide_engine.Types.highlight list option) ->
  semantic_tokens:(uri:string -> Jide_engine.Types.semantic_token list option) ->
  unit ->
  t
(** [make ~completion ~signature_help ~prepare_rename ~rename
    ~workspace_symbols ~highlights ~semantic_tokens ()] is the engine's
    answers. It is the one place the two layers meet: the body of each
    function is the engine's, and the protocol layer calls them through the
    queries below. *)

val completion :
  t -> uri:string -> byte:int -> Jide_engine.Types.completion_item list option
(** The items that may be written at [byte]: keywords, the names the
    resolver sees in scope, the functions the file requires, the intrinsics
    of the project's architecture, and the annotation keys. *)

val signature_help :
  t -> uri:string -> byte:int -> Jide_engine.Types.signature_help option
(** The signatures of the call holding [byte], with the argument the cursor
    is in. *)

val prepare_rename : t -> uri:string -> byte:int -> Jide_lang.Span.t option
(** The name a rename would rewrite, [None] where a rename is refused: a
    keyword, an intrinsic, or a position that holds no name. *)

val rename :
  t ->
  uri:string ->
  byte:int ->
  new_name:string ->
  (edit list, string) result option
(** [rename t ~uri ~byte ~new_name] is the edits that rename the name holding
    [byte] to [new_name], every file they touch. [Some (Ok edits)] holds at
    least one edit. [Some (Error message)] is a refusal and [message] is the
    engine's reason, which the request answers as an error. [None] means the
    engine has no rename, which the request also answers as an error. *)

val workspace_symbols : t -> query:string -> located_symbol list option
(** The declarations of every project file whose name holds [query], the
    empty [query] meaning all of them. *)

val highlights :
  t -> uri:string -> byte:int -> Jide_engine.Types.highlight list option
(** The occurrences of the name holding [byte] in the same file. *)

val semantic_tokens :
  t -> uri:string -> Jide_engine.Types.semantic_token list option
(** The tokens of the document, in any order; the encoder sorts them. *)
