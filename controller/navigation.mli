(** The four navigation requests: their parameters, read strictly, and their
    answers, drawn the way the protocol draws them.

    A request's parameters are read here and nowhere else. The reading is
    strict: the members the protocol requires must be there, must be of the
    type it gives them, and a line or a character must be a non-negative
    integer. A member the specification makes optional, or one [WorkDoneProgressParams]
    and [PartialResultParams] add, is ignored rather than refused, because it
    is the client's to send. A parameters object that fails the reading is an
    [InvalidParams] (-32602) answer, never a request handled with a default:
    a definition asked for at no position is not a definition at line zero.

    The answers are the protocol's own shapes:

    - a definition is an array of [Location], or [null] when the engine has
      none;
    - references are an array of [Location], empty when there are none;
    - a hover is [{"contents":{"kind":"markdown","value":...}}], with a
      [range] when the engine gives one, or [null] when it has no answer;
    - document symbols are an array of [DocumentSymbol], each with [range],
      [selectionRange], the protocol's [kind] number and its nested
      [children], empty when the document has no items.

    An empty answer is not an error: it is this module that turns "the engine
    has nothing" into [null] or [[]], so a request that lands in whitespace,
    past the end of the file, or on a document the server does not hold is
    answered, not refused. *)

type target = { uri : string; position : Jide_lang.Position.t }
(** Where a request looks: the document's uri as the client wrote it, and the
    position in it. The engine normalises the uri, so two spellings of one
    document are one document and neither is a parse error. *)

type reference = { target : target; include_declaration : bool }
(** A references request: where to look, and whether the declaration itself
    belongs in the answer. *)

val target : Yojson.Safe.t -> (target, string) result
(** [target params] reads the parameters of a [textDocument/definition],
    [textDocument/references] or [textDocument/hover] request. [Error] names
    the first member that is missing, mistyped or negative, and the caller
    answers [InvalidParams] with that text. *)

val reference : Yojson.Safe.t -> (reference, string) result
(** [reference params] reads the parameters of a [textDocument/references]
    request: {!target} plus the required [context.includeDeclaration]. *)

val document : Yojson.Safe.t -> (string, string) result
(** [document params] reads the uri out of the parameters of a
    [textDocument/documentSymbol] request, which carries a text document and
    no position. *)

val definition_json : Jide_engine.Locate.location list -> Yojson.Safe.t
(** The result of a [textDocument/definition] request: the locations, or
    [null] when there are none. *)

val references_json : Jide_engine.Locate.location list -> Yojson.Safe.t
(** The result of a [textDocument/references] request: the locations, an
    empty array when there are none. *)

val hover_json : Jide_engine.Locate.hover option -> Yojson.Safe.t
(** The result of a [textDocument/hover] request: the contents as one
    MarkupContent of the protocol's [markdown] kind, with the range the engine
    gave, or [null].

    The kind is always [markdown]: the client's
    [textDocument.hover.contentFormat] is not read, and no plain text answer
    is offered. That is a client-capability negotiation this server does not
    do; both clients this repository ships ask for markdown, so nothing is lost
    there, and a client that asked for [plaintext] alone would be sent a
    MarkupContent it may not render. *)

val symbols_json : Jide_engine.Locate.symbol list -> Yojson.Safe.t
(** The result of a [textDocument/documentSymbol] request: the tree of
    [DocumentSymbol], an empty array when there are none. A node carries
    [children] only when it has some and [detail] only when the engine gave
    one, so a leaf is four members and no empty array.

    The hierarchy is always what is written. The client's
    [textDocument.documentSymbol.hierarchicalDocumentSymbolSupport] is not
    read, so a client that can only take the flat [SymbolInformation] form is
    sent nested nodes instead. *)
