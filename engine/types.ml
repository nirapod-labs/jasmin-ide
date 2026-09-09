(** Values the engine returns to the protocol layer.

    Nothing here mentions JSON or LSP: the controller converts these to
    protocol types, so the engine stays testable without a protocol. *)

(* [file] is a path, the one the index holds and normalised: it is not an
   identifier, so nothing here builds a uri. The seam that answers the
   protocol converts it once, in [Locate.definition] and [Locate.references],
   which are the only places a client's own spelling is known. *)
type location = { file : string; span : Jide_lang.Span.t }
type diagnostic = Jide_lang.Diagnostic.t

type symbol_kind =
  | Function
  | Parameter
  | Variable
  | Global
  | Namespace
  | Type_alias
  | Require
  | Exec
  | File
  | Unknown

type symbol = {
  name : string;
  detail : string option;
  kind : symbol_kind;
  span : Jide_lang.Span.t;
  name_span : Jide_lang.Span.t;
  children : symbol list;
}

type markdown = { value : string }
type hover = { span : Jide_lang.Span.t; contents : markdown list }

type completion_kind =
  | Keyword
  | Function
  | Variable_
  | Field
  | Module_
  | Type_parameter
  | Snippet
  | Text

type completion_item = {
  label : string;
  kind : completion_kind;
  detail : string option;
  documentation : markdown option;
  insert_text : string option;
  sort_text : string option;
}

type signature_parameter = { label : string; documentation : markdown option }

type signature = {
  label : string;
  documentation : markdown option;
  parameters : signature_parameter list;
}

type signature_help = {
  signatures : signature list;
  active_signature : int;
  active_parameter : int;
}

type text_edit = { span : Jide_lang.Span.t; new_text : string }
type workspace_edit = { changes : (string * text_edit list) list }
type highlight_kind = Text_ | Read | Write
type highlight = { span : Jide_lang.Span.t; kind : highlight_kind }

type semantic_token_type =
  | Token_namespace
  | Token_type
  | Token_function
  | Token_parameter
  | Token_variable
  | Token_property
  | Token_keyword

type semantic_token = {
  span : Jide_lang.Span.t;
  token_type : semantic_token_type;
  modifiers : string list;
}
