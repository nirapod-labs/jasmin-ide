open Jasmin

type kind = Parse | Typing | Annotation | Preprocess | Internal
type t = { span : Jide_lang.Span.t; message : string; kind : kind }

let kind_name = function
  | Parse -> "parse"
  | Typing -> "typing"
  | Annotation -> "annotation"
  | Preprocess -> "preprocess"
  | Internal -> "internal"

let make ~kind ~span message = { span; message; kind }
let location_of_error_location = Error_span.of_error_location

let message_of_outcome = function
  | Ok message -> Ok message
  | Error message -> Error message

let render_tyerror error = Format.asprintf "%a" Pretyping.pp_tyerror error

let of_exn (exn : exn) : t =
  match exn with
  | Syntax.ParseError (location, message) ->
      let message =
        match message with Some m -> m | None -> "unexpected token"
      in
      make ~kind:Parse ~span:(Error_span.of_location location) message
  | Pretyping.TyError (location, error) ->
      make ~kind:Typing
        ~span:(Error_span.of_location location)
        (render_tyerror error)
  | Annot.AnnotationError (location, print) ->
      make ~kind:Annotation
        ~span:(Error_span.of_location location)
        (Format.asprintf "%t" print)
  | Typing.TyError (location, message) ->
      make ~kind:Typing ~span:(Error_span.of_i_location location) message
  | Utils.HiError error ->
      make ~kind:Typing
        ~span:(Error_span.of_error_location error.Utils.err_loc)
        (Format.asprintf "%a" Utils.pp_hierror error)
  | Stack_overflow ->
      make ~kind:Internal
        ~span:(Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
        "the file is too deeply nested to analyse"
  | exn ->
      make ~kind:Internal
        ~span:(Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
        (Printexc.to_string exn)

let to_diagnostic (error : t) : Jide_lang.Diagnostic.t =
  let severity =
    match error.kind with
    | Internal -> Jide_lang.Diagnostic.Error
    | Parse | Typing | Annotation | Preprocess -> Jide_lang.Diagnostic.Error
  in
  Jide_lang.Diagnostic.make ~code:(kind_name error.kind) ~source:"jasmin"
    ~severity error.span error.message

let diagnostic_of_exn exn = to_diagnostic (of_exn exn)
