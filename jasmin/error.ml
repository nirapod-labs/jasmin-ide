open Jasmin

type kind = Parse | Typing | Annotation | Preprocess | Internal

type t = {
  span : Jide_lang.Span.t;
  message : string;
  kind : kind;
  file : string option;
      (** The file the span indexes, when the compiler named one. *)
}

let kind_name = function
  | Parse -> "parse"
  | Typing -> "typing"
  | Annotation -> "annotation"
  | Preprocess -> "preprocess"
  | Internal -> "internal"

let make ?(file = None) ~kind ~span message = { span; message; kind; file }

let file_of_location (location : Location.t) : string option =
  if Location.isdummy location || String.equal location.loc_fname "" then None
  else Some location.loc_fname

let file_of_error_location = function
  | Utils.Lnone -> None
  | Utils.Lone location -> file_of_location location
  | Utils.Lmore location -> file_of_location location.Location.base_loc

(** [belongs_to ~file error] is true when the span of [error] indexes [file].
    A compiler error that names no file cannot be placed anywhere else, so
    it is kept wherever it is reported. *)
let belongs_to ~(file : string) (error : t) : bool =
  match error.file with None -> true | Some name -> String.equal name file

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
      make
        ~file:(file_of_location location)
        ~kind:Parse
        ~span:(Error_span.of_location location)
        message
  | Pretyping.TyError (location, error) ->
      make
        ~file:(file_of_location location)
        ~kind:Typing
        ~span:(Error_span.of_location location)
        (render_tyerror error)
  | Annot.AnnotationError (location, print) ->
      make
        ~file:(file_of_location location)
        ~kind:Annotation
        ~span:(Error_span.of_location location)
        (Format.asprintf "%t" print)
  | Typing.TyError (location, message) ->
      make
        ~file:(file_of_location location.Location.base_loc)
        ~kind:Typing
        ~span:(Error_span.of_i_location location)
        message
  | Utils.HiError error ->
      make
        ~file:(file_of_error_location error.Utils.err_loc)
        ~kind:Typing
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
