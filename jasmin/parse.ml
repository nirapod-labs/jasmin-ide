open Jasmin

type t = { items : Syntax.pprogram; diagnostics : Jide_lang.Diagnostic.t list }

let parse ~(name : string) (text : string) : t =
  match Parseio.parse_program ~name (Utils.IO.input_string text) with
  | items -> { items; diagnostics = [] }
  | exception exn ->
      { items = []; diagnostics = [ Error.diagnostic_of_exn exn ] }

let failed t = t.diagnostics <> []
