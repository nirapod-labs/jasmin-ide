open Jasmin
type result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
}

let empty = { diagnostics = []; warnings = [] }

let max_diagnostics = 200

module Make (A : Arch_full.Arch) = struct
  let type_item (environment : A.extended_op Pretyping.Env.env) item =
    match Pretyping.tt_item A.arch_info environment item with
    | environment -> Ok environment
    | exception exn -> Error exn

  let type_items ~(file : string) ~(items : Syntax.pprogram)
      ~(idirs : (string * string) list) : result =
    let initial =
      List.fold_left Pretyping.Env.add_from Pretyping.Env.empty idirs
    in
    match Pretyping.Env.enter_file initial None None file with
    | None ->
      { empty with
        diagnostics =
          [ Error.to_diagnostic
              (Error.make ~kind:Error.Internal
                 ~span:(Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
                 ("cannot enter " ^ file)) ] }
    | Some (environment, _resolved) ->
      let environment = ref environment in
      let diagnostics = ref [] in
      let stopped = ref false in
      List.iter
        (fun item ->
          if not !stopped then (
            let snapshot = !environment in
            match type_item snapshot item with
            | Ok next -> environment := next
            | Error exn ->
              diagnostics := Error.diagnostic_of_exn exn :: !diagnostics;
              environment := snapshot;
              if List.length !diagnostics >= max_diagnostics then stopped := true))
        items;
      let _ = Pretyping.Env.exit_file !environment in
      { empty with diagnostics = List.rev !diagnostics }

  let check ~(file : string) ~(text : string) ~(idirs : (string * string) list) :
      result =
    Options.apply ();
    let parsed = Parse.parse ~name:file text in
    if Parse.failed parsed then { empty with diagnostics = parsed.diagnostics }
    else
      let typed, printed =
        Warnings.with_capture (fun () ->
            type_items ~file ~items:parsed.items ~idirs)
      in
      { typed with warnings = Warnings.parse printed }
end
