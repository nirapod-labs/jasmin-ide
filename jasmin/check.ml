open Jasmin

type result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
}

let empty = { diagnostics = []; warnings = [] }
let max_diagnostics = 200

module Make (A : Arch_full.Arch) = struct
  type environment = A.extended_op Pretyping.Env.env

  let type_item (environment : environment) item =
    match Pretyping.tt_item A.arch_info environment item with
    | environment -> Ok environment
    | exception exn -> Error exn

  (** [type_items ~file ~items ~idirs] types the items one by one and
      returns the diagnostics, plus the environment the loop leaves behind
      when it started at all. A failed item contributes one diagnostic and
      the next item is typed in the environment from before it. *)
  let type_items ~(file : string) ~(items : Syntax.pprogram)
      ~(idirs : (string * string) list) : result * environment option =
    let initial =
      match List.fold_left Pretyping.Env.add_from Pretyping.Env.empty idirs with
      | environment -> Ok environment
      | exception exn -> Error exn
    in
    match initial with
    | Error exn ->
        ({ empty with diagnostics = [ Error.diagnostic_of_exn exn ] }, None)
    | Ok initial -> (
        match Pretyping.Env.enter_file initial None None file with
        | None ->
            ( {
                empty with
                diagnostics =
                  [
                    Error.to_diagnostic
                      (Error.make ~file:(Some file) ~kind:Error.Internal
                         ~span:(Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
                         ("cannot enter " ^ file));
                  ];
              },
              None )
        | Some (environment, _resolved) ->
            let environment = ref environment in
            let diagnostics = ref [] in
            let stopped = ref false in
            List.iter
              (fun item ->
                if not !stopped then
                  let snapshot = !environment in
                  match type_item snapshot item with
                  | Ok next -> environment := next
                  | Error exn ->
                      let error = Error.of_exn exn in
                      (* A span indexes the text of the file its location names.
                         Typing follows a require into another file, so a failure
                         there carries that file's offsets and is not this
                         document's to report. *)
                      if Error.belongs_to ~file error then
                        diagnostics := Error.to_diagnostic error :: !diagnostics;
                      environment := snapshot;
                      if List.length !diagnostics >= max_diagnostics then
                        stopped := true)
              items;
            let environment = Pretyping.Env.exit_file !environment in
            ( { empty with diagnostics = List.rev !diagnostics },
              Some environment ))

  let check ~(file : string) ~(text : string) ~(idirs : (string * string) list)
      : result =
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
