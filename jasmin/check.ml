open Jasmin

type result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
  facts : Facts.t;
}

let empty = { diagnostics = []; warnings = []; facts = Facts.empty }
let max_diagnostics = 200

module Make (A : Arch_full.Arch) = struct
  type environment = A.extended_op Pretyping.Env.env

  let type_item (environment : environment) item =
    match Pretyping.tt_item A.arch_info environment item with
    | environment -> Ok environment
    | exception exn -> Error exn

  (** [added ~before ~after] is the run of declarations [after] holds and
      [before] does not. Typing pushes every declaration at the front
      ([e_decls = P.MIfun v :: env.e_decls] in the compiler's
      [pretyping.ml]), so an item's own declarations are the head of [after]
      and the old ones are the tail that follows them. An item that declares
      nothing, an [exec] for example, leaves the list alone, and a [require]
      pushes a whole file's declarations, so the size of the run is read from
      the two lists and never assumed to be one.

      The run is kept only when the tail it leaves is [before] itself. When
      it is not, the declarations did not arrive by pushing and the whole of
      [after] is offered instead: the collector keeps only the declarations
      whose location lies inside the item, so the wider run is read through
      this item's own range and the item is not left without its
      declarations. *)
  let added ~(before : (unit, A.extended_op) Prog.pmod_item list)
      ~(after : (unit, A.extended_op) Prog.pmod_item list) :
      (unit, A.extended_op) Prog.pmod_item list =
    let grown = List.length after - List.length before in
    let rec split count prefix rest =
      if count <= 0 then (List.rev prefix, rest)
      else
        match rest with
        | [] -> (List.rev prefix, [])
        | declaration :: rest -> split (count - 1) (declaration :: prefix) rest
    in
    match split (Stdlib.max grown 0) [] after with
    | prefix, rest when rest == before -> prefix
    | _ -> after

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
            let facts = ref Facts.empty in
            let stopped = ref false in
            List.iter
              (fun item ->
                if not !stopped then
                  let snapshot = !environment in
                  match type_item snapshot item with
                  | Ok next ->
                      let decls =
                        added
                          ~before:(Pretyping.Env.decls snapshot)
                          ~after:(Pretyping.Env.decls next)
                      in
                      (match Collect.of_item ~file ~env:next ~item ~decls with
                      | collected -> facts := Facts.merge !facts collected
                      | exception exn ->
                          (* A fact the adapter cannot read is a failure of
                             this server, never of the user's file: it is
                             reported as such instead of ending the check. *)
                          let error = Error.of_exn exn in
                          if Error.belongs_to ~file error then
                            diagnostics :=
                              Error.to_diagnostic error :: !diagnostics);
                      environment := next
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
            ( { empty with diagnostics = List.rev !diagnostics; facts = !facts },
              Some environment ))

  let check ~(file : string) ~(text : string) ~(idirs : (string * string) list)
      : result =
    (* The formatter is redirected for the whole check, parse included: the
       lexer and the parser warn through the same channel as the type
       checker. [with_capture] restores the formatter on every path. *)
    let outcome, printed =
      Warnings.with_capture (fun () ->
          Options.apply ();
          let parsed = Parse.parse ~name:file text in
          if Parse.failed parsed then
            { empty with diagnostics = parsed.diagnostics }
          else fst (type_items ~file ~items:parsed.items ~idirs))
    in
    { outcome with warnings = Warnings.parse ~file printed }
end
