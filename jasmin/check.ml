open Jasmin

type result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
  facts : Facts.t;
}

let empty = { diagnostics = []; warnings = []; facts = Facts.empty }

(** The most diagnostics one check returns, syntax and typing together. *)
let max_diagnostics = 200

(** The first [count] entries of a list. *)
let rec take count = function
  | _ when count <= 0 -> []
  | [] -> []
  | entry :: rest -> entry :: take (count - 1) rest

(** The kind of name an item declares, as the compiler's own messages spell
    it. An item that did not reach the environment is remembered by its kind,
    because the message a later reference to it produces depends on that
    kind: a function is an unknown function, a parameter or a global is an
    unknown variable, and a type alias is a type the compiler cannot find. *)
type declaration = Function of string | Variable of string | Alias of string

(** The message the compiler prints for a reference to [declaration] it
    cannot resolve. A name that was dropped by recovery or whose item failed
    to type is not in the environment, so every later reference to it would
    produce this message; it is a consequence of the first failure, not a
    second one, and the typing loop drops it. *)
let unresolved_message = function
  | Function name -> Printf.sprintf "unknown function: `%s'" name
  | Variable name -> Printf.sprintf "unknown variable: `%s'" name
  | Alias name -> Printf.sprintf "Type '%s' not found" name

(** The declaration an item makes, when the compiler names the item by one. *)
let declaration_of_item (item : Syntax.pitem Location.located) =
  match Location.unloc item with
  | Syntax.PFundef definition ->
      Some (Function (Location.unloc definition.pdf_name))
  | Syntax.PParam parameter ->
      Some (Variable (Location.unloc parameter.ppa_name))
  | Syntax.PGlobal global -> Some (Variable (Location.unloc global.pgd_name))
  | Syntax.PTypeAlias (name, _, _) -> Some (Alias (Location.unloc name))
  | Syntax.Pexec _ | Syntax.Prequire _ | Syntax.PNamespace _ -> None

(** The declaration a stub stands for. A namespace has no single name, so a
    dropped one contributes nothing here: the names inside it are not known
    until the namespace parses. *)
let declaration_of_dropped = function
  | Recover.Dropped { kind = "function"; name = Some name; _ } ->
      Some (Function name)
  | Recover.Dropped { kind = "parameter"; name = Some name; _ } ->
      Some (Variable name)
  | Recover.Dropped { kind = "global"; name = Some name; _ } ->
      Some (Variable name)
  | Recover.Dropped { kind = "type"; name = Some name; _ } -> Some (Alias name)
  | Recover.Dropped _ -> None

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

  (** Entering and leaving a file are calls into the library like any other:
      the environment they build is threaded through the items, so a raise
      in either is turned into a diagnostic here rather than escaping. *)
  let enter_file (environment : environment) (file : string) =
    match Pretyping.Env.enter_file environment None None file with
    | entered -> Ok entered
    | exception exn -> Error exn

  let exit_file (environment : environment) =
    match Pretyping.Env.exit_file environment with
    | environment -> Ok environment
    | exception exn -> Error exn

  (** [type_items ~file ~items ~idirs] types the items of one file one by one
      and returns the diagnostics, plus the environment the loop leaves
      behind when it started at all.

      Before each item the environment is kept, and a failed item leaves the
      environment as it was, so the items after it still type. The names of
      the items recovery dropped, and of the items whose typing failed, are
      remembered: a later reference to one of them is the same failure seen
      again and is not reported a second time. A diagnostic whose span lies
      in a region recovery rewrote or skipped is dropped for the same
      reason.

      At most {!max_diagnostics} diagnostics are kept. Every call into the
      library is wrapped: no exception leaves this module. *)
  let type_items ~(dropped : Recover.dropped list)
      ~(recovered : Jide_lang.Span.t list) ~(file : string)
      ~(items : Syntax.pprogram) ~(idirs : (string * string) list) :
      result * environment option =
    let initial =
      match List.fold_left Pretyping.Env.add_from Pretyping.Env.empty idirs with
      | environment -> Ok environment
      | exception exn -> Error exn
    in
    match initial with
    | Error exn ->
        ({ empty with diagnostics = [ Error.diagnostic_of_exn exn ] }, None)
    | Ok initial -> (
        match enter_file initial file with
        | Error exn ->
            ({ empty with diagnostics = [ Error.diagnostic_of_exn exn ] }, None)
        | Ok None ->
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
        | Ok (Some (environment, _resolved)) ->
            let environment = ref environment in
            let diagnostics = ref [] in
            let facts = ref Facts.empty in
            let kept = ref 0 in
            let stopped = ref false in
            let suppressed = ref [] in
            let suppress declaration =
              suppressed := unresolved_message declaration :: !suppressed
            in
            List.iter
              (fun dropped ->
                Option.iter suppress (declaration_of_dropped dropped))
              dropped;
            let keep (error : Error.t) =
              let recovered_here =
                List.exists
                  (fun (span : Jide_lang.Span.t) ->
                    Jide_lang.Span.overlaps span error.Error.span)
                  recovered
              in
              if
                Error.belongs_to ~file error
                && (not (List.mem error.Error.message !suppressed))
                && not recovered_here
              then (
                diagnostics := Error.to_diagnostic error :: !diagnostics;
                incr kept;
                if !kept >= max_diagnostics then stopped := true)
            in
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
