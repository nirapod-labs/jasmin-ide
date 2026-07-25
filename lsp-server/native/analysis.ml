(** The analysis the server publishes.

    The engine owns the documents and the decision about when to analyse;
    this module gives the server the two calls it makes for that: analyse
    one document's current text, and hand back the result. Until the engine
    has a check of its own, the analysis is the adapter's, converted the way
    the batch checker converts it, so the protocol and the command line
    report the same diagnostics from the same code. Replacing this module's
    body with the engine's check changes no protocol code.

    The architecture is the checker's default: the project file that names
    one belongs to the engine, and no project file is read yet. *)

let default_arch = "x86-64"
let default_call_conv = "linux"

type stored = int * Jide_lang.Diagnostic.t list
(** The result of one analysis: the version of the text it describes and the
    full set of its diagnostics. *)

let create (engine : Jide_engine.Engine.t) :
    Jide_controller.Server.analysis option =
  match
    ( Jide_jasmin.Adapter.arch_of_string default_arch,
      Jide_jasmin.Adapter.call_conv_of_string default_call_conv )
  with
  | Some arch, Some call_conv ->
      let stored : (string, stored) Hashtbl.t = Hashtbl.create 16 in
      let check ~uri =
        match Jide_engine.Engine.document engine ~uri with
        | None -> Hashtbl.remove stored uri
        | Some document ->
            let text = Jide_engine.Document.text document in
            let version = Jide_engine.Document.version document in
            let file =
              match Jide_engine.Document.path document with
              | Some path -> path
              | None -> uri
            in
            let diagnostics =
              match
                Jide_jasmin.Adapter.check arch call_conv ~file ~text ~idirs:[]
              with
              | result -> Jide_check.Report.of_check ~file ~text result
              | exception exn ->
                  [ Jide_check.Report.internal (Printexc.to_string exn) ]
            in
            Hashtbl.replace stored uri (version, diagnostics)
      in
      let diagnostics ~uri = Hashtbl.find_opt stored uri in
      let forget ~uri = Hashtbl.remove stored uri in
      Some { Jide_controller.Server.check; diagnostics; forget }
  | _ -> None
