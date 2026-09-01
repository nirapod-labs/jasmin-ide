(** The analysis the server publishes.

    The engine owns the documents and the decision about when to analyse. This
    module gives the server the three calls it makes for that: analyse one
    document's current text, hand back the result, and forget a closed
    document. It reads {!Jide_engine.Engine.check}, which is the whole
    pipeline — parse, recover, type item by item, index, resolve — and stores
    what it built, so the requests that answer from a snapshot and the
    diagnostics published for the same text come from one analysis. *)

type stored = int * Jide_lang.Diagnostic.t list
(** The result of one analysis: the version of the text it describes and the
    full set of its diagnostics. *)

let create (engine : Jide_engine.Engine.t) : Jide_controller.Server.analysis =
  let stored : (string, stored) Hashtbl.t = Hashtbl.create 16 in
  let check ~uri =
    match Jide_engine.Engine.check engine ~uri with
    | None -> Hashtbl.remove stored uri
    | Some (analysis : Jide_engine.Engine.analysis) ->
        Hashtbl.replace stored uri
          ( analysis.Jide_engine.Engine.version,
            analysis.Jide_engine.Engine.diagnostics )
  in
  let diagnostics ~uri = Hashtbl.find_opt stored uri in
  let forget ~uri = Hashtbl.remove stored uri in
  { Jide_controller.Server.check; diagnostics; forget }
