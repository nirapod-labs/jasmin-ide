(** The analysis the server publishes.

    The engine owns the documents and the decision about when to analyse. This
    module gives the server the calls it makes for that: analyse one
    document's current text, hand back the result, hand back the files it
    requires that hold an error, and forget a closed document. It reads
    {!Jide_engine.Engine.check}, which is the whole pipeline (parse, recover,
    type item by item, index, resolve) and stores what it built, so the
    requests that answer from a snapshot and the diagnostics published for the
    same text come from one analysis. *)

type stored = {
  version : int;  (** The version of the text the analysis describes. *)
  diagnostics : Jide_lang.Diagnostic.t list;  (** The document's own set. *)
  required : Jide_controller.Server.required list;
      (** The files it requires that hold an error, each with the text its
          diagnostics index. *)
}
(** The result of one analysis. *)

let create (engine : Jide_engine.Engine.t) : Jide_controller.Server.analysis =
  let stored : (string, stored) Hashtbl.t = Hashtbl.create 16 in
  let check ~uri =
    match Jide_engine.Engine.check engine ~uri with
    | None -> Hashtbl.remove stored uri
    | Some (analysis : Jide_engine.Engine.analysis) ->
        let required =
          List.map
            (fun (file : Jide_engine.Engine.required) ->
              {
                Jide_controller.Server.uri = file.Jide_engine.Engine.uri;
                text = file.Jide_engine.Engine.text;
                diagnostics = file.Jide_engine.Engine.diagnostics;
              })
            analysis.Jide_engine.Engine.required
        in
        Hashtbl.replace stored uri
          {
            version = analysis.Jide_engine.Engine.version;
            diagnostics = analysis.Jide_engine.Engine.diagnostics;
            required;
          }
  in
  let diagnostics ~uri =
    Option.map
      (fun (result : stored) -> (result.version, result.diagnostics))
      (Hashtbl.find_opt stored uri)
  in
  let required ~uri =
    match Hashtbl.find_opt stored uri with
    | Some result -> result.required
    | None -> []
  in
  let forget ~uri = Hashtbl.remove stored uri in
  { Jide_controller.Server.check; diagnostics; required; forget }
