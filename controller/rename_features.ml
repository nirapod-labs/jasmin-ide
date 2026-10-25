(* The engine's answers to prepareRename and rename, in the shape {!Features}
   asks for them. The native server and the benchmark build their features
   from these two functions, so the two cannot answer a rename differently.

   A rename is answered from the snapshot the last check of the document left:
   the editing request checks a document whose text is newer than its analysis
   before it asks, so the snapshot holds the text the client has. *)

(* The files of the document's require closure that hold an error, by path. A
   rename refuses where one of them is broken: its index is not one to edit
   other files from. No analysis stored means none is broken. *)
let broken_paths engine ~uri =
  match Jide_engine.Engine.analysis engine ~uri with
  | None -> fun _ -> false
  | Some analysis ->
      let paths =
        List.filter_map
          (fun (required : Jide_engine.Engine.required) ->
            Option.map Jide_engine.Index.normalise
              (Jide_lang.Uri.path
                 (Jide_lang.Uri.of_string required.Jide_engine.Engine.uri)))
          analysis.Jide_engine.Engine.required
      in
      fun path ->
        List.exists (String.equal (Jide_engine.Index.normalise path)) paths

(* The file a rename edits, as the protocol names it, with the line index of
   the text its resolution read: the document's own text from the engine, any
   other file from the index its resolution holds. *)
let rename_file engine ~uri (snapshot : Jide_engine.Engine.snapshot) path =
  let path = Jide_engine.Index.normalise path in
  let document_path =
    Jide_engine.Index.normalise
      snapshot.Jide_engine.Engine.index.Jide_engine.Index.file
  in
  if String.equal path document_path then
    let lines =
      match Jide_engine.Engine.document engine ~uri with
      | Some document -> Jide_engine.Document.lines document
      | None ->
          Jide_lang.Line_index.create
            snapshot.Jide_engine.Engine.index.Jide_engine.Index.text
    in
    { Features.uri; lines }
  else
    let text =
      match
        Jide_engine.Resolve.file snapshot.Jide_engine.Engine.resolve path
      with
      | Some index -> index.Jide_engine.Index.text
      | None -> ""
    in
    {
      Features.uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path);
      lines = Jide_lang.Line_index.create text;
    }

let prepare engine ~uri ~byte =
  match Jide_engine.Engine.snapshot engine ~uri with
  | None -> None
  | Some snapshot -> (
      match
        Jide_engine.Rename.prepare ~index:snapshot.Jide_engine.Engine.index
          ~resolve:snapshot.Jide_engine.Engine.resolve ~byte
          ~broken:(broken_paths engine ~uri)
      with
      | Ok (span, _kind) -> Some span
      | Error _ -> None)

let rename engine ~uri ~byte ~new_name =
  match Jide_engine.Engine.snapshot engine ~uri with
  | None -> Some (Error "rename: the document has no analysis to rename from")
  | Some snapshot -> (
      match
        Jide_engine.Rename.rename ~files:[]
          ~index:snapshot.Jide_engine.Engine.index
          ~resolve:snapshot.Jide_engine.Engine.resolve ~byte ~new_name
          ~broken:(broken_paths engine ~uri)
      with
      | Error reason -> Some (Error reason)
      | Ok edits ->
          Some
            (Ok
               (List.map
                  (fun (path, group) ->
                    {
                      Features.file = rename_file engine ~uri snapshot path;
                      edits =
                        List.map
                          (fun (edit : Jide_engine.Rename.edit) ->
                            {
                              Jide_engine.Types.span =
                                edit.Jide_engine.Rename.span;
                              new_text = edit.Jide_engine.Rename.new_text;
                            })
                          group;
                    })
                  edits)))
