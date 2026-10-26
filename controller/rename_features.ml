(* The engine's answers to prepareRename and rename, in the shape {!Features}
   asks for them. The native server and the benchmark build their features
   from these two functions, so the two cannot answer a rename differently.

   A rename is answered from the snapshot the last check of the document left:
   the editing request checks a document whose text is newer than its analysis
   before it asks, so the snapshot holds the text the client has. The other
   files it reads are the project's entries, each resolved once and kept by
   the engine until a file changes: a declaration in the document may be used
   by any entry that reads it, and a rename that left one of those uses behind
   would break that entry. *)

(* A file with syntax errors is refused by the engine's own rule, which reads
   the resolutions' parses; nothing else is known to be unfit here. *)
let nothing_broken _ = false

(* The file a rename edits, as the protocol names it, with the line index of
   the text its resolution read: the document's own text from the engine, any
   other file from the index a resolution holds. *)
let rename_file engine ~uri (snapshot : Jide_engine.Engine.snapshot) ~files path
    =
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
      List.find_map
        (fun resolution ->
          Option.map
            (fun (index : Jide_engine.Index.t) -> index.Jide_engine.Index.text)
            (Jide_engine.Resolve.file resolution path))
        (snapshot.Jide_engine.Engine.resolve :: files)
      |> Option.value ~default:""
    in
    let file_uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path) in
    (* An open document is named by the uri the client opened it with. *)
    let file_uri =
      match Jide_engine.Engine.document engine ~uri:file_uri with
      | Some document -> Jide_engine.Document.uri document
      | None -> file_uri
    in
    { Features.uri = file_uri; lines = Jide_lang.Line_index.create text }

let prepare engine ~uri ~byte =
  match Jide_engine.Engine.snapshot engine ~uri with
  | None -> None
  | Some snapshot -> (
      let files = Jide_engine.Engine.project_resolutions engine ~uri in
      match
        Jide_engine.Rename.prepare ~files
          ~index:snapshot.Jide_engine.Engine.index
          ~resolve:snapshot.Jide_engine.Engine.resolve ~byte
          ~broken:nothing_broken
      with
      | Ok (span, _kind) -> Some (Ok span)
      | Error reason -> Some (Error reason))

let rename engine ~uri ~byte ~new_name =
  match Jide_engine.Engine.snapshot engine ~uri with
  | None -> Some (Error "rename: the document has no analysis to rename from")
  | Some snapshot -> (
      let files = Jide_engine.Engine.project_resolutions engine ~uri in
      match
        Jide_engine.Rename.rename ~files
          ~index:snapshot.Jide_engine.Engine.index
          ~resolve:snapshot.Jide_engine.Engine.resolve ~byte ~new_name
          ~broken:nothing_broken
      with
      | Error reason -> Some (Error reason)
      | Ok edits ->
          Some
            (Ok
               (List.map
                  (fun (path, group) ->
                    {
                      Features.file =
                        rename_file engine ~uri snapshot ~files path;
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
