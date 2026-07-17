type t = {
  documents : (string, Document.t) Hashtbl.t;
  mutable root : string option;
}

let create () = { documents = Hashtbl.create 16; root = None }
let set_root t root = t.root <- root
let root t = t.root

let open_document t ~uri ~version ~text =
  Hashtbl.replace t.documents uri (Document.create ~uri ~version ~text)

let change_document t ~uri ~version ~text =
  match Hashtbl.find_opt t.documents uri with
  | Some document ->
      Hashtbl.replace t.documents uri
        (Document.with_text document ~version text)
  | None -> ()

let close_document t ~uri = Hashtbl.remove t.documents uri
let document t ~uri = Hashtbl.find_opt t.documents uri

let documents t =
  Hashtbl.fold (fun _ document acc -> document :: acc) t.documents []
  |> List.sort (fun a b -> String.compare (Document.uri a) (Document.uri b))
