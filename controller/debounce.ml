type t = { deadlines : (string, float) Hashtbl.t }

let create () = { deadlines = Hashtbl.create 8 }
let mark t ~uri ~deadline = Hashtbl.replace t.deadlines uri deadline
let stale t ~uri = Hashtbl.mem t.deadlines uri
let clear t ~uri = Hashtbl.remove t.deadlines uri

let due t ~now =
  let ready =
    Hashtbl.fold
      (fun uri deadline ready ->
        if deadline <= now then (deadline, uri) :: ready else ready)
      t.deadlines []
  in
  let ready =
    List.sort
      (fun (left_deadline, left_uri) (right_deadline, right_uri) ->
        let by_deadline = Float.compare left_deadline right_deadline in
        if by_deadline <> 0 then by_deadline
        else String.compare left_uri right_uri)
      ready
  in
  List.iter (fun (_, uri) -> Hashtbl.remove t.deadlines uri) ready;
  List.map snd ready

let next_deadline t ~now =
  Hashtbl.fold
    (fun _uri deadline earliest ->
      let left = Float.max 0.0 (deadline -. now) in
      match earliest with
      | None -> Some left
      | Some best -> Some (Float.min best left))
    t.deadlines None
