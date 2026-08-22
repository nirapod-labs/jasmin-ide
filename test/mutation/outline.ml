(** The outline of a parsed file, and the comparison the criterion needs.

    An outline entry is what an outline view shows for a top-level item: its
    kind, its name and the bytes it covers. A namespace is an entry of its
    own and its items follow it, with the namespace's name in front of
    theirs, so two entries cannot share a name and a kind by accident.

    The criterion is "the outline of items away from the edit is unchanged".
    Away from the edit means: an item that ends before the touched range
    starts, or starts after it ends. Items that overlap the edit are what
    the recovery is for; they may change freely. An item away from the edit
    must still be there, in the same place in the outline, with the same
    kind and name, and, when the mutation's shift is undone, the same
    extent. *)

open Jasmin

type entry = {
  kind : string;  (** The item's keyword: [fn], [param], [namespace], ... *)
  name : string;  (** Its name, qualified by the namespaces around it. *)
  start_byte : int;
  end_byte : int;
}

let of_items (items : Syntax.pprogram) =
  let entries = ref [] in
  let add kind name (location : Location.t) =
    entries :=
      {
        kind;
        name;
        start_byte = location.loc_bchar;
        end_byte = location.loc_echar;
      }
      :: !entries
  in
  let rec walk prefix (items : Syntax.pitem Location.located list) =
    List.iter
      (fun item ->
        let location = Location.loc item in
        match Location.unloc item with
        | Syntax.PFundef definition ->
            add "fn" (prefix ^ Location.unloc definition.pdf_name) location
        | Syntax.PParam parameter ->
            add "param" (prefix ^ Location.unloc parameter.ppa_name) location
        | Syntax.PGlobal global ->
            add "global" (prefix ^ Location.unloc global.pgd_name) location
        | Syntax.Pexec execution ->
            add "exec" (prefix ^ Location.unloc execution.pex_name) location
        | Syntax.Prequire (name, _) ->
            let name =
              match name with
              | Some name -> Location.unloc name
              | None -> "<require>"
            in
            add "require" (prefix ^ name) location
        | Syntax.PTypeAlias (name, _, _) ->
            add "type" (prefix ^ Location.unloc name) location
        | Syntax.PNamespace (name, children) ->
            let qualified = prefix ^ Location.unloc name in
            add "namespace" qualified location;
            walk (qualified ^ "::") children)
      items
  in
  walk "" items;
  List.rev !entries

(** The closest item that covers the whole range, if any. *)
let enclosing entries ~start_byte ~end_byte =
  List.fold_left
    (fun best entry ->
      if entry.start_byte <= start_byte && entry.end_byte >= end_byte then
        match best with
        | None -> Some entry
        | Some current ->
            let cover entry = entry.end_byte - entry.start_byte in
            if cover entry < cover current then Some entry else best
      else best)
    None entries

let is_present entries entry =
  List.exists
    (fun candidate ->
      candidate.kind = entry.kind && candidate.name = entry.name)
    entries

type comparison = {
  away : int;  (** Entries the mutation had to leave alone. *)
  identity_ok : bool;  (** Each is still there, in place, with its name. *)
  span_ok : bool;  (** Each still covers the bytes it covered. *)
  detail : string;  (** The first difference, empty when there is none. *)
}

let unchanged = { away = 0; identity_ok = true; span_ok = true; detail = "" }

(** The entries an edit leaves alone: those that end at or before its first
    byte, or start at or after its last. An entry that covers the edit is in
    neither list: it is the item the recovery works on, and it may change. *)
let away_from entries ~edit_start ~edit_end =
  List.filter
    (fun entry -> entry.end_byte <= edit_start || entry.start_byte >= edit_end)
    entries

(** How many entries the mutation had to leave alone. *)
let away_count entries ~edit_start ~edit_end =
  List.length (away_from entries ~edit_start ~edit_end)

let rec take count items =
  if count <= 0 then []
  else
    match items with [] -> [] | item :: rest -> item :: take (count - 1) rest

let rec drop count items =
  if count <= 0 then items
  else match items with [] -> [] | _ :: rest -> drop (count - 1) rest

let compare ~(original : entry list) ~(mutant : entry list) ~edit_start
    ~edit_end ~map_start ~map_end =
  let away_entries = away_from original ~edit_start ~edit_end in
  let before =
    List.filter (fun entry -> entry.end_byte <= edit_start) away_entries
  in
  let after =
    List.filter (fun entry -> entry.start_byte >= edit_end) away_entries
  in
  let away = List.length before + List.length after in
  (* An item that covers the edit is in neither list, and the mutant still
     holds it. Pairing the mutant by position would take that item for the
     first one after the edit and shift every pair after it, so the mutant is
     read in the original's coordinates and the same test splits it: an entry
     that covers the edit, mapped back, is not an away item either.

     The covering item may also have shrunk: a namespace cut in the middle
     of its body ends where the cut is, so mapped back it no longer covers
     the edit, and by position alone it would stand before its own items.
     It is still the item that covered the edit, and it is recognised as
     that item by what does not move: its kind, its name and its start,
     which lies before the edit. No away item shares all three with it. *)
  let covering =
    List.filter
      (fun entry ->
        not (entry.end_byte <= edit_start || entry.start_byte >= edit_end))
      original
  in
  let covers entry =
    List.exists
      (fun item ->
        item.kind = entry.kind && item.name = entry.name
        && item.start_byte = map_start entry.start_byte)
      covering
  in
  let mutant =
    List.filter
      (fun entry ->
        let start_byte = map_start entry.start_byte
        and end_byte = map_end entry.end_byte in
        (end_byte <= edit_start || start_byte >= edit_end) && not (covers entry))
      mutant
  in
  if List.length mutant < away then
    {
      away;
      identity_ok = false;
      span_ok = false;
      detail =
        Printf.sprintf
          "the mutant has %d entries and %d were away from the edit"
          (List.length mutant) away;
    }
  else
    let prefix = take (List.length before) mutant in
    let suffix = drop (List.length mutant - List.length after) mutant in
    let identity_ok = ref true and span_ok = ref true and detail = ref "" in
    let note message = if !detail = "" then detail := message in
    let lost () =
      identity_ok := false;
      span_ok := false
    in
    let check side expected actual =
      if expected.kind <> actual.kind || expected.name <> actual.name then (
        lost ();
        note
          (Printf.sprintf "%s the edit: expected %s %s, found %s %s" side
             expected.kind expected.name actual.kind actual.name))
      else
        let start_byte = map_start actual.start_byte in
        let end_byte = map_end actual.end_byte in
        if start_byte <> expected.start_byte || end_byte <> expected.end_byte
        then (
          span_ok := false;
          note
            (Printf.sprintf
               "%s the edit: %s %s covered bytes %d..%d, now %d..%d" side
               expected.kind expected.name expected.start_byte expected.end_byte
               start_byte end_byte))
    in
    let rec walk side expected actual =
      match (expected, actual) with
      | [], [] -> ()
      | [], item :: _ ->
          lost ();
          note
            (Printf.sprintf "%s the edit: unexpected %s %s" side item.kind
               item.name)
      | item :: _, [] ->
          lost ();
          note
            (Printf.sprintf "%s the edit: %s %s is missing" side item.kind
               item.name)
      | expected :: expected_rest, actual :: actual_rest ->
          check side expected actual;
          walk side expected_rest actual_rest
    in
    walk "before" before prefix;
    walk "after" after suffix;
    { away; identity_ok = !identity_ok; span_ok = !span_ok; detail = !detail }
