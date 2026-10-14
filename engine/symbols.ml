(* Workspace symbols over the project's indexed files.

   A filter over declarations the index already holds: no parse, no resolve,
   no check. Case-insensitive, because a client's symbol palette is typed in
   whatever case the user's fingers produce, and substring, because the
   user's query is a fragment of a name rather than a whole one. *)

type symbol = {
  file : string;
  name : string;
  span : Jide_lang.Span.t;
  name_span : Jide_lang.Span.t;
  kind : Index.kind;
  detail : string option;
}

let fold_case (text : string) = String.lowercase_ascii text

let contains ~(needle : string) (haystack : string) =
  let needle = fold_case needle in
  let haystack = fold_case haystack in
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  if needle_length = 0 then true
  else
    let rec from index =
      if index + needle_length > haystack_length then false
      else if String.equal (String.sub haystack index needle_length) needle then
        true
      else from (index + 1)
    in
    from 0

let matches ~(query : string) (symbol : symbol) =
  contains ~needle:query symbol.name

(* A declaration the index marks [Unknown] is the resolver's fallback for a
   range it could not find, not a name the source introduces. *)
let indexed (declaration : Index.declaration) =
  declaration.Index.kind <> Index.Unknown

let of_declaration (file : string) (declaration : Index.declaration) =
  {
    file;
    name = declaration.Index.name;
    span = declaration.Index.item_span;
    name_span = declaration.Index.name_span;
    kind = declaration.Index.kind;
    detail = declaration.Index.detail;
  }

let search ~(files : Index.t list) ~(query : string) =
  List.concat_map
    (fun (index : Index.t) ->
      List.filter_map
        (fun (declaration : Index.declaration) ->
          if indexed declaration then
            let symbol = of_declaration index.Index.file declaration in
            if matches ~query symbol then Some symbol else None
          else None)
        index.Index.declarations)
    files
