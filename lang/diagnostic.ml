type severity = Error | Warning | Information | Hint

type t = {
  span : Span.t;
  severity : severity;
  message : string;
  code : string option;
  source : string;
  related : (Span.t * string) list;
}

let default_source = "jasmin"

let make ?code ?source ?related ~severity span message =
  let source =
    match source with Some source -> source | None -> default_source
  in
  let related = match related with Some related -> related | None -> [] in
  { span; severity; message; code; source; related }

let compare a b =
  let by_span = Span.compare a.span b.span in
  if by_span <> 0 then by_span else String.compare a.message b.message
