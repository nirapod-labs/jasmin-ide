type t = { scheme : string; authority : string option; path : string }

let is_scheme_byte c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' -> true
  | _ -> false

(* The index of the colon that ends a scheme, when the text starts with one:
   a letter followed by letters, digits, plus, minus and full stop. *)
let scheme_end text =
  let length = String.length text in
  let rec scan index =
    if index >= length then None
    else if Char.equal (String.get text index) ':' then Some index
    else if is_scheme_byte (String.get text index) then scan (index + 1)
    else None
  in
  if length = 0 then None
  else
    match String.get text 0 with
    | 'a' .. 'z' | 'A' .. 'Z' -> scan 1
    | _ -> None

let hex_value c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let decode text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec go index =
    if index >= length then ()
    else if Char.equal (String.get text index) '%' && index + 2 < length then
      let high = hex_value (String.get text (index + 1)) in
      let low = hex_value (String.get text (index + 2)) in
      if high < 0 || low < 0 then begin
        Buffer.add_char buffer (String.get text index);
        go (index + 1)
      end
      else begin
        Buffer.add_char buffer (Char.chr ((high * 16) + low));
        go (index + 3)
      end
    else begin
      Buffer.add_char buffer (String.get text index);
      go (index + 1)
    end
  in
  go 0;
  Buffer.contents buffer

let is_unreserved c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let encode text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      if is_unreserved c || Char.equal c '/' then Buffer.add_char buffer c
      else begin
        Buffer.add_string buffer "%";
        Buffer.add_string buffer (Printf.sprintf "%02X" (Char.code c))
      end)
    text;
  Buffer.contents buffer

(* The first index at or after [from] whose byte satisfies [is_stop], or the
   length of the text. *)
let stop_at text from is_stop =
  let length = String.length text in
  let rec go index =
    if index >= length then length
    else if is_stop (String.get text index) then index
    else go (index + 1)
  in
  go from

let is_path_end c = Char.equal c '?' || Char.equal c '#'
let is_authority_end c = Char.equal c '/' || is_path_end c

let of_string text =
  match scheme_end text with
  | None -> { scheme = ""; authority = None; path = text }
  | Some colon ->
    let scheme = String.lowercase_ascii (String.sub text 0 colon) in
    let start = colon + 1 in
    let length = String.length text in
    if
      start + 1 < length
      && Char.equal (String.get text start) '/'
      && Char.equal (String.get text (start + 1)) '/'
    then begin
      let authority_start = start + 2 in
      let authority_stop = stop_at text authority_start is_authority_end in
      let path_stop = stop_at text authority_stop is_path_end in
      let authority =
        String.sub text authority_start (authority_stop - authority_start)
      in
      let path = String.sub text authority_stop (path_stop - authority_stop) in
      { scheme; authority = Some authority; path = decode path }
    end
    else begin
      let path_stop = stop_at text start is_path_end in
      {
        scheme;
        authority = None;
        path = decode (String.sub text start (path_stop - start));
      }
    end

let to_string uri =
  if String.equal uri.scheme "" then uri.path
  else
    match uri.authority with
    | Some authority -> uri.scheme ^ "://" ^ authority ^ encode uri.path
    | None -> uri.scheme ^ ":" ^ encode uri.path

let is_local authority =
  String.equal (String.lowercase_ascii authority) "localhost"

let path uri =
  if not (String.equal uri.scheme "file") then None
  else
    match uri.authority with
    | None -> Some uri.path
    | Some authority when String.equal authority "" -> Some uri.path
    | Some authority when is_local authority -> Some uri.path
    | Some _ -> None

let of_path path =
  let absolute = String.length path > 0 && Char.equal (String.get path 0) '/' in
  if absolute then { scheme = "file"; authority = Some ""; path }
  else { scheme = "file"; authority = None; path }

let authority_key authority =
  match authority with
  | None -> ""
  | Some authority -> String.lowercase_ascii authority

let equal a b =
  String.equal a.scheme b.scheme
  && String.equal (authority_key a.authority) (authority_key b.authority)
  && String.equal a.path b.path
