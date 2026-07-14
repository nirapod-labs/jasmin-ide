type t = {
  arch : Jide_jasmin.Adapter.arch option;
  call_conv : Jide_jasmin.Adapter.call_conv option;
  idirs : (string * string) list;
  entries : string list;
}

let empty = { arch = None; call_conv = None; idirs = []; entries = [] }
let file_name = "jasmin-project.json"

type problem = { span : Jide_lang.Span.t; message : string }

type loaded = {
  path : string option;
  text : string;
  bom : int;
  project : t;
  problems : problem list;
}

let blank_span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0

(* The same rule as the engine's: a byte-order mark is not part of the JSON
   and every byte offset moves back by it. *)
let strip_bom text =
  let length = String.length text in
  if
    length >= 3
    && Char.code text.[0] = 0xEF
    && Char.code text.[1] = 0xBB
    && Char.code text.[2] = 0xBF
  then (3, String.sub text 3 (length - 3))
  else (0, text)

(* Every top-level member's name, the range of that name and the range of its
   value, in the order the text holds them. The JSON has already been
   accepted, so this only has to walk it; a text this cannot follow yields no
   ranges at all, and an unknown field then sits at the first byte. *)
let member_ranges text =
  let length = String.length text in
  let skip_space index =
    let rec loop index =
      if index >= length then index
      else
        match text.[index] with
        | ' ' | '\t' | '\r' | '\n' -> loop (index + 1)
        | _ -> index
    in
    loop index
  in
  (* The index just past the string that starts at [index], and the string
     with its escapes resolved. *)
  let scan_string index =
    let buffer = Buffer.create 16 in
    let rec loop index =
      if index >= length then (index, Buffer.contents buffer)
      else
        match text.[index] with
        | '"' -> (index + 1, Buffer.contents buffer)
        | '\\' when index + 1 < length ->
            let escaped = text.[index + 1] in
            let character =
              match escaped with
              | 'n' -> '\n'
              | 't' -> '\t'
              | 'r' -> '\r'
              | 'b' -> '\b'
              | 'f' -> '\012'
              | other -> other
            in
            Buffer.add_char buffer character;
            loop (index + 2)
        | character ->
            Buffer.add_char buffer character;
            loop (index + 1)
    in
    loop (index + 1)
  in
  (* The index just past the value that starts at [index]. A string's braces
     and brackets do not count, which is what makes this walk safe. *)
  let rec scan_value index =
    let index = skip_space index in
    if index >= length then index
    else
      match text.[index] with
      | '"' -> fst (scan_string index)
      | '{' -> scan_container index '{' '}'
      | '[' -> scan_container index '[' ']'
      | _ ->
          let rec loop index =
            if index >= length then index
            else
              match text.[index] with
              | ',' | '}' | ']' -> index
              | _ -> loop (index + 1)
          in
          loop index
  and scan_container index open_ close =
    let rec loop depth index =
      if index >= length then index
      else
        match text.[index] with
        | '"' -> loop depth (fst (scan_string index))
        | character when character = open_ -> loop (depth + 1) (index + 1)
        | character when character = close ->
            if depth = 1 then index + 1 else loop (depth - 1) (index + 1)
        | _ -> loop depth (index + 1)
    in
    loop 0 index
  in
  let rec members index acc =
    let index = skip_space index in
    if index >= length then List.rev acc
    else
      match text.[index] with
      | '"' ->
          let start = index in
          let key_end, name = scan_string index in
          let next = skip_space key_end in
          let after_key =
            if next < length && text.[next] = ':' then skip_space (next + 1)
            else next
          in
          let after_value = scan_value after_key in
          let acc =
            ( name,
              ( Jide_lang.Span.make ~start_byte:start ~end_byte:key_end,
                Jide_lang.Span.make ~start_byte:after_key ~end_byte:after_value
              ) )
            :: acc
          in
          let after_value = skip_space after_value in
          if after_value < length && text.[after_value] = ',' then
            members (after_value + 1) acc
          else List.rev acc
      | _ -> List.rev acc
  in
  let start = skip_space 0 in
  if start < length && text.[start] = '{' then members (start + 1) [] else []

(* The names of the values the file may hold. Their order is the order the
   error message lists the accepted values in. *)
let arch_names = Jide_jasmin.Adapter.arch_names
let call_conv_names = [ "linux"; "windows" ]
let quoted names = String.concat ", " names

(* A path relative to the project file's directory, as an absolute one. *)
let resolve ~directory path =
  if Filename.is_relative path then Filename.concat directory path else path

