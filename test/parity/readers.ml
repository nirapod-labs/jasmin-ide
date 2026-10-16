(** How the checker, the compiler and the release's record print a first
    error, read into one shape. Shared by the verdict parity run and the
    first-error run on mutants, so both read the two tools alike. *)

(* ------------------------------------------------------------------ *)
(* Text *)

let is_digit c = c >= '0' && c <= '9'
let is_blank s = String.for_all (fun c -> c = ' ' || c = '\t') s

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

let drop n s = String.sub s n (String.length s - n)

(* One space for every run of blanks, none at either end. *)
let collapse s =
  let b = Buffer.create (String.length s) in
  let pending = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' then pending := Buffer.length b > 0
      else (
        if !pending then Buffer.add_char b ' ';
        pending := false;
        Buffer.add_char b c))
    s;
  Buffer.contents b

let index_from s i sub =
  let n = String.length s and k = String.length sub in
  let rec go i =
    if i + k > n then None
    else if String.sub s i k = sub then Some i
    else go (i + 1)
  in
  go i

let replace_all s ~sub ~by =
  let b = Buffer.create (String.length s) in
  let k = String.length sub in
  let rec go i =
    match index_from s i sub with
    | None -> Buffer.add_string b (drop i s)
    | Some j ->
        Buffer.add_string b (String.sub s i (j - i));
        Buffer.add_string b by;
        go (j + k)
  in
  if k = 0 then s
  else (
    go 0;
    Buffer.contents b)

(* The digits at [i], as a number and the index after them. *)
let digits s i =
  let n = String.length s in
  let rec go j = if j < n && is_digit s.[j] then go (j + 1) else j in
  let j = go i in
  if j = i then None else Some (int_of_string (String.sub s i (j - i)), j)

(* The number after the first [word] followed by digits. *)
let number_after s word =
  let rec go i =
    match index_from s i word with
    | None -> None
    | Some j -> (
        match digits s (j + String.length word) with
        | Some (v, _) -> Some v
        | None -> go (j + 1))
  in
  go 0

let rec spaces s = if starts_with ~prefix:" " s then spaces (drop 1 s) else s

(* Whether a line holds [line N (N], as a location does. *)
let has_location line =
  let rec go i =
    match index_from line i "line " with
    | None -> false
    | Some j -> (
        match digits line (j + 5) with
        | Some (_, k)
          when starts_with ~prefix:" (" (drop k line)
               && k + 2 < String.length line
               && is_digit line.[k + 2] ->
            true
        | _ -> go (j + 1))
  in
  go 0

let kinds =
  [ "parse"; "typing"; "annotation"; "pretyping"; "compilation"; "internal" ]

(* A message without the compiler's kind prefix: [parse error:] and the rest. *)
let strip_kind s =
  match
    List.find_map
      (fun kind ->
        let prefix = kind ^ " error" in
        if starts_with ~prefix s then Some (drop (String.length prefix) s)
        else None)
      kinds
  with
  | None -> s
  | Some rest ->
      let rest = if starts_with ~prefix:":" rest then drop 1 rest else rest in
      spaces rest

(* One spacing and no kind prefix, except that a message that is only its
   prefix ([parse error]) is kept: the release records some that way. *)
let normalise text =
  let text = collapse text in
  let bare = strip_kind text in
  if bare <> "" then bare else text

let word kind =
  if ends_with ~suffix:" error" kind then
    String.sub kind 0 (String.length kind - 6)
  else kind

let is_pass_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = ' '

(* The leading [name:] of a text, letters and spaces only. *)
let leading_name t =
  let n = String.length t in
  let rec go i = if i < n && is_pass_char t.[i] then go (i + 1) else i in
  let i = go 0 in
  if i > 0 && i < n && t.[i] = ':' then Some (String.sub t 0 i, drop (i + 1) t)
  else None

(* The pass a message names: the word before its first colon, after an
   [in function f:] that comes first. *)
let sub_of text =
  let t =
    if starts_with ~prefix:"in function " text then
      match index_from text 12 ": " with
      | Some j -> drop (j + 2) text
      | None -> text
    else text
  in
  match leading_name t with Some (name, _) -> name | None -> ""

(* ------------------------------------------------------------------ *)
(* The three readers *)

type first = { line : int; column : int; kind : string; message : string }

let nothing = { line = 0; column = 0; kind = ""; message = "" }

(* The first error [jasmin-lsp check] prints, [path:line:column: kind:
   message], with a zero-based column. Warnings are not errors. *)
let checker_first out =
  let located line =
    let n = String.length line in
    let rec scan i =
      if i >= n then None
      else if line.[i] = ':' then
        match digits line (i + 1) with
        | Some (l, j) when j < n && line.[j] = ':' -> (
            match digits line (j + 1) with
            | Some (c, k) when k + 1 < n && line.[k] = ':' && line.[k + 1] = ' '
              ->
                Some (l, c, drop (k + 2) line)
            | _ -> scan (i + 1))
        | _ -> scan (i + 1)
      else scan (i + 1)
    in
    scan 0
  in
  let rec go = function
    | [] -> nothing
    | line :: rest -> (
        match located line with
        | None -> go rest
        | Some (l, c, rest_text) ->
            let kind =
              match String.index_opt rest_text ':' with
              | Some i -> String.sub rest_text 0 i
              | None -> rest_text
            in
            if kind = "warning" then go rest
            else
              let message =
                if String.length rest_text >= String.length kind + 2 then
                  drop (String.length kind + 2) rest_text
                else ""
              in
              let message =
                replace_all
                  (replace_all
                     (replace_all message ~sub:"\\n" ~by:" ")
                     ~sub:"\\t" ~by:" ")
                  ~sub:"\\\\" ~by:"\\"
              in
              { line = l; column = c - 1; kind; message = collapse message })
  in
  go (String.split_on_char '\n' out)

(* A compiler location line, ["file", line 3 (2-15):] or with [to line 4
   (1)]: the line, the column and the text after it. *)
let location_line line =
  if not (starts_with ~prefix:"\"" line) then None
  else
    match String.index_from_opt line 1 '"' with
    | None -> None
    | Some q -> (
        let parse_range i =
          match digits line i with
          | Some (_, j) when j < String.length line && line.[j] = '-' -> (
              match digits line (j + 1) with
              | Some (_, k) -> Some k
              | None -> None)
          | Some (_, j) -> Some j
          | None -> None
        in
        let rest = drop (q + 1) line in
        if not (starts_with ~prefix:", line " rest) then None
        else
          let i = q + 1 + 7 in
          match digits line i with
          | Some (l, j) when starts_with ~prefix:" (" (drop j line) -> (
              match digits line (j + 2) with
              | Some (c, _) -> (
                  match parse_range (j + 2) with
                  | Some k when k < String.length line && line.[k] = ')' ->
                      let k = k + 1 in
                      let k =
                        if starts_with ~prefix:" to line " (drop k line) then
                          match digits line (k + 9) with
                          | Some (_, m)
                            when starts_with ~prefix:" (" (drop m line) -> (
                              match parse_range (m + 2) with
                              | Some e
                                when e < String.length line && line.[e] = ')' ->
                                  e + 1
                              | _ -> -1)
                          | _ -> -1
                        else k
                      in
                      if k > 0 && k < String.length line && line.[k] = ':' then
                        let at = Option.get (index_from line 0 "):") in
                        Some (l, c, drop (at + 2) line)
                      else None
                  | _ -> None)
              | None -> None)
          | _ -> None)

