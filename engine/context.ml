(* The context a file is checked in: an entry of the project, or the entry
   that requires it, directly or through other files. The rule is in the
   design's section on included files. *)

type t = {
  file : string;  (** the file the context is for, as a normalised path *)
  entry : string option;
      (** the entry it is checked in the context of; [None] means alone *)
  chain : string list;
      (** the entry first and [file] last; empty when the file is alone *)
}

let alone file = { file; entry = None; chain = [] }
let file t = t.file
let entry t = t.entry
let chain t = t.chain
let is_alone t = t.entry = None

(* One spelling per file: an absolute path with no [.] and no [..] left in
   it. Nothing here asks the filesystem, so a symbolic link is not resolved
   and two paths that name one file through different links stay different. *)
let normalise path =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let rec fold stack = function
    | [] -> List.rev stack
    | ("" | ".") :: rest -> fold stack rest
    | ".." :: rest -> (
        match stack with [] -> fold stack rest | _ :: stack -> fold stack rest)
    | part :: rest -> fold (part :: stack) rest
  in
  let body = String.concat "/" (fold [] parts) in
  if absolute then "/" ^ body else if body = "" then "." else body

let equal left right = String.equal (normalise left) (normalise right)

let is_letter character =
  (character >= 'a' && character <= 'z')
  || (character >= 'A' && character <= 'Z')

let is_identifier character =
  is_letter character
  || (character >= '0' && character <= '9')
  || character = '_' || character = '\'' || character = '.'

(* The specifications of every [require] a text holds, in the order it holds
   them. The scan skips comments, so a commented-out require is not one, and
   reads a run of string literals after the keyword, which the grammar
   allows. A string outside a require is skipped rather than read, so an
   [inline] block that happens to hold the word is not followed: the scan is
   lexical and knows nothing about nesting. *)
let requires_of_text text =
  let length = String.length text in
  let rec string_end index =
    if index >= length then index
    else
      match text.[index] with
      | '"' -> index + 1
      | '\\' -> string_end (index + 2)
      | _ -> string_end (index + 1)
  in
  let rec comment_end index =
    if index + 1 >= length then length
    else if text.[index] = '*' && text.[index + 1] = '/' then index + 2
    else comment_end (index + 1)
  in
  let rec line_end index =
    if index >= length then index
    else if text.[index] = '\n' then index + 1
    else line_end (index + 1)
  in
  let rec blanks index =
    if index >= length then index
    else
      match text.[index] with
      | ' ' | '\t' | '\r' | '\n' -> blanks (index + 1)
      | '/' when index + 1 < length && text.[index + 1] = '/' ->
          blanks (line_end (index + 2))
      | '/' when index + 1 < length && text.[index + 1] = '*' ->
          blanks (comment_end (index + 2))
      | _ -> index
  in
  (* The string literals that follow a [require], in the order they come. *)
  let rec literals index acc =
    let index = blanks index in
    if index < length && text.[index] = '"' then
      let stop = string_end (index + 1) in
      let last = min (stop - 1) length in
      let literal = String.sub text (index + 1) (max 0 (last - index - 1)) in
      literals stop (literal :: acc)
    else (index, List.rev acc)
  in
  let rec scan index acc =
    if index >= length then List.rev acc
    else
      match text.[index] with
      | '/' when index + 1 < length && text.[index + 1] = '/' ->
          scan (line_end (index + 2)) acc
      | '/' when index + 1 < length && text.[index + 1] = '*' ->
          scan (comment_end (index + 2)) acc
      | '"' -> scan (string_end (index + 1)) acc
      | character when is_letter character ->
          let rec read stop =
            if stop < length && is_identifier text.[stop] then read (stop + 1)
            else stop
          in
          let stop = read (index + 1) in
          let word = String.sub text index (stop - index) in
          if String.equal word "require" then
            let next, found = literals stop [] in
            scan next (List.rev_append found acc)
          else scan stop acc
      | _ -> scan (index + 1) acc
  in
  scan 0 []

(* One requirement as a path: [NAME:path] through the named include path
   [NAME], a bare [path] beside the file that requires it. [None] is a name
   the project does not give, so the file cannot be found through it. *)
let resolve ~idirs ~file specification =
  match String.index_opt specification ':' with
  | Some index when index > 0 ->
      let name = String.sub specification 0 index in
      let path =
        String.sub specification (index + 1)
          (String.length specification - index - 1)
      in
      Option.map
        (fun directory -> normalise (Filename.concat directory path))
        (List.assoc_opt name idirs)
  | _ ->
      Some (normalise (Filename.concat (Filename.dirname file) specification))

let required_files ~idirs ~read file =
  match read file with
  | None -> []
  | Some text ->
      List.filter_map
        (fun specification -> resolve ~idirs ~file specification)
        (requires_of_text text)

let with_extension extension file = Filename.check_suffix file extension

let is_directory path =
  match Sys.is_directory path with ok -> ok | exception Sys_error _ -> false

(* Every [.jazz] file under a directory, sorted, which is the entry set of a
   project that names none. Hidden directories and the build directory are
   not searched: neither holds a Jasmin entry. A depth of eight is more than
   a Jasmin tree uses and keeps a symbolic link loop from running forever. *)
let entries_of_directory directory =
  let rec walk depth directory acc =
    if depth > 8 then acc
    else
      match Sys.readdir directory with
      | exception Sys_error _ -> acc
      | names ->
          Array.fold_left
            (fun acc name ->
              if name = "" || name.[0] = '.' then acc
              else
                let path = Filename.concat directory name in
                if is_directory path then walk (depth + 1) path acc
                else if with_extension ".jazz" name then path :: acc
                else acc)
            acc names
  in
  List.sort String.compare (walk 0 directory [])

let is_entry ~entries file =
  match entries with
  | [] -> with_extension ".jazz" file
  | entries -> List.exists (fun entry -> equal entry file) entries

(* The first entry, in the order the project lists them, that requires [file]
   directly or through other files. The walk is depth first in the order a
   file writes its requires, which is the order the compiler enters them. *)
let of_file ~entries ~idirs ~read file =
  let file = normalise file in
  if is_entry ~entries file then alone file
  else
    let rec walk ~seen path =
      if List.mem path seen then None
      else
        let seen = path :: seen in
        let rec each = function
          | [] -> None
          | required :: rest -> (
              if equal required file then Some [ path; file ]
              else
                match walk ~seen required with
                | Some chain -> Some (path :: chain)
                | None -> each rest)
        in
        each (required_files ~idirs ~read path)
    in
    let rec from_entries = function
      | [] -> alone file
      | entry :: rest -> (
          match walk ~seen:[] entry with
          | Some chain -> { file; entry = Some entry; chain }
          | None -> from_entries rest)
    in
    from_entries entries

let describe t =
  match t.entry with
  | None -> "checked alone"
  | Some entry -> (
      (* The files between the entry and this one, which is what a reader
         needs to see why the context applies. *)
      let through =
        match List.rev t.chain with
        | _file :: rest -> (
            match List.rev rest with _entry :: through -> through | [] -> [])
        | [] -> []
      in
      match through with
      | [] -> Printf.sprintf "checked in the context of %s" entry
      | through ->
          Printf.sprintf "checked in the context of %s, required through %s"
            entry
            (String.concat ", " through))
