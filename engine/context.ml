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

type require = { from : string option; written : string }

let alone file = { file; entry = None; chain = [] }
let file t = t.file
let entry t = t.entry
let chain t = t.chain
let is_alone t = t.entry = None

(* One spelling per file: an absolute path with no [.] and no [..] left in
   it, or a relative path with no [.] and only the [..] that climb out of its
   start. Nothing here asks the filesystem, so a symbolic link is not resolved
   and two paths that name one file through different links stay different. *)
let normalise path =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let rec fold stack = function
    | [] -> List.rev stack
    | ("" | ".") :: rest -> fold stack rest
    | ".." :: rest -> (
        match stack with
        | [] -> fold (if absolute then [] else [ ".." ]) rest
        | ".." :: _ when not absolute -> fold (".." :: stack) rest
        | _ :: stack -> fold stack rest)
    | part :: rest -> fold (part :: stack) rest
  in
  let body = String.concat "/" (fold [] parts) in
  if absolute then "/" ^ body else if body = "" then "." else body

(* The absolute spelling of a path: a relative one is taken from the working
   directory, and the result is normalised. *)
let absolute path =
  normalise
    (if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
     else path)

(* Two paths name one file when their absolute spellings are equal, so a
   relative path and an absolute one to the same file compare equal. *)
let equal left right = String.equal (absolute left) (absolute right)

let is_letter character =
  (character >= 'a' && character <= 'z')
  || (character >= 'A' && character <= 'Z')

let is_identifier character =
  is_letter character
  || (character >= '0' && character <= '9')
  || character = '_' || character = '\'' || character = '.'

(* The [require]s a text holds, in the order it holds them. A bare
   [require "p"] names [p] beside the file that writes it. A [from NAME
   require "p"] names [p] beside the directory the include path [NAME] is
   bound to; that is the form the compiler reads, and the one the checker and
   the navigation resolver read too. The scan skips comments, so a
   commented-out require is not one, and reads a run of string literals after
   the keyword, which the grammar allows. A string outside a require is
   skipped rather than read, so an [inline] block that happens to hold the
   word is not followed: the scan is lexical and knows nothing about
   nesting. *)
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
  (* The string literals that follow a [require], in the order they come. *)
  let rec literals index acc =
    let index = blanks index in
    if index < length && text.[index] = '"' then
      let stop = string_end (index + 1) in
      let last = min (stop - 1) length in
      let literal = String.sub text (index + 1) (max 0 (last - index - 1)) in
      literals stop (literal :: acc)
    else (index, List.rev acc)
  and blanks index =
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
  (* What the words just before a [require] say about its [from]: the keyword
     [from] and the name after it, with only blanks and comments between. *)
  let rec scan index acc state =
    if index >= length then List.rev acc
    else
      match text.[index] with
      | ' ' | '\t' | '\r' | '\n' -> scan (index + 1) acc state
      | '/' when index + 1 < length && text.[index + 1] = '/' ->
          scan (line_end (index + 2)) acc state
      | '/' when index + 1 < length && text.[index + 1] = '*' ->
          scan (comment_end (index + 2)) acc state
      | '"' -> scan (string_end (index + 1)) acc `Plain
      | character when is_letter character ->
          let rec read stop =
            if stop < length && is_identifier text.[stop] then read (stop + 1)
            else stop
          in
          let stop = read (index + 1) in
          let word = String.sub text index (stop - index) in
          if String.equal word "require" then
            let from =
              match state with `Named name -> Some name | _ -> None
            in
            let next, found = literals stop [] in
            let found = List.map (fun written -> { from; written }) found in
            scan next (List.rev_append found acc) `Plain
          else if String.equal word "from" then scan stop acc `From
          else
            let state = match state with `From -> `Named word | _ -> `Plain in
            scan stop acc state
      | _ -> scan (index + 1) acc `Plain
  in
  scan 0 [] `Plain

(* One requirement as a path, the way the compiler enters it. [None] is a name
   the project does not bind, so the file cannot be found through it. An
   absolute path is taken as it is. *)
let resolve ~idirs ~file { from; written } =
  let base =
    match from with
    | None -> Some (Filename.dirname file)
    | Some name -> List.assoc_opt name idirs
  in
  Option.map
    (fun base ->
      let path =
        if Filename.is_relative written then Filename.concat base written
        else written
      in
      normalise path)
    base

let required_files ~idirs ~read file =
  match read file with
  | None -> []
  | Some text ->
      List.filter_map
        (fun require -> resolve ~idirs ~file require)
        (requires_of_text text)

let with_extension extension file = Filename.check_suffix file extension

let is_directory path =
  match Sys.is_directory path with ok -> ok | exception Sys_error _ -> false

(* Every [.jazz] file under a directory, sorted, which is the entry set of a
   project that names none. Hidden directories are not searched, the walk
   stops at a depth of eight and a directory it cannot read is skipped. A
   depth of eight is more than a Jasmin tree uses and keeps a symbolic link
   loop from running forever. *)
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

(* [path], absolute and normalised, is [root] or lies inside it. *)
let within ~root path =
  String.equal root path
  ||
  let prefix = if String.equal root "/" then "/" else root ^ "/" in
  let length = String.length prefix in
  String.length path >= length && String.equal (String.sub path 0 length) prefix

(* [path] written from [base], both absolute and normalised, with a [..] for
   each step the walk takes out of [base]. *)
let relative_to ~base path =
  let parts text =
    List.filter (fun part -> part <> "") (String.split_on_char '/' text)
  in
  let rec strip left right =
    match (left, right) with
    | x :: left, y :: right when String.equal x y -> strip left right
    | _ -> (left, right)
  in
  let up, down = strip (parts base) (parts path) in
  match List.map (fun _ -> "..") up @ down with
  | [] -> "."
  | parts -> String.concat "/" parts

let climbs_out path =
  String.equal path ".."
  || (String.length path >= 3 && String.equal (String.sub path 0 3) "../")

(* The directory the entries of a file's context are searched under. The
   project root is the directory of the nearest project file above the file,
   and the workspace root is the directory the client opened. The project root
   wins when it holds the file, because the project is the unit an entry
   belongs to, and the workspace root is the fallback. The search never leaves
   the root it names, so a file that neither holds has no root and the caller
   decides what to search instead. A relative [file] gets a root written
   relative to the working directory, unless that root lies above it, in which
   case it is written absolutely, so no relative path climbs out of the
   working directory. *)
let search_root ?workspace ?project file =
  let target = absolute file in
  let holds directory = within ~root:(absolute directory) target in
  let chosen =
    match project with
    | Some directory when holds directory -> Some directory
    | _ -> (
        match workspace with
        | Some directory when holds directory -> Some directory
        | _ -> None)
  in
  Option.map
    (fun directory ->
      let directory = absolute directory in
      if Filename.is_relative file then
        let written =
          relative_to ~base:(absolute Filename.current_dir_name) directory
        in
        if climbs_out written then directory else written
      else directory)
    chosen

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
      if List.exists (fun seen_path -> equal seen_path path) seen then None
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
