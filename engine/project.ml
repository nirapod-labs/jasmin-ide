(* The project file, [jasmin.project]: an INI file with one optional
   [[jasmin]] section, one [key = value] per line, whole-line comments and
   repeated keys that accumulate in file order. It is found by walking up from
   the file being analysed, and every problem in it is reported at the line it
   is on rather than only in a log. *)

type t = {
  arch : Jide_jasmin.Adapter.arch option;
  call_conv : Jide_jasmin.Adapter.call_conv option;
  idirs : (string * string) list;  (** in the order the file gives them *)
  entries : string list;
}

let empty = { arch = None; call_conv = None; idirs = []; entries = [] }
let file_name = "jasmin.project"

type problem = { span : Jide_lang.Span.t; message : string }

type loaded = {
  path : string option;
  text : string;
  bom : int;
  project : t;
  problems : problem list;
}

(* No file: no path, no text, no setting and nothing wrong. *)
let absent = { path = None; text = ""; bom = 0; project = empty; problems = [] }
let blank_span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0

(* The same rule as the engine's: a byte-order mark is not part of the file's
   own text and every byte offset moves back by it. *)
let strip_bom text =
  let length = String.length text in
  if
    length >= 3
    && Char.code text.[0] = 0xEF
    && Char.code text.[1] = 0xBB
    && Char.code text.[2] = 0xBF
  then (3, String.sub text 3 (length - 3))
  else (0, text)

(* A blank within a line: a space, a tab, or a carriage return that no line
   terminator took. *)
let is_blank character = character = ' ' || character = '\t' || character = '\r'

(* The part of a line that is not blank at either end, as the offsets left.
   Every span below is taken from the result, so a problem covers the text it
   is about and never the whitespace round it. *)
let trim text start stop =
  let start = ref start and stop = ref stop in
  while !start < !stop && is_blank text.[!start] do
    incr start
  done;
  while !stop > !start && is_blank text.[!stop - 1] do
    decr stop
  done;
  (!start, !stop)

(* The content of every line as [start, stop) byte offsets: the line feed
   excluded, and a carriage return before it too. A text that ends in a line
   feed has a final empty line, which is blank and so passed over. *)
let line_bounds text =
  let length = String.length text in
  let rec loop index acc =
    if index > length then List.rev acc
    else
      match String.index_from_opt text index '\n' with
      | None -> List.rev ((index, length) :: acc)
      | Some stop ->
          let content_end =
            if stop > index && text.[stop - 1] = '\r' then stop - 1 else stop
          in
          loop (stop + 1) ((index, content_end) :: acc)
  in
  loop 0 []

(* The first [jasmin.project] in this directory or in the nearest directory
   above it. The filesystem root stops the walk, and it does so by itself:
   [Filename.dirname "/"] is "/". *)
let rec find_above directory =
  let candidate = Filename.concat directory file_name in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then None else find_above parent

(* The names of the values the file may hold. Their order is the order the
   message that refuses a bad one lists the accepted values in. *)
let arch_names = Jide_jasmin.Adapter.arch_names
let call_conv_names = [ "linux"; "windows" ]
let quoted names = String.concat ", " names

(* A path relative to the project file's directory, as an absolute one. Without
   a named file there is no directory to resolve against, and the interface
   says a relative path is then left as written rather than guessed against the
   process's working directory. *)
let resolve ~directory path =
  match directory with
  | None -> path
  | Some directory ->
      if Filename.is_relative path then Filename.concat directory path else path

(* One problem for a file that is there and could not be read: the text is
   empty, the defaults apply and the reader is told, which is what the
   interface promises. *)
let unreadable path exn =
  {
    path = Some path;
    text = "";
    bom = 0;
    project = empty;
    problems =
      [
        {
          span = blank_span;
          message = "cannot be read: " ^ Printexc.to_string exn;
        };
      ];
  }

(* One [key = value] line applied to [project]. [complain] takes the bytes the
   problem is at and the message; a key this version cannot use leaves the
   project as it was and complains, and the rest of the file is still read. *)
let apply_key ~directory ~on_disk ~complain ~set_once ~line ~key ~key_span
    ~value ~value_span project =
  (* A path an [include] names has to be a directory and an entry has to be
     there. Both questions are asked only when the text came from a named
     file: a relative path means nothing without the directory that holds it,
     and guessing one would resolve against the process's working directory,
     which has nothing to do with the project. *)
  let exists path = Sys.file_exists path in
  match key with
  | "arch" -> (
      if not (set_once ~key ~line key_span) then project
      else
        match Jide_jasmin.Adapter.arch_of_string value with
        | Some arch -> { project with arch = Some arch }
        | None ->
            complain value_span
              (Printf.sprintf "unknown architecture: %s (known: %s)" value
                 (quoted arch_names));
            project)
  | "call_conv" -> (
      if not (set_once ~key ~line key_span) then project
      else
        match Jide_jasmin.Adapter.call_conv_of_string value with
        | Some call_conv -> { project with call_conv = Some call_conv }
        | None ->
            complain value_span
              (Printf.sprintf "unknown calling convention: %s (known: %s)" value
                 (quoted call_conv_names));
            project)
  | "include" -> (
      (* The value holds a second [=]: the first one on the line separates the
         key from the value, and the first one of the value separates the name
         from the path. A value with no second [=] is one mistake, reported
         once, and with the message for a malformed line: a value that has to
         hold a [key = value] pair of its own and does not is missing the very
         thing that message names. An [include] accumulates, so writing it
         twice binds two names in the order the file gives them. *)
      match String.index_opt value '=' with
      | None ->
          complain value_span "expected key = value";
          project
      | Some index ->
          let name_start, name_stop = trim value 0 index in
          let path_start, path_stop =
            trim value (index + 1) (String.length value)
          in
          if name_start >= name_stop || path_start >= path_stop then (
            complain value_span "expected key = value";
            project)
          else
            let name = String.sub value name_start (name_stop - name_start) in
            let path = String.sub value path_start (path_stop - path_start) in
            let resolved = resolve ~directory path in
            if on_disk && not (exists resolved && Sys.is_directory resolved)
            then (
              complain value_span
                (Printf.sprintf "include %s is not a directory" name);
              project)
            else { project with idirs = project.idirs @ [ (name, resolved) ] })
  | "entry" ->
      if not (set_once ~key ~line key_span) then project
      else
        let resolved = resolve ~directory value in
        if on_disk && not (exists resolved) then (
          complain value_span "the entry file does not exist";
          project)
        else { project with entries = project.entries @ [ resolved ] }
  | unknown ->
      complain key_span (Printf.sprintf "unknown key: %s" unknown);
      project

let parse ?path text =
  let bom, body = strip_bom text in
  let problems = ref [] in
  (* Every span is a byte range of the file, so the bytes a byte-order mark
     took from the front are put back for the reader. *)
  let complain span message =
    problems := { span = Jide_lang.Span.shift span bom; message } :: !problems
  in
  let span_of start stop =
    Jide_lang.Span.make ~start_byte:start ~end_byte:stop
  in
  let directory =
    match path with Some path -> Some (Filename.dirname path) | None -> None
  in
  let on_disk = Option.is_some path in
  let project = ref empty in
  (* The keys before the first header belong to [jasmin], because that section
     is optional. A header this version does not read stops the reading until
     the next header, so an unknown section is one problem and not one per key
     inside it. *)
  let reading = ref true in
  (* The single-valued keys already given, with the line each was first on.
     Only [include] accumulates: a key written twice would silently lose one
     of its two values, which is the failure mode unknown keys are refused
     for. *)
  let given = ref [] in
  let set_once ~key ~line span =
    match List.assoc_opt key !given with
    | Some first ->
        complain span
          (Printf.sprintf "%s is given twice (line %d, then line %d)" key first
             line);
        false
    | None ->
        given := (key, line) :: !given;
        true
  in
  List.iteri
    (fun index bounds ->
      let line = index + 1 in
      let start, stop = trim body (fst bounds) (snd bounds) in
      if start < stop then
        let line_span = span_of start stop in
        match body.[start] with
        (* A comment is a whole line: a line whose first non-blank character
           is [#] or [;]. A [#] inside a value is part of the value, which is
           what keeps a path holding one usable, so there is no trailing
           comment. *)
        | '#' | ';' -> ()
        | '[' ->
            if body.[stop - 1] <> ']' then (
              complain line_span "the section name is not closed with ]";
              reading := false)
            else
              let name_start, name_stop = trim body (start + 1) (stop - 1) in
              if name_start >= name_stop then (
                complain line_span "a section header needs a name";
                reading := false)
              else
                let name =
                  String.sub body name_start (name_stop - name_start)
                in
                if String.equal name "jasmin" then reading := true
                else (
                  complain
                    (span_of name_start name_stop)
                    (Printf.sprintf
                       "unknown section: %s (only [jasmin] is read)" name);
                  reading := false)
        | _ -> (
            let equals =
              match String.index_from_opt body start '=' with
              | Some index when index < stop -> Some index
              | _ -> None
            in
            match equals with
            | None -> complain line_span "expected key = value"
            | Some index ->
                let key_start, key_stop = trim body start index in
                let value_start, value_stop = trim body (index + 1) stop in
                if key_start >= key_stop then
                  complain line_span "expected key = value"
                else if not !reading then ()
                else
                  let key = String.sub body key_start (key_stop - key_start) in
                  let key_span = span_of key_start key_stop in
                  if value_start >= value_stop then
                    complain line_span (Printf.sprintf "%s needs a value" key)
                  else
                    let value =
                      String.sub body value_start (value_stop - value_start)
                    in
                    project :=
                      apply_key ~directory ~on_disk ~complain ~set_once ~line
                        ~key ~key_span ~value
                        ~value_span:(span_of value_start value_stop)
                        !project))
    (line_bounds body);
  { path; text = body; bom; project = !project; problems = List.rev !problems }

(* A file that is not there is the ordinary case: the defaults apply and
   nothing is wrong. [In_channel.with_open_bin] raises [Sys_error] for that
   case and for every other read failure, so the two are told apart before the
   read, with [Sys.file_exists]. A file removed between the two calls is
   reported as unreadable, which is the honest answer: it was there when the
   project was looked for. *)
let load path =
  if not (Sys.file_exists path) then absent
  else
    match In_channel.with_open_bin path In_channel.input_all with
    | text -> (
        match parse ~path text with
        | loaded -> loaded
        | exception exn -> unreadable path exn)
    | exception exn -> unreadable path exn

(* The project a directory is analysed under: the first file at it or above
   it. A directory with no such file above it is the ordinary case: the
   defaults apply and nothing is reported. *)
let load_above directory =
  match find_above directory with Some path -> load path | None -> absent

let diagnostics ~uri (loaded : loaded) =
  List.map
    (fun (problem : problem) ->
      Jide_lang.Diagnostic.make ~code:"project"
        ~severity:Jide_lang.Diagnostic.Error problem.span problem.message)
    loaded.problems
