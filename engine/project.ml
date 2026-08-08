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

(* A path relative to the project file's directory, as an absolute one. *)
let resolve ~directory path =
  if Filename.is_relative path then Filename.concat directory path else path

let parse_members ~directory ~bom ranges members =
  let problems = ref [] in
  let project = ref empty in
  let shift span = if bom = 0 then span else Jide_lang.Span.shift span bom in
  let complain ~span message =
    problems := { span = shift span; message } :: !problems
  in
  let spans_of name =
    match List.assoc_opt name ranges with
    | Some (key, value) -> (key, value)
    | None -> (blank_span, blank_span)
  in
  let key_range name = fst (spans_of name) in
  let value_range name = snd (spans_of name) in
  (* A version other than 1 is another schema. The fields it holds are not
     read at all, because none of them would mean what this version means. *)
  let version_problem =
    match List.assoc_opt "version" members with
    | None -> None
    | Some (`Int 1) -> None
    | Some (`Intlit text) when String.trim text = "1" -> None
    | Some (`Int number) ->
        Some
          (Printf.sprintf
             "this project file is version %d; only version 1 is read" number)
    | Some (`Intlit text) ->
        Some
          (Printf.sprintf
             "this project file is version %s; only version 1 is read" text)
    | Some _ ->
        Some "the version field must be the number 1; the defaults are used"
  in
  match version_problem with
  | Some message ->
      complain ~span:(value_range "version") message;
      (empty, List.rev !problems)
  | None ->
      List.iter
        (fun (name, value) ->
          match name with
          | "version" -> ()
          | "arch" -> (
              match value with
              | `String name -> (
                  match Jide_jasmin.Adapter.arch_of_string name with
                  | Some arch -> project := { !project with arch = Some arch }
                  | None ->
                      complain ~span:(value_range "arch")
                        (Printf.sprintf "unknown architecture: %s (known: %s)"
                           name (quoted arch_names)))
              | _ ->
                  complain ~span:(value_range "arch")
                    "the arch field must be a string; the default is used")
          | "call_conv" -> (
              match value with
              | `String name -> (
                  match Jide_jasmin.Adapter.call_conv_of_string name with
                  | Some call_conv ->
                      project := { !project with call_conv = Some call_conv }
                  | None ->
                      complain ~span:(value_range "call_conv")
                        (Printf.sprintf
                           "unknown calling convention: %s (known: %s)" name
                           (quoted call_conv_names)))
              | _ ->
                  complain ~span:(value_range "call_conv")
                    "the call_conv field must be a string; the default is used")
          | "include" -> (
              match value with
              | `Assoc pairs ->
                  let paths =
                    List.filter_map
                      (fun (key, value) ->
                        match value with
                        | `String path -> Some (key, resolve ~directory path)
                        | _ ->
                            complain ~span:(value_range "include")
                              (Printf.sprintf
                                 "the include path %s must be a string" key);
                            None)
                      pairs
                  in
                  project := { !project with idirs = paths }
              | _ ->
                  complain ~span:(value_range "include")
                    "the include field must be an object of name to path")
          | "entries" -> (
              match value with
              | `List values ->
                  let entries =
                    List.filter_map
                      (fun value ->
                        match value with
                        | `String path -> Some (resolve ~directory path)
                        | _ ->
                            complain ~span:(value_range "entries")
                              "every entry must be a string";
                            None)
                      values
                  in
                  project := { !project with entries }
              | _ ->
                  complain ~span:(value_range "entries")
                    "the entries field must be a list of paths")
          | unknown ->
              complain ~span:(key_range unknown)
                (Printf.sprintf "unknown field: %s" unknown))
        members;
      (!project, List.rev !problems)

let parse ?path text =
  let bom, body = strip_bom text in
  let broken message =
    {
      path;
      text = body;
      bom;
      project = empty;
      problems = [ { span = blank_span; message } ];
    }
  in
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error message -> broken ("not valid JSON: " ^ message)
  | exception exn -> broken ("cannot be read: " ^ Printexc.to_string exn)
  | `Assoc members ->
      let directory =
        match path with
        | Some path -> Filename.dirname path
        | None -> Filename.current_dir_name
      in
      let ranges = member_ranges body in
      let project, problems = parse_members ~directory ~bom ranges members in
      { path; text = body; bom; project; problems }
  | _ -> broken "the project file must hold one JSON object"

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

(* A file that is not there is the ordinary case: the defaults apply and
   nothing is wrong. [In_channel.with_open_bin] raises [Sys_error] for that
   case and for every other read failure, so the two are told apart before
   the read, with [Sys.file_exists]. A file removed between the two calls is
   reported as unreadable, which is the honest answer: it was there when the
   project was looked for. *)
let load path =
  if not (Sys.file_exists path) then
    { path = None; text = ""; bom = 0; project = empty; problems = [] }
  else
    match In_channel.with_open_bin path In_channel.input_all with
    | text -> (
        match parse ~path text with
        | loaded -> { loaded with path = Some path }
        | exception exn -> unreadable path exn)
    | exception exn -> unreadable path exn

let diagnostics ~uri (loaded : loaded) =
  List.map
    (fun (problem : problem) ->
      Jide_lang.Diagnostic.make ~code:"project"
        ~severity:Jide_lang.Diagnostic.Error problem.span problem.message)
    loaded.problems
