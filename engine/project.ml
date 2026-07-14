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

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> (
      match parse ~path text with
      | loaded -> { loaded with path = Some path }
      | exception exn ->
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
          })
  | exception Sys_error _ ->
      (* No file is the ordinary case: the defaults apply and nothing is
         wrong. *)
      { path = None; text = ""; bom = 0; project = empty; problems = [] }

let diagnostics ~uri (loaded : loaded) =
  List.map
    (fun (problem : problem) ->
      Jide_lang.Diagnostic.make ~code:"project"
        ~severity:Jide_lang.Diagnostic.Error problem.span problem.message)
    loaded.problems
