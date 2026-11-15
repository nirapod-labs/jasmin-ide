(** The [check] mode of the server binary. *)

open Jide_lang

let read path = In_channel.with_open_bin path In_channel.input_all

(* The same read, as the lookup a [require] and the context rule take: a file
   that cannot be read is [None], never a raise. *)
let read_opt path =
  match read path with text -> Some text | exception Sys_error _ -> None

(* The files the compiler would enter before [file] when the file is not an
   entry, outermost first: the first .jazz file that requires it, directly or
   through other files, and the files in between. The entries are the .jazz
   files under the project root, which is the directory of the nearest
   jasmin.project at or above the file. With no project file above it the
   search stays in the file's own directory, because there is no root to
   bound a walk up the tree. A .jazz file is an entry by default, so it is
   checked on its own and nothing is searched; a file no entry requires is
   alone, which is the empty list. *)
let context_of ~(idirs : (string * string) list) (file : string) : string list =
  (* The walk reads a [from NAME require] through the same bindings the check
     uses, [JASMINPATH]'s included. *)
  let idirs = Jide_jasmin.Adapter.with_jasmin_path idirs in
  if Filename.check_suffix file ".jazz" then []
  else
    let project =
      Option.map Filename.dirname
        (Jide_engine.Project.find_above
           (Filename.dirname (Jide_engine.Context.absolute file)))
    in
    let directory =
      match Jide_engine.Context.search_root ?project file with
      | Some directory -> directory
      | None -> Filename.dirname file
    in
    let entries = Jide_engine.Context.entries_of_directory directory in
    let context =
      Jide_engine.Context.of_file ~entries ~idirs ~read:read_opt file
    in
    match List.rev (Jide_engine.Context.chain context) with
    | _document :: context -> List.rev context
    | [] -> []

(* The lines of the files a [require] reached, as the compiler prints them. *)
let required_lines (entries : Jide_jasmin.Adapter.required list) =
  List.concat_map
    (fun (required : Jide_jasmin.Adapter.required) ->
      Report.lines ~file:required.file ~text:required.text required.diagnostics)
    entries

(* The order the compiler meets the errors in. It parses the whole document
   before it types anything, so a syntax error in the document comes first.
   Otherwise it types the items in order and reads a required file when it
   reaches the [require], so a required file whose [require] comes at or
   before the document's first error is met before that error, and one after
   it is met after; a file of the context chain is entered before the
   document. Errors are printed in that order, so the first line is the
   compiler's first error. *)
let ordered ~text ~(own : string list) (diagnostics : Diagnostic.t list)
    (required : Jide_jasmin.Adapter.required list) =
  let errors =
    List.filter
      (fun (diagnostic : Diagnostic.t) ->
        diagnostic.severity = Diagnostic.Error)
      diagnostics
  in
  let syntax =
    List.exists
      (fun (diagnostic : Diagnostic.t) -> diagnostic.code = Some "parse")
      errors
  in
  let first =
    List.fold_left
      (fun low (diagnostic : Diagnostic.t) ->
        min low diagnostic.span.Jide_lang.Span.start_byte)
      (String.length text + 1)
      errors
  in
  let leads (entry : Jide_jasmin.Adapter.required) =
    (not syntax)
    && match entry.site with None -> true | Some site -> site <= first
  in
  let leading, trailing = List.partition leads required in
  required_lines leading @ own @ required_lines trailing

let check_file ~out (request : Request.t) file =
  let text, diagnostics, required =
    match read file with
    | text ->
        let result =
          Jide_jasmin.Adapter.check
            ~context:(context_of ~idirs:request.idirs file)
            request.arch request.call_conv ~file ~text ~idirs:request.idirs
        in
        (text, Report.of_check ~file ~text result, result.required)
    | exception Sys_error message ->
        ("", [ Report.internal ("cannot read the file: " ^ message) ], [])
  in
  let own = Report.lines ~file ~text diagnostics in
  List.iter (fun line -> out line) (ordered ~text ~own diagnostics required);
  out (Report.summary ~file diagnostics);
  required <> []
  || List.exists
       (fun (diagnostic : Diagnostic.t) ->
         diagnostic.severity = Diagnostic.Error)
       diagnostics

let run ~out ~err arguments =
  match Request.parse arguments with
  | Request.Help ->
      out Request.usage;
      0
  | Request.Invalid message ->
      err ("jasmin-lsp check: " ^ message);
      err Request.usage;
      2
  | Request.Run request ->
      let failed =
        List.fold_left
          (fun failed file -> check_file ~out request file || failed)
          false request.files
      in
      if failed then 1 else 0

let main arguments =
  let out line = print_endline line in
  let err line = prerr_endline line in
  try run ~out ~err arguments
  with exn ->
    prerr_endline ("[jasmin-lsp] check: " ^ Printexc.to_string exn);
    1
