(** The [check] mode of the server binary. *)

open Jide_lang

let read path = In_channel.with_open_bin path In_channel.input_all

let check_file ~out (request : Request.t) file =
  let text, diagnostics =
    match read file with
    | text ->
        let result =
          Jide_jasmin.Adapter.check request.arch request.call_conv ~file ~text
            ~idirs:request.idirs
        in
        (text, Report.of_check ~file ~text result)
    | exception Sys_error message ->
        ("", [ Report.internal ("cannot read the file: " ^ message) ])
  in
  List.iter (fun line -> out line) (Report.lines ~file ~text diagnostics);
  out (Report.summary ~file diagnostics);
  List.exists
    (fun (diagnostic : Diagnostic.t) -> diagnostic.severity = Diagnostic.Error)
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
