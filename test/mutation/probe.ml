(** One check of one text through the adapter, and the outline it parsed.

    This is the measurement the criteria are stated on: the error count of
    the check, and the items the parse returned. Nothing here is caught: a
    caller that wants an exception counted, rather than an exception, runs
    this in a child process (see {!Isolate}). *)

type result = {
  errors : int;  (** Diagnostics with severity [Error]. *)
  warnings : int;
  outline : Outline.entry list;
}

let run ~arch ~call_conv ~file text =
  let checked =
    Jide_jasmin.Adapter.check arch call_conv ~file ~text ~idirs:[]
  in
  let errors =
    List.fold_left
      (fun total (diagnostic : Jide_lang.Diagnostic.t) ->
        match diagnostic.severity with
        | Jide_lang.Diagnostic.Error -> total + 1
        | _ -> total)
      0 checked.diagnostics
  in
  let warnings = List.length checked.warnings in
  let parsed = Jide_jasmin.Adapter.parse ~name:file text in
  { errors; warnings; outline = Outline.of_items parsed.items }
