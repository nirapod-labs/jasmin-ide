(** The text surface: a source text in, the checker's answer out.

    Three properties, each a way the server could break:

    - the call returns: the library catches its own failures, so anything
      that reaches this process as an exception is a finding;
    - the same text is checked the same way twice: the compiler's state is
      not carried from one document to the next;
    - every span lies inside the text it came from, and the engine's own
      spans (warnings, the byte-order mark shift, the size refusal) lie
      inside the document the client sent.

    A case reports the first property that does not hold, in one line, and
    reports nothing when all three do. *)

let file = "fuzz.jazz"
let arch = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let call_conv = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

(** The document has no path, so nothing here reads the disk: a corpus unit
    that says [require "x.jinc"] is a text, not a tree. *)
let uri = "untitled:fuzz/document.jazz"

let span_text (span : Jide_lang.Span.t) =
  Printf.sprintf "%d..%d" span.Jide_lang.Span.start_byte
    span.Jide_lang.Span.end_byte

let check_ordering where (diagnostics : Jide_lang.Diagnostic.t list) =
  let rec walk = function
    | [] -> ""
    | (diagnostic : Jide_lang.Diagnostic.t) :: rest ->
        let span = diagnostic.Jide_lang.Diagnostic.span in
        if span.Jide_lang.Span.start_byte < 0 then
          Printf.sprintf "%s: a span starts at %d, before the text" where
            span.Jide_lang.Span.start_byte
        else if span.Jide_lang.Span.end_byte < span.Jide_lang.Span.start_byte
        then
          Printf.sprintf "%s: the span %s ends before it starts" where
            (span_text span)
        else walk rest
  in
  walk diagnostics

let check_inside_engine (diagnostics : Jide_lang.Diagnostic.t list)
    (length : int) =
  let rec walk = function
    | [] -> ""
    | (diagnostic : Jide_lang.Diagnostic.t) :: rest ->
        let span = diagnostic.Jide_lang.Diagnostic.span in
        if
          span.Jide_lang.Span.start_byte < 0
          || span.Jide_lang.Span.end_byte > length
          || span.Jide_lang.Span.start_byte > span.Jide_lang.Span.end_byte
        then
          Printf.sprintf
            "the engine reported the span %s in a text of %d bytes: %s"
            (span_text span) length diagnostic.Jide_lang.Diagnostic.message
        else walk rest
  in
  walk diagnostics

(** The engine's path over the same text: the store, the analysis key, the
    byte-order mark and the warnings the compiler printed with line and
    column numbers rather than offsets. *)
let check_engine (text : string) =
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.open_document engine ~uri ~version:1 ~text;
  match Jide_engine.Engine.check engine ~uri with
  | None -> "the engine forgot the document it was given"
  | Some analysis -> (
      let diagnostics = analysis.Jide_engine.Engine.diagnostics in
      let length = String.length text in
      match check_ordering "the engine" diagnostics with
      | "" -> check_inside_engine diagnostics length
      | report -> report)

let same_diagnostics (first : Jide_lang.Diagnostic.t list)
    (second : Jide_lang.Diagnostic.t list) =
  let key (diagnostic : Jide_lang.Diagnostic.t) =
    ( diagnostic.Jide_lang.Diagnostic.span,
      diagnostic.Jide_lang.Diagnostic.severity,
      diagnostic.Jide_lang.Diagnostic.message )
  in
  List.length first = List.length second
  && List.for_all2 (fun a b -> key a = key b) first second

let case ?(corpus = [||]) (st : Gen.state) : string =
  let text = Gen.source ~corpus st in
  match Jide_jasmin.Adapter.check arch call_conv ~file ~text ~idirs:[] with
  | exception exn ->
      Printf.sprintf "the adapter raised %s on %d bytes"
        (Printexc.to_string exn) (String.length text)
  | result -> (
      let diagnostics = result.Jide_jasmin.Adapter.diagnostics in
      match check_ordering "the adapter" diagnostics with
      | "" -> (
          match
            Jide_jasmin.Adapter.check arch call_conv ~file ~text ~idirs:[]
          with
          | exception exn ->
              Printf.sprintf "the second check raised %s"
                (Printexc.to_string exn)
          | again ->
              let second = again.Jide_jasmin.Adapter.diagnostics in
              if not (same_diagnostics diagnostics second) then
                Printf.sprintf
                  "one text was checked twice and the answers differ: %d then \
                   %d diagnostics"
                  (List.length diagnostics) (List.length second)
              else check_engine text)
      | report -> report)
