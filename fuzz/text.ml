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

