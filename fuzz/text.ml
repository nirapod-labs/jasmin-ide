(** The text surface: a source text in, the checker's answer out.

    Three properties, each a way the server could break:

    - the call returns: the library catches its own failures, so anything
      that reaches this process as an exception is a finding;
    - the same text is checked the same way twice: the compiler's state is
      not carried from one document to the next;
    - every span lies inside the text it came from, and the engine's own
      spans (warnings, the byte-order mark shift, the size refusal) lie
      inside the document the client sent;
    - a rename the server makes is undone by the rename back: at a few names
      of the text, prepareRename and rename are asked as the server asks them
      ({!Jide_controller.Rename_features}), and every rename that is made has
      edits inside the text, in order and apart, and renaming the declaration
      back to its old name restores the text byte for byte. A refusal is an
      answer, not a finding.

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

(* A name that is a part of no word of the text, so a rename to it cannot meet
   another declaration, and a rename back is always possible. *)
let fresh_name (text : string) =
  let contains part =
    let lt = String.length text and lp = String.length part in
    let rec at i =
      i + lp <= lt && (String.sub text i lp = part || at (i + 1))
    in
    at 0
  in
  let rec attempt n =
    let candidate = Printf.sprintf "fz_rn%d" n in
    if contains candidate then attempt (n + 1) else candidate
  in
  attempt 0

let apply_edits (text : string) (edits : Jide_engine.Types.text_edit list) =
  List.fold_left
    (fun text (edit : Jide_engine.Types.text_edit) ->
      let span = edit.Jide_engine.Types.span in
      String.sub text 0 span.Jide_lang.Span.start_byte
      ^ edit.Jide_engine.Types.new_text
      ^ String.sub text span.Jide_lang.Span.end_byte
          (String.length text - span.Jide_lang.Span.end_byte))
    text (List.rev edits)

(* The edits of the document, checked to be inside the text, in order and
   apart, or the first way they are not. *)
let document_edits (text : string) (edits : Jide_controller.Features.edit list)
    =
  let length = String.length text in
  match edits with
  | [ edit ]
    when String.equal
           edit.Jide_controller.Features.file.Jide_controller.Features.uri uri
    ->
      let rec ordered previous = function
        | [] -> Ok edit.Jide_controller.Features.edits
        | (one : Jide_engine.Types.text_edit) :: rest ->
            let span = one.Jide_engine.Types.span in
            if
              span.Jide_lang.Span.start_byte < previous
              || span.Jide_lang.Span.end_byte > length
              || span.Jide_lang.Span.end_byte < span.Jide_lang.Span.start_byte
            then
              Error
                (Printf.sprintf
                   "a rename edit %s is outside a text of %d bytes or out of \
                    order"
                   (span_text span) length)
            else ordered span.Jide_lang.Span.end_byte rest
      in
      ordered 0 edit.Jide_controller.Features.edits
  | _ -> Error "a rename of a document that reads no file edited another file"

(* Where [byte] lands once the edits are made. *)
let moved (edits : Jide_engine.Types.text_edit list) byte =
  List.fold_left
    (fun after (edit : Jide_engine.Types.text_edit) ->
      let span = edit.Jide_engine.Types.span in
      if span.Jide_lang.Span.end_byte <= byte then
        after
        + String.length edit.Jide_engine.Types.new_text
        - (span.Jide_lang.Span.end_byte - span.Jide_lang.Span.start_byte)
      else after)
    byte edits

(* One rename and its inverse, at [byte], through the server's own answers. *)
let rename_round_trip engine (text : string) ~byte =
  match Jide_controller.Rename_features.prepare engine ~uri ~byte with
  | None | Some (Error _) -> ""
  | Some (Ok span) -> (
      let old_name =
        String.sub text span.Jide_lang.Span.start_byte
          (span.Jide_lang.Span.end_byte - span.Jide_lang.Span.start_byte)
      in
      let new_name = fresh_name text in
      match
        Jide_controller.Rename_features.rename engine ~uri
          ~byte:span.Jide_lang.Span.start_byte ~new_name
      with
      | None -> "a rename after a prepareRename that offered it gave no answer"
      | Some (Error _) -> ""
      | Some (Ok edits) -> (
          match document_edits text edits with
          | Error report -> report
          | Ok forward -> (
              let renamed = apply_edits text forward in
              Jide_engine.Engine.change_document engine ~uri ~version:2
                ~text:renamed;
              ignore (Jide_engine.Engine.check engine ~uri);
              let back_byte = moved forward span.Jide_lang.Span.start_byte in
              let restored =
                match
                  Jide_controller.Rename_features.rename engine ~uri
                    ~byte:back_byte ~new_name:old_name
                with
                | None -> Error "the rename back gave no answer"
                | Some (Error reason) ->
                    Error ("the rename back was refused: " ^ reason)
                | Some (Ok edits) -> (
                    match document_edits renamed edits with
                    | Error report -> Error report
                    | Ok back -> Ok (apply_edits renamed back))
              in
              Jide_engine.Engine.change_document engine ~uri ~version:3 ~text;
              ignore (Jide_engine.Engine.check engine ~uri);
              match restored with
              | Error report ->
                  Printf.sprintf "%s renamed to %s: %s" old_name new_name report
              | Ok back when String.equal back text -> ""
              | Ok _ ->
                  Printf.sprintf
                    "%s renamed to %s and back does not restore the text"
                    old_name new_name)))

let renames = 3

(** The engine's path over the same text: the store, the analysis key, the
    byte-order mark and the warnings the compiler printed with line and
    column numbers rather than offsets; then the rename round trip at a few
    of the text's names. *)
let check_engine (st : Gen.state) (text : string) =
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.open_document engine ~uri ~version:1 ~text;
  match Jide_engine.Engine.check engine ~uri with
  | None -> "the engine forgot the document it was given"
  | Some analysis -> (
      let diagnostics = analysis.Jide_engine.Engine.diagnostics in
      let length = String.length text in
      match check_ordering "the engine" diagnostics with
      | "" -> (
          match check_inside_engine diagnostics length with
          | "" ->
              let occurrences =
                match Jide_engine.Engine.snapshot engine ~uri with
                | Some snapshot ->
                    Array.of_list
                      snapshot.Jide_engine.Engine.index
                        .Jide_engine.Index.occurrences
                | None -> [||]
              in
              let rec loop n =
                if n = 0 || Array.length occurrences = 0 then ""
                else
                  let occurrence = Gen.pick st occurrences in
                  let byte =
                    occurrence.Jide_engine.Index.last.Jide_lang.Span.start_byte
                  in
                  match rename_round_trip engine text ~byte with
                  | "" -> loop (n - 1)
                  | report -> report
              in
              loop renames
          | report -> report)
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
              else check_engine st text)
      | report -> report)
