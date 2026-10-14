(* Document highlights, from the index and the resolver.

   One symbol, one file: every occurrence the resolver attributed to the
   declaration the byte is on, plus the declaration itself. Nothing is
   matched by name, so a shadowing declaration in another block of the same
   file stays dark. *)

type kind = Text | Read | Write
type highlight = { span : Jide_lang.Span.t; kind : kind }

(* What the occurrence at a byte is, when it is something highlightable. *)
type subject =
  | Declaration of Index.declaration
  | Intrinsic of string  (** an intrinsic, named as it is written after [#] *)
  | Annotation of string  (** an annotation label *)

let compare_highlight (left : highlight) (right : highlight) =
  Jide_lang.Span.compare left.span right.span

let subject_at ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int) =
  match Index.find_occurrence index ~byte with
  | None -> None
  (* A qualified name has one range for the whole name, so a byte before its
     last segment is on the namespace, which is not a symbol of its own: the
     occurrences a client would want lit are not the ones this position
     names. *)
  | Some occurrence when byte < occurrence.Index.last.Jide_lang.Span.start_byte
    ->
      None
  | Some occurrence -> (
      match occurrence.Index.role with
      | Index.Prim -> Some (Intrinsic occurrence.Index.name)
      | Index.Annot -> Some (Annotation occurrence.Index.name)
      | Index.Include_path -> None
      | _ -> (
          match
            Resolve.answer_at resolve ~file:index.Index.file
              ~byte:occurrence.Index.span.Jide_lang.Span.start_byte
          with
          | Some (_, Resolve.Resolved declaration) ->
              if declaration.Index.kind = Index.Namespace then None
              else Some (Declaration declaration)
          | Some (_, Resolve.Intrinsic) ->
              Some (Intrinsic occurrence.Index.name)
          | Some (_, Resolve.Annotation) ->
              Some (Annotation occurrence.Index.name)
          | Some (_, Resolve.Include_path) | Some (_, Resolve.Unresolved) | None
            ->
              None))

(* The index records no read/write distinction for a variable occurrence:
   [lvalues] and [use_ident] both write [Var], and the occurrence carries
   nothing else. Every occurrence is therefore [Text], and the interface says
   so. *)
let kind_of (_occurrence : Index.occurrence) = Text

let matches ~(index : Index.t) ~(resolve : Resolve.t) (subject : subject)
    (occurrence : Index.occurrence) =
  match subject with
  | Intrinsic name ->
      occurrence.Index.role = Index.Prim
      && String.equal occurrence.Index.name name
  | Annotation name ->
      occurrence.Index.role = Index.Annot
      && String.equal occurrence.Index.name name
  | Declaration target -> (
      match
        Resolve.answer_at resolve ~file:index.Index.file
          ~byte:occurrence.Index.span.Jide_lang.Span.start_byte
      with
      | Some (_, Resolve.Resolved declaration) ->
          Index.equal_declaration declaration target
      | _ -> false)

(* The last segment of a qualified name, which is the range a client paints:
   the compiler gives one range for the whole name, and the index reads the
   last segment from the source bytes when no space surrounds its [::]. For a
   name that is not qualified it is the occurrence's own range. *)
let span_of (occurrence : Index.occurrence) = occurrence.Index.last

let at ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int) =
  match subject_at ~index ~resolve ~byte with
  | None -> []
  | Some subject ->
      let highlights =
        List.filter_map
          (fun (occurrence : Index.occurrence) ->
            if matches ~index ~resolve subject occurrence then
              Some { span = span_of occurrence; kind = kind_of occurrence }
            else None)
          index.Index.occurrences
      in
      List.sort compare_highlight highlights
