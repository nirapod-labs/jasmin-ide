(* Rename, by identity: the declaration's file and the byte where its name
   starts, never a compiler unique id.

   Every occurrence rewritten here was resolved, at its own byte, to the very
   declaration being renamed. Nothing is matched by name alone, so a
   same-named declaration in another block, another file or another namespace
   keeps its own occurrences, and a name the resolver never reached is left
   alone.

   The collision rule asks the resolver's own scope at each occurrence: when
   the name an occurrence would be written with after the rename already
   names a different declaration at that byte, the rename is refused. That is
   the one rule that keeps a rename safe, and it is asked of the environment
   the walk built at that byte, which is the view pretyping takes. *)

type edit = { span : Jide_lang.Span.t; new_text : string }
type edits = (string * edit list) list

(* ------------------------------------------------------------------ *)
(* Words *)

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

(* The lexer's own table, so a keyword a later Jasmin adds is refused here
   without a second list to keep in step. *)
let keywords = List.map (fun (word, _token) -> word) Jasmin.Lexer._keywords
let is_keyword word = word <> "" && List.exists (String.equal word) keywords

let valid_name (name : string) =
  let length = String.length name in
  length > 0
  && is_ident_start name.[0]
  && (let rec loop index =
        index >= length || (is_ident_char name.[index] && loop (index + 1))
      in
      loop 1)
  && not (is_keyword name)

(* The identifier holding [byte], or the one ending at it: a client sends the
   position just past the name it offers when the user clicks at its edge. *)
let word_at (text : string) (byte : int) =
  let length = String.length text in
  let byte = max 0 (min byte length) in
  let inside index =
    index >= 0 && index < length && is_ident_char text.[index]
  in
  let anchor =
    if inside byte then Some byte
    else if inside (byte - 1) then Some (byte - 1)
    else None
  in
  match anchor with
  | None -> ""
  | Some anchor ->
      let rec back index =
        if inside (index - 1) then back (index - 1) else index
      in
      let rec forward index =
        if inside index then forward (index + 1) else index
      in
      let start = back anchor in
      let stop = forward anchor in
      if stop <= start then "" else String.sub text start (stop - start)

(* ------------------------------------------------------------------ *)
(* Refusals *)

let no_position = "the position holds no name"
let keyword_reason = "a keyword is not a name a rename can change"

let intrinsic_reason =
  "an intrinsic is fixed by the compiler and cannot be renamed"

let annotation_reason =
  "an annotation label is fixed by the compiler and cannot be renamed"

let include_reason = "an include path names a configuration entry, not a symbol"

let namespace_reason =
  "a namespace's own name has no range of its own inside a qualified name"

let undeclared_reason = "the name at the position has no declaration"

let spaced_reason =
  "a qualified name written with spaces around :: has no range for its last \
   segment"

let prefix_reason =
  "the position holds the namespace part of a qualified name, not the name it \
   qualifies"

let invalid_reason name =
  Printf.sprintf "%s is not a valid Jasmin identifier" name

let broken_reason path =
  Printf.sprintf
    "a file the document requires has recovered syntax errors, so no rename is \
     offered: %s"
    path

let collision_reason key (other : Index.declaration) =
  Printf.sprintf "%s already names a different declaration (%s in %s)" key
    other.Index.name other.Index.file

(* ------------------------------------------------------------------ *)
(* The subject: what a rename at a byte would rewrite *)

type subject = { declaration : Index.declaration; writable : Jide_lang.Span.t }

let same_span (left : Jide_lang.Span.t) (right : Jide_lang.Span.t) =
  Jide_lang.Span.compare left right = 0

(* A qualified name has one range for the whole name, so only its last
   segment can be rewritten, and only when no space surrounds its [::]. The
   index reads that segment from the source bytes and reports the whole range
   when it cannot, which is exactly the case refused here. *)
let writable_span (occurrence : Index.occurrence) =
  if String.contains occurrence.Index.name ':' then
    if same_span occurrence.Index.last occurrence.Index.span then None
    else Some occurrence.Index.last
  else Some occurrence.Index.span

let locate ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int) :
    (subject, string) result =
  if is_keyword (word_at index.Index.text byte) then Error keyword_reason
  else
    match Index.find_occurrence index ~byte with
    | None -> Error no_position
    | Some occurrence -> (
        if
          (* A qualified name has one range for the whole name, so its first
             bytes name the namespace, not the symbol the last segment names:
             renaming from there would rewrite a symbol the client never pointed
             at. *)
          byte < occurrence.Index.last.Jide_lang.Span.start_byte
        then Error prefix_reason
        else
          match occurrence.Index.role with
          | Index.Prim -> Error intrinsic_reason
          | Index.Annot -> Error annotation_reason
          | Index.Include_path -> Error include_reason
          | _ -> (
              let at = occurrence.Index.span.Jide_lang.Span.start_byte in
              match
                Resolve.answer_at resolve ~file:index.Index.file ~byte:at
              with
              | None -> Error undeclared_reason
              | Some (_, Resolve.Intrinsic) -> Error intrinsic_reason
              | Some (_, Resolve.Annotation) -> Error annotation_reason
              | Some (_, Resolve.Include_path) -> Error include_reason
              | Some (_, Resolve.Unresolved) -> Error undeclared_reason
              | Some (_, Resolve.Resolved declaration) -> (
                  if declaration.Index.kind = Index.Namespace then
                    Error namespace_reason
                  else
                    match writable_span occurrence with
                    | None -> Error spaced_reason
                    | Some writable -> Ok { declaration; writable })))

(* ------------------------------------------------------------------ *)
(* The occurrences of one identity, grouped by file *)

type searched = {
  path : string;
  resolution : Resolve.t;  (** the resolution whose answers cover [path] *)
  occurrences : Index.occurrence list;  (** the identity's own, in order *)
}

let compare_occurrence (left : Index.occurrence) (right : Index.occurrence) =
  Jide_lang.Span.compare left.Index.span right.Index.span

let belongs (index : Index.t) (resolve : Resolve.t) (target : Index.declaration)
    (occurrence : Index.occurrence) =
  match
    Resolve.answer_at resolve ~file:index.Index.file
      ~byte:occurrence.Index.span.Jide_lang.Span.start_byte
  with
  | Some (_, Resolve.Resolved declaration) ->
      Index.equal_declaration declaration target
  | _ -> false

let group_of (index : Index.t) (resolve : Resolve.t)
    (target : Index.declaration) =
  List.sort compare_occurrence
    (List.filter (belongs index resolve target) index.Index.occurrences)

(* The document first, then the files its resolution read, then each project
   file's own resolution in the order the caller gave them. A file reached by
   two resolutions is searched once, through the first one that reached it. *)
let search ~(resolve : Resolve.t) ~(files : Resolve.t list)
    ~(target : Index.declaration) =
  let seen = Hashtbl.create 16 in
  let groups = ref [] in
  let add (resolution : Resolve.t) (index : Index.t) =
    let key = Index.normalise index.Index.file in
    if not (Hashtbl.mem seen key) then (
      Hashtbl.add seen key ();
      match group_of index resolution target with
      | [] -> ()
      | occurrences ->
          groups :=
            { path = index.Index.file; resolution; occurrences } :: !groups)
  in
  List.iter (add resolve) (Resolve.files resolve);
  List.iter
    (fun (other : Resolve.t) -> List.iter (add other) (Resolve.files other))
    files;
  List.rev !groups

(* ------------------------------------------------------------------ *)
(* The collision rule *)

(* The name space of the declaration decides which list of the scope the
   question is asked of: a variable and a function of one name are different
   symbols in Jasmin, so one never collides with the other. *)
let visible_of (scope : Resolve.scope) (kind : Index.kind) =
  match kind with
  | Index.Function -> scope.Resolve.funs
  | Index.Type_alias -> scope.Resolve.types
  | _ -> scope.Resolve.vars

(* The name an occurrence would be written with after the rename: a qualified
   name keeps its qualification and replaces its last segment. *)
let renamed_written (occurrence : Index.occurrence) (new_name : string) =
  match String.rindex_opt occurrence.Index.name ':' with
  | None -> new_name
  | Some index -> String.sub occurrence.Index.name 0 (index + 1) ^ new_name

type clash = Free | Taken of Index.declaration | Unknown

let clash_at (resolve : Resolve.t) ~(file : string) ~(byte : int)
    ~(key : string) ~(kind : Index.kind) ~(target : Index.declaration) =
  match Resolve.scope_at resolve ~file ~byte with
  | None -> Unknown
  | Some scope -> (
      match
        List.find_opt
          (fun (binding : Resolve.binding) ->
            String.equal binding.Resolve.name key)
          (visible_of scope kind)
      with
      | None -> Free
      | Some binding ->
          if Index.equal_declaration binding.Resolve.declaration target then
            Free
          else Taken binding.Resolve.declaration)

(* ------------------------------------------------------------------ *)

let prepare ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int) =
  match locate ~index ~resolve ~byte with
  | Error _ -> None
  | Ok subject -> Some (subject.writable, subject.declaration.Index.kind)

let broken_files ~(resolve : Resolve.t) ~(broken : string -> bool) =
  List.filter_map
    (fun (index : Index.t) ->
      if broken index.Index.file then Some index.Index.file else None)
    (Resolve.files resolve)

let rename ~(files : Resolve.t list) ~(index : Index.t) ~(resolve : Resolve.t)
    ~(byte : int) ~(new_name : string) ~(broken : string -> bool) =
  if not (valid_name new_name) then Error (invalid_reason new_name)
  else
    match broken_files ~resolve ~broken with
    | path :: _ -> Error (broken_reason path)
    | [] -> (
        match locate ~index ~resolve ~byte with
        | Error reason -> Error reason
        | Ok subject ->
            let target = subject.declaration in
            let groups = search ~resolve ~files ~target in
            (* Every occurrence is checked before the rename is answered: a
               partial edit set would leave the program naming two
               declarations, and an occurrence whose last segment has no range
               cannot be rewritten at all. The check and the edit are made
               together, so no range is ever written that was not checked, and
               one refusal refuses the whole rename. *)
            let rec check_groups = function
              | [] -> Ok []
              | (group : searched) :: rest -> (
                  match check_group group with
                  | Error _ as error -> error
                  | Ok edits -> (
                      match check_groups rest with
                      | Error _ as error -> error
                      | Ok groups -> Ok ((group.path, edits) :: groups)))
            and check_group (group : searched) =
              let rec loop acc = function
                | [] -> Ok (List.rev acc)
                | (occurrence : Index.occurrence) :: rest -> (
                    match writable_span occurrence with
                    | None -> Error spaced_reason
                    | Some span -> (
                        let at =
                          occurrence.Index.span.Jide_lang.Span.start_byte
                        in
                        let key = renamed_written occurrence new_name in
                        match
                          clash_at group.resolution ~file:group.path ~byte:at
                            ~key ~kind:target.Index.kind ~target
                        with
                        | Free ->
                            loop ({ span; new_text = new_name } :: acc) rest
                        | Unknown ->
                            Error
                              "the names in scope at one occurrence are not \
                               known"
                        | Taken other -> Error (collision_reason key other)))
              in
              loop [] group.occurrences
            in
            check_groups groups)
