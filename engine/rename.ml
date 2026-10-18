(* Rename, by identity: the declaration's file and the byte where its name
   starts, never a compiler unique id.

   Every occurrence rewritten here was resolved, at its own byte, to the very
   declaration being renamed. Nothing is matched by name alone, so a
   same-named declaration in another block, another file or another namespace
   keeps its own occurrences, and a name the resolver never reached is left
   alone.

   The collision rule is asked at both ends of the scope relation. At each
   occurrence, the name it would be written with after the rename must not
   already name a different declaration there: that is the environment the
   walk built at the occurrence's byte, which is the view pretyping takes. The
   walk is define-before-use, so a declaration written later in the same
   block is invisible there; the rule's other end asks, at the declaration
   each occurrence would duplicate, whether the declaration being renamed
   would land in that declaration's own frame. Between them a rename cannot
   declare one name twice in one frame, which is the one rule that keeps a
   rename safe. A frame is not a scope: a namespace merges its contents into
   its parent under its own name, so what a namespace's own scope declares as
   [total] its parent's frame holds as [A::total]. *)

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

(* The name spaces the scope keeps apart: a function and a type alias live in
   their own lists, and every other declaration an index holds (a parameter, a
   variable, a global) lives in the variables' list. A namespace's own name is
   in none of the three: [namespace A { ... }] and a function or a variable
   called [A] are two names the compiler accepts in one file, so a rename to
   [A] is not a collision with the namespace. *)
let same_name_space (left : Index.kind) (right : Index.kind) =
  match (left, right) with
  | Index.Namespace, _ | _, Index.Namespace -> false
  | Index.Function, Index.Function -> true
  | Index.Type_alias, Index.Type_alias -> true
  | (Index.Function | Index.Type_alias), _ -> false
  | _, (Index.Function | Index.Type_alias) -> false
  | _ -> true

(* Whether [target] is one of the declarations in scope at [byte], under the
   name the declaration is written with. A name a namespace merged into its
   parent is listed with its qualification, which is not the name its own
   scope declares, so the binding's name is compared as well as its identity:
   a declaration of another scope that happens to be visible here is not the
   declaration a later name would be written as. *)
let in_scope (resolve : Resolve.t) ~(file : string) ~(byte : int)
    ~(target : Index.declaration) =
  match Resolve.scope_at resolve ~file ~byte with
  | None -> false
  | Some (scope : Resolve.scope) ->
      let holds (binding : Resolve.binding) =
        String.equal binding.Resolve.name target.Index.name
        && Index.equal_declaration binding.Resolve.declaration target
      in
      List.exists holds scope.Resolve.types
      || List.exists holds scope.Resolve.funs
      || List.exists holds scope.Resolve.vars

(* The namespaces a declaration is written inside, outermost first: every
   namespace item of the file whose range holds the declaration's name. The
   index lists declarations in source order, so the outermost namespace comes
   first. *)
let namespace_path (index : Index.t) (declaration : Index.declaration) =
  List.filter_map
    (fun (item : Index.declaration) ->
      if
        item.Index.kind = Index.Namespace
        && item.Index.item_span.Jide_lang.Span.start_byte
           <= declaration.Index.name_span.Jide_lang.Span.start_byte
        && declaration.Index.name_span.Jide_lang.Span.end_byte
           <= item.Index.item_span.Jide_lang.Span.end_byte
      then Some item.Index.name
      else None)
    index.Index.declarations

(* Whether two declarations land in one frame once the namespaces around them
   close, which is the question the rule's second end is asking. The compiler
   keeps one frame per namespace: a variable is pushed into the frame of the
   namespace it is written in ([Vars.push_core]), and a namespace merges its
   contents into its parent under its own name, so [A::total] and [total] are
   two names in one frame and only two equal paths put two variables in one
   of them. A function or a type alias is pushed differently: [Funs.push]
   looks the unqualified name up in every enclosing frame, so a function
   inside a namespace and one outside it are one name to the compiler, and
   visibility alone answers for them. A declaration of another file is left
   to that answer too: the frame it lands in is the one its [require] was
   read into, which this file's index does not record. *)
let one_frame (index : Index.t) (target : Index.declaration)
    (declaration : Index.declaration) =
  match target.Index.kind with
  | Index.Function | Index.Type_alias -> true
  | _ ->
      if
        String.equal
          (Index.normalise target.Index.file)
          (Index.normalise index.Index.file)
      then namespace_path index target = namespace_path index declaration
      else true

(* The other end of the collision rule. The scope at an occurrence is the scope
   before it, so a declaration written later in the same block is invisible
   there, though renaming to its name would declare that name twice in one
   frame: an error for a function or a type alias, a warning for a variable.
   The question is therefore asked at the declaration the new name would
   duplicate: when the declaration being renamed is already in scope where that
   one is written, and the two would land in one frame, the renamed program has
   two declarations of one name. *)
let later_clash (group : searched) (index : Index.t)
    (target : Index.declaration) (new_name : string) =
  (* A rename to the name the declaration already has writes every occurrence
     as itself: no declaration of the program changes, so nothing can collide,
     and the interface promises the edits rather than a refusal. *)
  if String.equal new_name target.Index.name then None
  else
    List.find_opt
      (fun (declaration : Index.declaration) ->
        String.equal declaration.Index.name new_name
        && same_name_space declaration.Index.kind target.Index.kind
        && (not (Index.equal_declaration declaration target))
        && one_frame index target declaration
        && in_scope group.resolution ~file:group.path
             ~byte:declaration.Index.name_span.Jide_lang.Span.start_byte ~target)
      index.Index.declarations

(* ------------------------------------------------------------------ *)

let broken_files ~(resolve : Resolve.t) ~(broken : string -> bool) =
  List.filter_map
    (fun (index : Index.t) ->
      if broken index.Index.file then Some index.Index.file else None)
    (Resolve.files resolve)

(* Both entry points refuse a recovered require closure the same way and with
   the same words: a [prepareRename] that offered a name the following [rename]
   would not write shows its input box for a reason the user cannot see yet.
   The position's own refusals come from the same [locate], so they are shared
   too. *)
let subject_or_refusal ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int)
    ~(broken : string -> bool) =
  match broken_files ~resolve ~broken with
  | path :: _ -> Error (broken_reason path)
  | [] -> locate ~index ~resolve ~byte

let prepare ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int)
    ~(broken : string -> bool) =
  match subject_or_refusal ~index ~resolve ~byte ~broken with
  | Error reason -> Error reason
  | Ok subject -> Ok (subject.writable, subject.declaration.Index.kind)

let rename ~(files : Resolve.t list) ~(index : Index.t) ~(resolve : Resolve.t)
    ~(byte : int) ~(new_name : string) ~(broken : string -> bool) =
  if not (valid_name new_name) then Error (invalid_reason new_name)
  else
    match subject_or_refusal ~index ~resolve ~byte ~broken with
    | Error reason -> Error reason
    | Ok subject -> (
        let target = subject.declaration in
        let groups = search ~resolve ~files ~target in
        (* The rule's other end, once per file searched: a declaration the new
           name already has there, when the declaration being renamed is in
           scope where that one is written. *)
        let rec check_declarations = function
          | [] -> Ok ()
          | (group : searched) :: rest -> (
              match Resolve.file group.resolution group.path with
              | None -> check_declarations rest
              | Some index -> (
                  match later_clash group index target new_name with
                  | Some other -> Error (collision_reason new_name other)
                  | None -> check_declarations rest))
        in
        (* Every occurrence is checked before the rename is answered: a
           partial edit set would leave the program naming two declarations,
           and an occurrence whose last segment has no range cannot be
           rewritten at all. The check and the edit are made together, so no
           range is ever written that was not checked, and one refusal refuses
           the whole rename. *)
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
                    let at = occurrence.Index.span.Jide_lang.Span.start_byte in
                    let key = renamed_written occurrence new_name in
                    match
                      clash_at group.resolution ~file:group.path ~byte:at ~key
                        ~kind:target.Index.kind ~target
                    with
                    | Free -> loop ({ span; new_text = new_name } :: acc) rest
                    | Unknown ->
                        Error
                          "the names in scope at one occurrence are not known"
                    | Taken other -> Error (collision_reason key other)))
          in
          loop [] group.occurrences
        in
        match check_declarations groups with
        | Error _ as error -> error
        | Ok () -> check_groups groups)
