(* Rename, by identity: the declaration's file and the byte where its name
   starts, never a compiler unique id.

   Every occurrence rewritten here was resolved, at its own byte, to the very
   declaration being renamed. Nothing is matched by name alone, so a
   same-named declaration in another block, another file or another namespace
   keeps its own occurrences, and a name the resolver never reached is left
   alone. A declaration is its text: a file read into two namespaces declares
   two instances of each name it holds, and both are renamed together,
   because one edit of the file renames both.

   Whether the rename is safe is not predicted from scope rules. The edit is
   made in memory, every resolution that reads an edited file is walked again
   over the edited texts, and the rename is answered only when every
   occurrence in every file means the declaration it meant before, at every
   site it is read at, and the compiler would report the same declarations as
   declared twice. The walk is the resolver's, which is pretyping's naming
   rules, so a name the compiler would bind to another declaration after the
   rename, a declaration that would shadow one a later use reaches, or a
   second function of one name are all seen as what they are: a change. *)

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

(* The names a call is read as something other than a function call by: the
   compiler's flag combinations ([_EQ], [_uLT], ...), which it refuses as a
   function's name and reads a call to as the combination, and the three
   predicates of an assertion. The first list is the compiler's own printer
   over its own type; the second is [safety_map] in [Pretyping.tt_assert],
   which the library does not export. *)
let combine_flag_names =
  let open Jasmin.Operators in
  List.map Jasmin.Printer.string_of_combine_flags
    [
      CF_LT Jasmin.Wsize.Signed;
      CF_LT Jasmin.Wsize.Unsigned;
      CF_LE Jasmin.Wsize.Signed;
      CF_LE Jasmin.Wsize.Unsigned;
      CF_EQ;
      CF_NEQ;
      CF_GE Jasmin.Wsize.Signed;
      CF_GE Jasmin.Wsize.Unsigned;
      CF_GT Jasmin.Wsize.Signed;
      CF_GT Jasmin.Wsize.Unsigned;
    ]

let assertion_predicates = [ "is_mem_init"; "is_arr_init"; "is_var_init" ]

let reserved_function_name name =
  List.mem name combine_flag_names || List.mem name assertion_predicates

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

let reserved_reason name =
  Printf.sprintf
    "%s is a name the compiler reads as a built-in where it is called, so no \
     function can take it"
    name

let broken_reason path =
  Printf.sprintf
    "a file the document requires has recovered syntax errors, so no rename is \
     offered: %s"
    path

let reparse_reason path =
  Printf.sprintf
    "the renamed text of %s would not parse into the same names, so no rename \
     is offered"
    path

let unbound_reason name path =
  Printf.sprintf
    "%s is used in %s with no declaration, and the rename would give it one"
    name path

let changed_reason name path =
  Printf.sprintf "the rename would change what %s in %s names" name path

let undone_duplicate_reason name (other : Index.declaration) =
  Printf.sprintf
    "%s is declared twice in one scope (the other is in %s), which the \
     compiler reports, and the rename would change that report"
    name other.Index.file

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

(* What the name at the occurrence names: the first of the resolutions that
   read this file, with these bytes, and resolved it. The document's own
   resolution comes first; a file checked in the context of an entry has names
   only that entry's walk gives it, so the others are asked after it. *)
let answer_of ~(index : Index.t) ~(resolutions : Resolve.t list) ~(at : int) =
  let path = Index.normalise index.Index.file in
  let readers =
    List.filter
      (fun resolution ->
        match Resolve.file resolution path with
        | Some read -> String.equal read.Index.text index.Index.text
        | None -> false)
      resolutions
  in
  let answers =
    List.filter_map
      (fun resolution ->
        Option.map snd (Resolve.answer_at resolution ~file:path ~byte:at))
      readers
  in
  match
    List.find_opt (function Resolve.Resolved _ -> true | _ -> false) answers
  with
  | Some answer -> Some answer
  | None -> ( match answers with answer :: _ -> Some answer | [] -> None)

let locate ~(index : Index.t) ~(resolutions : Resolve.t list) ~(byte : int) :
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
              match answer_of ~index ~resolutions ~at with
              | None -> Error undeclared_reason
              | Some Resolve.Intrinsic -> Error intrinsic_reason
              | Some Resolve.Annotation -> Error annotation_reason
              | Some Resolve.Include_path -> Error include_reason
              | Some Resolve.Unresolved -> Error undeclared_reason
              | Some (Resolve.Resolved declaration) -> (
                  if declaration.Index.kind = Index.Namespace then
                    Error namespace_reason
                  else
                    match writable_span occurrence with
                    | None -> Error spaced_reason
                    | Some writable -> Ok { declaration; writable })))

(* ------------------------------------------------------------------ *)
(* What each occurrence means at every require site of its file *)

exception Refused of string

(* The namespace a site was read into, for a reason a client shows: the
   document's own walk has no namespace. *)
let read_into (site : string) =
  match String.index_opt site '@' with
  | None | Some 0 -> "the document"
  | Some index -> "namespace " ^ String.sub site 0 index

(* What an occurrence means at each site its file was read at. A file read into
   two namespaces is one text read twice, and each reading declares its own
   instances of the names in it. *)
let meanings (resolution : Resolve.t) ~(file : string)
    (occurrence : Index.occurrence) =
  let byte = occurrence.Index.span.Jide_lang.Span.start_byte in
  List.map
    (fun site -> (site, Resolve.answer_at_site resolution ~site ~file ~byte))
    (Resolve.sites resolution file)

(* The target is the declaration's text: every instance a reading of its file
   makes is renamed by the one edit of that file. *)
let is_target (target : Index.declaration) (answer : Resolve.answer) =
  match answer with
  | Resolve.Resolved declaration -> Index.equal_declaration declaration target
  | Resolve.Intrinsic | Resolve.Annotation | Resolve.Include_path
  | Resolve.Unresolved ->
      false

type occurrence_plan =
  | Skip  (** the occurrence means something else at some site, never this *)
  | Unbound  (** the occurrence names no declaration at any site *)
  | Edit
      (** the occurrence is the target at some site, and at every other one it
          names nothing: a reading that leaves a name unbound stays unbound
          once it is renamed, which the comparison after the edit checks *)
  | Shared of string
      (** the target at some site and another declaration at another: the one
          text cannot change one of them alone *)

(* The name a lookup of the occurrence is by: its last segment. Only an
   occurrence written with the declaration's own name can resolve to it, so
   every other one is passed over without asking the walks. *)
let last_segment (name : string) =
  match String.rindex_opt name ':' with
  | None -> name
  | Some index ->
      String.trim (String.sub name (index + 1) (String.length name - index - 1))

let classify (resolution : Resolve.t) ~(file : string)
    (target : Index.declaration) (occurrence : Index.occurrence) =
  if
    not
      (String.equal
         (last_segment occurrence.Index.name)
         (last_segment target.Index.name))
  then Skip
  else
    let meanings = meanings resolution ~file occurrence in
    let hits =
      List.filter (fun (_site, answer) -> is_target target answer) meanings
    in
    let others =
      List.filter
        (fun (_site, answer) ->
          (not (is_target target answer)) && answer <> Resolve.Unresolved)
        meanings
    in
    match (hits, others) with
    | [], [] -> Unbound
    | [], _ :: _ -> Skip
    | _ :: _, [] -> Edit
    | (hit_site, _) :: _, (other_site, other) :: _ ->
        let meaning =
          match other with
          | Resolve.Resolved declaration -> declaration.Index.name
          | Resolve.Intrinsic | Resolve.Annotation | Resolve.Include_path
          | Resolve.Unresolved ->
              "no declaration"
        in
        Shared
          (Printf.sprintf
             "%s in %s means %s when it is read into %s and %s when it is read \
              into %s, so an edit of that file would change both"
             occurrence.Index.name file target.Index.name (read_into hit_site)
             meaning (read_into other_site))

(* ------------------------------------------------------------------ *)
(* The occurrences of one identity, grouped by file *)

type searched = {
  path : string;
  resolution : Resolve.t;  (** the resolution whose answers cover [path] *)
  occurrences : Index.occurrence list;  (** the identity's own, in order *)
}

let compare_occurrence (left : Index.occurrence) (right : Index.occurrence) =
  Jide_lang.Span.compare left.Index.span right.Index.span

(* Every file the resolutions read, once each, through the first resolution that
   reached it: the document first, then the files its resolution read, then each
   project file's own resolution in the order the caller gave them. *)
let reached ~(resolve : Resolve.t) ~(files : Resolve.t list) =
  let seen = Hashtbl.create 16 in
  let found = ref [] in
  let add (resolution : Resolve.t) (index : Index.t) =
    let path = Index.normalise index.Index.file in
    if not (Hashtbl.mem seen path) then (
      Hashtbl.add seen path ();
      found := (resolution, path, index) :: !found)
  in
  List.iter (fun index -> add resolve index) (Resolve.files resolve);
  List.iter
    (fun (other : Resolve.t) ->
      List.iter (fun index -> add other index) (Resolve.files other))
    files;
  List.rev !found

(* The occurrences of the identity in every reached file. A file with none is
   not a group, but it is still asked the rule's second end. *)
let search ~(resolve : Resolve.t) ~(files : Resolve.t list)
    ~(target : Index.declaration) =
  List.filter_map
    (fun (resolution, path, (index : Index.t)) ->
      let occurrences =
        List.filter_map
          (fun occurrence ->
            match classify resolution ~file:path target occurrence with
            | Skip -> None
            | Edit -> Some occurrence
            | Shared reason -> raise (Refused reason))
          (List.sort compare_occurrence index.Index.occurrences)
      in
      match occurrences with
      | [] -> None
      | _ -> Some { path; resolution; occurrences })
    (reached ~resolve ~files)

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

(* The name spaces the scope keeps apart: a function and a type alias live in
   their own lists, and every other declaration an index holds (a parameter, a
   variable, a global) lives in the variables' list. A namespace's own name is
   in none of the three, so a rename to it is not a collision with it. *)
let same_name_space (left : Index.kind) (right : Index.kind) =
  match (left, right) with
  | Index.Namespace, _ | _, Index.Namespace -> false
  | Index.Function, Index.Function -> true
  | Index.Type_alias, Index.Type_alias -> true
  | (Index.Function | Index.Type_alias), _ -> false
  | _, (Index.Function | Index.Type_alias) -> false
  | _ -> true

type clash = Free | Taken of Index.declaration | Unknown

(* The name the occurrence is written with after the rename, at one site: the
   scope the walk of that site built at the occurrence, so a name is compared
   with the declarations that walk sees. *)
let clash_at (resolve : Resolve.t) ~(file : string) ~(site : string)
    ~(byte : int) ~(key : string) ~(kind : Index.kind)
    ~(target : Index.declaration) =
  match Resolve.scope_at_site resolve ~site ~file ~byte with
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
          if Index.same_instance binding.Resolve.declaration target then Free
          else Taken binding.Resolve.declaration)

(* Whether [target] is one of the declarations in scope at [byte] at one site,
   under the name the declaration is written with. A name a namespace merged
   into its parent is listed with its qualification, so the binding's name is
   compared as well as its identity. *)
let in_scope (resolve : Resolve.t) ~(file : string) ~(site : string)
    ~(byte : int) ~(target : Index.declaration) =
  match Resolve.scope_at_site resolve ~site ~file ~byte with
  | None -> false
  | Some (scope : Resolve.scope) ->
      let holds (binding : Resolve.binding) =
        String.equal binding.Resolve.name target.Index.name
        && Index.same_instance binding.Resolve.declaration target
      in
      List.exists holds scope.Resolve.types
      || List.exists holds scope.Resolve.funs
      || List.exists holds scope.Resolve.vars

(* The namespaces a declaration is written inside, outermost first: every
   namespace item of the file whose range holds the declaration's name. *)
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
   close. A variable is pushed into the frame of the namespace it is written
   in, and a namespace merges its contents into its parent under its own name,
   so [A::total] and [total] are two names in one frame. A function or a type
   alias is looked up by its unqualified name in every enclosing frame, so
   visibility alone answers for it. A declaration of another file is left to
   that answer too. *)
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

(* The other end of the collision rule: a declaration the new name would
   duplicate, in the file the group is searched in, where the declaration being
   renamed is in scope at one of the file's sites. *)
let later_clash (resolution : Resolve.t) ~(path : string) (index : Index.t)
    (target : Index.declaration) (new_name : string) =
  (* A rename to the name the declaration already has writes every occurrence
     as itself: no declaration of the program changes, so nothing can collide. *)
  if String.equal new_name target.Index.name then None
  else
    let sites = Resolve.sites resolution path in
    List.find_opt
      (fun (declaration : Index.declaration) ->
        String.equal declaration.Index.name new_name
        && same_name_space declaration.Index.kind target.Index.kind
        && (not (Index.equal_declaration declaration target))
        && one_frame index target declaration
        && List.exists
             (fun site ->
               in_scope resolution ~file:path ~site
                 ~byte:declaration.Index.name_span.Jide_lang.Span.start_byte
                 ~target)
             sites)
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
   too. The sharing refusal is the plan's: it does not depend on the new name. *)
let subject_or_refusal ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int)
    ~(broken : string -> bool) =
  match broken_files ~resolve ~broken with
  | path :: _ -> Error (broken_reason path)
  | [] -> locate ~index ~resolve ~byte

(* The subject, and every group the rename would edit, or the refusal that no
   edit can make without changing another meaning. None of it depends on the
   new name. *)
let plan ~(index : Index.t) ~(resolve : Resolve.t) ~(files : Resolve.t list)
    ~(byte : int) ~(broken : string -> bool) =
  match subject_or_refusal ~index ~resolve ~byte ~broken with
  | Error reason -> Error reason
  | Ok subject -> (
      try
        let groups = search ~resolve ~files ~target:subject.declaration in
        (* A qualified occurrence with no range of its own cannot be written. *)
        List.iter
          (fun (group : searched) ->
            List.iter
              (fun occurrence ->
                match writable_span occurrence with
                | None -> raise (Refused spaced_reason)
                | Some _ -> ())
              group.occurrences)
          groups;
        Ok (subject, groups)
      with Refused reason -> Error reason)

let prepare ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int)
    ~(broken : string -> bool) =
  match plan ~index ~resolve ~files:[] ~byte ~broken with
  | Error reason -> Error reason
  | Ok (subject, _groups) ->
      Ok (subject.writable, subject.declaration.Index.kind)

let renamed_written (occurrence : Index.occurrence) (new_name : string) =
  match String.rindex_opt occurrence.Index.name ':' with
  | None -> new_name
  | Some index -> String.sub occurrence.Index.name 0 (index + 1) ^ new_name

let rename ~(files : Resolve.t list) ~(index : Index.t) ~(resolve : Resolve.t)
    ~(byte : int) ~(new_name : string) ~(broken : string -> bool) =
  if not (valid_name new_name) then Error (invalid_reason new_name)
  else
    match plan ~index ~resolve ~files ~byte ~broken with
    | Error reason -> Error reason
    | Ok (subject, groups) -> (
        let target = subject.declaration in
        (* The rule's other end, once per file searched: a declaration the new
           name already has there, when the declaration being renamed is in
           scope where that one is written. *)
        let rec check_declarations = function
          | [] -> Ok ()
          | (resolution, path, index) :: rest -> (
              match later_clash resolution ~path index target new_name with
              | Some other -> Error (collision_reason new_name other)
              | None -> check_declarations rest)
        in
        (* Every occurrence is checked at every site of its file before the
           rename is answered, so one refusal refuses the whole rename. *)
        let check_group (group : searched) =
          let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | (occurrence : Index.occurrence) :: rest -> (
                match writable_span occurrence with
                | None -> Error spaced_reason
                | Some span -> (
                    let at = occurrence.Index.span.Jide_lang.Span.start_byte in
                    let key = renamed_written occurrence new_name in
                    let clash =
                      List.fold_left
                        (fun found site ->
                          match found with
                          | Taken _ | Unknown -> found
                          | Free ->
                              clash_at group.resolution ~file:group.path ~site
                                ~byte:at ~key ~kind:target.Index.kind ~target)
                        Free
                        (Resolve.sites group.resolution group.path)
                    in
                    match clash with
                    | Free -> loop ({ span; new_text = new_name } :: acc) rest
                    | Unknown ->
                        Error
                          "the names in scope at one occurrence are not known"
                    | Taken other -> Error (collision_reason key other)))
          in
          loop [] group.occurrences
        in
        let rec check_groups acc = function
          | [] -> Ok (List.rev acc)
          | (group : searched) :: rest -> (
              match check_group group with
              | Error _ as error -> error
              | Ok edits -> check_groups ((group.path, edits) :: acc) rest)
        in
        match check_declarations (reached ~resolve ~files) with
        | Error _ as error -> error
        | Ok () -> check_groups [] groups)
