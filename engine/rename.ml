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
  index : Index.t;  (** the file as the searching resolution read it *)
  occurrences : Index.occurrence list;  (** the identity's own, in order *)
}

let compare_occurrence (left : Index.occurrence) (right : Index.occurrence) =
  Jide_lang.Span.compare left.Index.span right.Index.span

(* Every resolution once, by its entry, the document's own first. *)
let distinct (resolutions : Resolve.t list) =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun resolution ->
      let entry = Index.normalise (Resolve.entry resolution) in
      if Hashtbl.mem seen entry then false
      else (
        Hashtbl.add seen entry ();
        true))
    resolutions

(* Every file the resolutions read, once each, through the first resolution that
   reached it: the document first, then the files its resolution read, then each
   project file's own resolution in the order the caller gave them. *)
let reached (resolutions : Resolve.t list) =
  let seen = Hashtbl.create 16 in
  let found = ref [] in
  List.iter
    (fun (resolution : Resolve.t) ->
      List.iter
        (fun (index : Index.t) ->
          let path = Index.normalise index.Index.file in
          if not (Hashtbl.mem seen path) then (
            Hashtbl.add seen path ();
            found := (resolution, path, index) :: !found))
        (Resolve.files resolution))
    resolutions;
  List.rev !found

(* The occurrences of the identity in every reached file. Every resolution that
   reads a file is asked about it, not only the first: a file read at the top
   level by one entry and inside a namespace by another means what each walk
   says, and an occurrence is an edit only where every walk agrees. *)
let search ~(resolutions : Resolve.t list) ~(target : Index.declaration) =
  List.filter_map
    (fun (_first, path, (index : Index.t)) ->
      let readers =
        List.filter
          (fun resolution ->
            match Resolve.file resolution path with
            | Some read -> String.equal read.Index.text index.Index.text
            | None -> false)
          resolutions
      in
      let plan occurrence =
        let plans =
          List.map
            (fun resolution -> classify resolution ~file:path target occurrence)
            readers
        in
        match
          List.find_opt (function Shared _ -> true | _ -> false) plans
        with
        | Some shared -> shared
        | None ->
            if not (List.mem Edit plans) then Skip
            else if
              List.for_all (fun plan -> plan = Edit || plan = Unbound) plans
            then Edit
            else
              Shared
                (Printf.sprintf
                   "%s in %s means %s for one file that reads it and something \
                    else for another, so an edit of that file would change \
                    both"
                   occurrence.Index.name path target.Index.name)
      in
      let occurrences =
        List.filter_map
          (fun occurrence ->
            match plan occurrence with
            | Skip | Unbound -> None
            | Edit -> Some occurrence
            | Shared reason -> raise (Refused reason))
          (List.sort compare_occurrence index.Index.occurrences)
      in
      match occurrences with
      | [] -> None
      | _ -> Some { path; index; occurrences })
    (reached resolutions)

(* ------------------------------------------------------------------ *)
(* The rename made in memory, and the meaning of every name compared *)

(* One edit as a byte range and the length it changes the text by. *)
type shift = { stop : int; delta : int }

let shifts_of (edits : edit list) =
  List.map
    (fun (edit : edit) ->
      let start = edit.span.Jide_lang.Span.start_byte in
      let stop = edit.span.Jide_lang.Span.end_byte in
      { stop; delta = String.length edit.new_text - (stop - start) })
    edits

(* Where a byte that starts a name lands once the file's edits are made: every
   edit that ends at or before it moves it by its own change of length. An
   edit never cuts a name in two, so a name's start is never inside one. *)
let moved (shifts : shift list) byte =
  List.fold_left
    (fun byte_after shift ->
      if shift.stop <= byte then byte_after + shift.delta else byte_after)
    byte shifts

let apply_edits (text : string) (edits : edit list) =
  let buffer = Buffer.create (String.length text + 64) in
  let cursor =
    List.fold_left
      (fun cursor (edit : edit) ->
        let start = edit.span.Jide_lang.Span.start_byte in
        let stop = edit.span.Jide_lang.Span.end_byte in
        Buffer.add_string buffer (String.sub text cursor (start - cursor));
        Buffer.add_string buffer edit.new_text;
        stop)
      0 edits
  in
  Buffer.add_string buffer
    (String.sub text cursor (String.length text - cursor));
  Buffer.contents buffer

type world = {
  shifts : (string * shift list) list;  (** by normalised path *)
  target : Index.declaration;
}

let shifts_in world path =
  Option.value ~default:[] (List.assoc_opt (Index.normalise path) world.shifts)

(* A declaration before the rename, at its place after it. *)
let key_before world (declaration : Index.declaration) =
  let file = Index.normalise declaration.Index.file in
  ( file,
    moved (shifts_in world file)
      declaration.Index.name_span.Jide_lang.Span.start_byte,
    declaration.Index.site )

let key_after (declaration : Index.declaration) =
  ( Index.normalise declaration.Index.file,
    declaration.Index.name_span.Jide_lang.Span.start_byte,
    declaration.Index.site )

let is_target_after world (declaration : Index.declaration) =
  let file, byte, _site = key_after declaration in
  let target_file, target_byte, _ = key_before world world.target in
  String.equal file target_file && byte = target_byte

let same_meaning world (before : Resolve.answer) (after : Resolve.answer) =
  match (before, after) with
  | Resolve.Resolved left, Resolve.Resolved right ->
      key_before world left = key_after right
  | Resolve.Intrinsic, Resolve.Intrinsic
  | Resolve.Annotation, Resolve.Annotation
  | Resolve.Include_path, Resolve.Include_path
  | Resolve.Unresolved, Resolve.Unresolved ->
      true
  | _ -> false

(* The reason a changed meaning is refused with, in the words of the
   collision: the name the occurrence is written with after the rename, and
   the declaration it would mean or the one it would stop meaning. *)
let changed world ~path ~(written : string) (before : Resolve.answer)
    (after : Resolve.answer) =
  match (before, after) with
  | Resolve.Resolved left, Resolve.Resolved right ->
      if Index.equal_declaration left world.target then
        collision_reason written right
      else if is_target_after world right then collision_reason written left
      else changed_reason written path
  | Resolve.Unresolved, Resolve.Resolved _ -> unbound_reason written path
  | _ -> changed_reason written path

let check_file world ~(before : Resolve.t) ~(after : Resolve.t) path =
  match (Resolve.file before path, Resolve.file after path) with
  | Some old_index, Some new_index ->
      let old_occurrences = old_index.Index.occurrences in
      let new_occurrences = new_index.Index.occurrences in
      let shifts = shifts_in world path in
      if
        List.length old_occurrences <> List.length new_occurrences
        || Resolve.sites before path <> Resolve.sites after path
        || not
             (List.for_all2
                (fun (old_one : Index.occurrence) (new_one : Index.occurrence) ->
                  moved shifts old_one.Index.span.Jide_lang.Span.start_byte
                  = new_one.Index.span.Jide_lang.Span.start_byte)
                old_occurrences new_occurrences)
      then raise (Refused (reparse_reason path));
      List.iter2
        (fun (old_one : Index.occurrence) (new_one : Index.occurrence) ->
          List.iter
            (fun site ->
              let was =
                Resolve.answer_at_site before ~site ~file:path
                  ~byte:old_one.Index.span.Jide_lang.Span.start_byte
              in
              let now =
                Resolve.answer_at_site after ~site ~file:path
                  ~byte:new_one.Index.span.Jide_lang.Span.start_byte
              in
              if not (same_meaning world was now) then
                raise
                  (Refused
                     (changed world ~path ~written:new_one.Index.name was now)))
            (Resolve.sites before path))
        old_occurrences new_occurrences
  | None, None -> ()
  | _ -> raise (Refused (reparse_reason path))

(* The compiler's reports of a name declared twice, as places after the
   rename, in one order: a rename moves names, so the walk's own order of a
   namespace's merge, which follows the names, is not kept. *)
let duplicate_keys keys pairs =
  List.sort compare
    (List.map (fun (later, earlier) -> (keys later, keys earlier)) pairs)

let check_duplicates world ~new_name ~(before : Resolve.t) ~(after : Resolve.t)
    =
  let old_pairs = Resolve.duplicates before in
  let new_pairs = Resolve.duplicates after in
  let old_keys = duplicate_keys (key_before world) old_pairs in
  let new_keys = duplicate_keys key_after new_pairs in
  if old_keys <> new_keys then (
    (* A pair the rename adds names the renamed declaration and another one:
       the other one is the collision. *)
    (match
       List.find_opt
         (fun (later, earlier) ->
           not (List.mem (key_after later, key_after earlier) old_keys))
         new_pairs
     with
    | Some (later, earlier) ->
        let other = if is_target_after world later then earlier else later in
        raise (Refused (collision_reason new_name other))
    | None -> ());
    match
      List.find_opt
        (fun (later, earlier) ->
          not
            (List.mem
               (key_before world later, key_before world earlier)
               new_keys))
        old_pairs
    with
    | Some (later, earlier) ->
        let other =
          if Index.equal_declaration later world.target then earlier else later
        in
        raise (Refused (undone_duplicate_reason world.target.Index.name other))
    | None -> raise (Refused (changed_reason new_name world.target.Index.file)))

let verify ~(resolutions : Resolve.t list) ~(target : Index.declaration)
    ~(new_name : string) (groups : (searched * edit list) list) =
  let texts =
    List.map
      (fun ((group : searched), edits) ->
        (group.path, apply_edits group.index.Index.text edits))
      groups
  in
  let world =
    {
      shifts =
        List.map
          (fun ((group : searched), edits) -> (group.path, shifts_of edits))
          groups;
      target;
    }
  in
  let edited path = List.mem_assoc (Index.normalise path) texts in
  List.iter
    (fun (before : Resolve.t) ->
      let files = Resolve.files before in
      if List.exists (fun (index : Index.t) -> edited index.Index.file) files
      then (
        (match Resolve.unparsed before with
        | path :: _ -> raise (Refused (broken_reason path))
        | [] -> ());
        let after = Resolve.replay before ~edited:texts in
        let paths (resolution : Resolve.t) =
          List.map
            (fun (index : Index.t) -> Index.normalise index.Index.file)
            (Resolve.files resolution)
        in
        if paths before <> paths after then
          raise (Refused (reparse_reason (Resolve.entry before)));
        List.iter (check_file world ~before ~after) (paths before);
        check_duplicates world ~new_name ~before ~after))
    resolutions

(* ------------------------------------------------------------------ *)

let broken_files ~(resolve : Resolve.t) ~(broken : string -> bool) =
  List.filter_map
    (fun (index : Index.t) ->
      if broken index.Index.file then Some index.Index.file else None)
    (Resolve.files resolve)
  @ Resolve.unparsed resolve

(* Both entry points refuse a recovered require closure the same way and with
   the same words: a [prepareRename] that offered a name the following [rename]
   would not write shows its input box for a reason the user cannot see yet.
   The position's own refusals come from the same [locate], so they are shared
   too. The sharing refusal is the plan's: it does not depend on the new name. *)
let subject_or_refusal ~(index : Index.t) ~(resolutions : Resolve.t list)
    ~(resolve : Resolve.t) ~(byte : int) ~(broken : string -> bool) =
  match broken_files ~resolve ~broken with
  | path :: _ -> Error (broken_reason path)
  | [] -> locate ~index ~resolutions ~byte

(* The subject, and every group the rename would edit, or the refusal that no
   edit can make without changing another meaning. None of it depends on the
   new name. *)
let plan ~(index : Index.t) ~(resolve : Resolve.t) ~(files : Resolve.t list)
    ~(byte : int) ~(broken : string -> bool) =
  let resolutions = distinct (resolve :: files) in
  match subject_or_refusal ~index ~resolutions ~resolve ~byte ~broken with
  | Error reason -> Error reason
  | Ok subject -> (
      try
        let groups = search ~resolutions ~target:subject.declaration in
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
        Ok (subject, resolutions, groups)
      with Refused reason -> Error reason)

let prepare ~(files : Resolve.t list) ~(index : Index.t) ~(resolve : Resolve.t)
    ~(byte : int) ~(broken : string -> bool) =
  match plan ~index ~resolve ~files ~byte ~broken with
  | Error reason -> Error reason
  | Ok (subject, _resolutions, _groups) ->
      Ok (subject.writable, subject.declaration.Index.kind)

let rename ~(files : Resolve.t list) ~(index : Index.t) ~(resolve : Resolve.t)
    ~(byte : int) ~(new_name : string) ~(broken : string -> bool) =
  if not (valid_name new_name) then Error (invalid_reason new_name)
  else
    match plan ~index ~resolve ~files ~byte ~broken with
    | Error reason -> Error reason
    | Ok (subject, resolutions, groups) -> (
        let target = subject.declaration in
        if target.Index.kind = Index.Function && reserved_function_name new_name
        then Error (reserved_reason new_name)
        else
          let groups =
            List.map
              (fun (group : searched) ->
                ( group,
                  List.filter_map
                    (fun occurrence ->
                      Option.map
                        (fun span -> { span; new_text = new_name })
                        (writable_span occurrence))
                    group.occurrences ))
              groups
          in
          (* A rename to the name the declaration already has writes every
             occurrence as itself, so there is nothing to compare. *)
          match
            if String.equal new_name target.Index.name then ()
            else verify ~resolutions ~target ~new_name groups
          with
          | () ->
              Ok
                (List.map
                   (fun ((group : searched), edits) -> (group.path, edits))
                   groups)
          | exception Refused reason -> Error reason)
