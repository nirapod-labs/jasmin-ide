(** Parity of the checker with the reference compiler.

    Every [.jazz] file under the corpus folders is checked once per
    architecture its path names, with [jasmin-lsp check], and the verdict and
    the first error are compared with the reference:

    - [--jasminc PATH]: the compiler, run here as
      [jasminc -arch <arch> -until_cstexp <file>]. [-until_cstexp] is the first
      checkpoint after [Compile.preprocess] (parameter expansion and the main
      typing pass), which is where the checker's own passes end;
    - no compiler: the release's [negative.expected], which records the first
      error of every negative case. A case it does not name, or names with a
      pass after the checkpoint, is expected to be accepted.

    A check agrees when both sides accept, or both reject with the same line,
    column and message. The release's record is compared where the reference
    reproduces it at the checkpoint. Exit status: 0 when every check agrees,
    1 when one differs, 2 when the run cannot be made. *)

module H = Jide_harness.Harness

let usage =
  "usage: parity --corpus <dir> [--checks <list>] [--jasminc <path>] \
   [--checker <path>]\n\
  \              [--jobs N] [--tsv <file>] [--keep <dir>]\n\
  \       parity --selftest\n\n\
   <dir> is a release's compiler/tests directory or a root that holds it.\n\
   <list> holds one label, path and architecture per line, tab separated.\n\
   Without --jasminc the release's negative.expected is the reference."

(* ------------------------------------------------------------------ *)
(* Text *)

let is_digit c = c >= '0' && c <= '9'
let is_blank s = String.for_all (fun c -> c = ' ' || c = '\t') s

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

let drop n s = String.sub s n (String.length s - n)

(* One space for every run of blanks, none at either end. *)
let collapse s =
  let b = Buffer.create (String.length s) in
  let pending = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' then pending := Buffer.length b > 0
      else (
        if !pending then Buffer.add_char b ' ';
        pending := false;
        Buffer.add_char b c))
    s;
  Buffer.contents b

let index_from s i sub =
  let n = String.length s and k = String.length sub in
  let rec go i =
    if i + k > n then None
    else if String.sub s i k = sub then Some i
    else go (i + 1)
  in
  go i

let replace_all s ~sub ~by =
  let b = Buffer.create (String.length s) in
  let k = String.length sub in
  let rec go i =
    match index_from s i sub with
    | None -> Buffer.add_string b (drop i s)
    | Some j ->
        Buffer.add_string b (String.sub s i (j - i));
        Buffer.add_string b by;
        go (j + k)
  in
  if k = 0 then s
  else (
    go 0;
    Buffer.contents b)

(* The digits at [i], as a number and the index after them. *)
let digits s i =
  let n = String.length s in
  let rec go j = if j < n && is_digit s.[j] then go (j + 1) else j in
  let j = go i in
  if j = i then None else Some (int_of_string (String.sub s i (j - i)), j)

(* The number after the first [word] followed by digits. *)
let number_after s word =
  let rec go i =
    match index_from s i word with
    | None -> None
    | Some j -> (
        match digits s (j + String.length word) with
        | Some (v, _) -> Some v
        | None -> go (j + 1))
  in
  go 0

let rec spaces s = if starts_with ~prefix:" " s then spaces (drop 1 s) else s

(* Whether a line holds [line N (N], as a location does. *)
let has_location line =
  let rec go i =
    match index_from line i "line " with
    | None -> false
    | Some j -> (
        match digits line (j + 5) with
        | Some (_, k)
          when starts_with ~prefix:" (" (drop k line)
               && k + 2 < String.length line
               && is_digit line.[k + 2] ->
            true
        | _ -> go (j + 1))
  in
  go 0

let kinds =
  [ "parse"; "typing"; "annotation"; "pretyping"; "compilation"; "internal" ]

(* A message without the compiler's kind prefix: [parse error:] and the rest. *)
let strip_kind s =
  match
    List.find_map
      (fun kind ->
        let prefix = kind ^ " error" in
        if starts_with ~prefix s then Some (drop (String.length prefix) s)
        else None)
      kinds
  with
  | None -> s
  | Some rest ->
      let rest = if starts_with ~prefix:":" rest then drop 1 rest else rest in
      spaces rest

(* One spacing and no kind prefix, except that a message that is only its
   prefix ([parse error]) is kept: the release records some that way. *)
let normalise text =
  let text = collapse text in
  let bare = strip_kind text in
  if bare <> "" then bare else text

let word kind =
  if ends_with ~suffix:" error" kind then
    String.sub kind 0 (String.length kind - 6)
  else kind

let is_pass_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = ' '

(* The leading [name:] of a text, letters and spaces only. *)
let leading_name t =
  let n = String.length t in
  let rec go i = if i < n && is_pass_char t.[i] then go (i + 1) else i in
  let i = go 0 in
  if i > 0 && i < n && t.[i] = ':' then Some (String.sub t 0 i, drop (i + 1) t)
  else None

(* The pass a message names: the word before its first colon, after an
   [in function f:] that comes first. *)
let sub_of text =
  let t =
    if starts_with ~prefix:"in function " text then
      match index_from text 12 ": " with
      | Some j -> drop (j + 2) text
      | None -> text
    else text
  in
  match leading_name t with Some (name, _) -> name | None -> ""

(* ------------------------------------------------------------------ *)
(* The three readers *)

type first = { line : int; column : int; kind : string; message : string }

let nothing = { line = 0; column = 0; kind = ""; message = "" }

(* The first error [jasmin-lsp check] prints, [path:line:column: kind:
   message], with a zero-based column. Warnings are not errors. *)
let checker_first out =
  let located line =
    let n = String.length line in
    let rec scan i =
      if i >= n then None
      else if line.[i] = ':' then
        match digits line (i + 1) with
        | Some (l, j) when j < n && line.[j] = ':' -> (
            match digits line (j + 1) with
            | Some (c, k) when k + 1 < n && line.[k] = ':' && line.[k + 1] = ' '
              ->
                Some (l, c, drop (k + 2) line)
            | _ -> scan (i + 1))
        | _ -> scan (i + 1)
      else scan (i + 1)
    in
    scan 0
  in
  let rec go = function
    | [] -> nothing
    | line :: rest -> (
        match located line with
        | None -> go rest
        | Some (l, c, rest_text) ->
            let kind =
              match String.index_opt rest_text ':' with
              | Some i -> String.sub rest_text 0 i
              | None -> rest_text
            in
            if kind = "warning" then go rest
            else
              let message =
                if String.length rest_text >= String.length kind + 2 then
                  drop (String.length kind + 2) rest_text
                else ""
              in
              let message =
                replace_all
                  (replace_all
                     (replace_all message ~sub:"\\n" ~by:" ")
                     ~sub:"\\t" ~by:" ")
                  ~sub:"\\\\" ~by:"\\"
              in
              { line = l; column = c - 1; kind; message = collapse message })
  in
  go (String.split_on_char '\n' out)

(* A compiler location line, ["file", line 3 (2-15):] or with [to line 4
   (1)]: the line, the column and the text after it. *)
let location_line line =
  if not (starts_with ~prefix:"\"" line) then None
  else
    match String.index_from_opt line 1 '"' with
    | None -> None
    | Some q -> (
        let parse_range i =
          match digits line i with
          | Some (_, j) when j < String.length line && line.[j] = '-' -> (
              match digits line (j + 1) with
              | Some (_, k) -> Some k
              | None -> None)
          | Some (_, j) -> Some j
          | None -> None
        in
        let rest = drop (q + 1) line in
        if not (starts_with ~prefix:", line " rest) then None
        else
          let i = q + 1 + 7 in
          match digits line i with
          | Some (l, j) when starts_with ~prefix:" (" (drop j line) -> (
              match digits line (j + 2) with
              | Some (c, _) -> (
                  match parse_range (j + 2) with
                  | Some k when k < String.length line && line.[k] = ')' ->
                      let k = k + 1 in
                      let k =
                        if starts_with ~prefix:" to line " (drop k line) then
                          match digits line (k + 9) with
                          | Some (_, m)
                            when starts_with ~prefix:" (" (drop m line) -> (
                              match parse_range (m + 2) with
                              | Some e
                                when e < String.length line && line.[e] = ')' ->
                                  e + 1
                              | _ -> -1)
                          | _ -> -1
                        else k
                      in
                      if k > 0 && k < String.length line && line.[k] = ':' then
                        let at = Option.get (index_from line 0 "):") in
                        Some (l, c, drop (at + 2) line)
                      else None
                  | _ -> None)
              | None -> None)
          | _ -> None)

let compiler_kind message =
  let candidates =
    List.concat_map
      (fun kind -> [ "internal " ^ kind ^ " error"; kind ^ " error" ])
      [ "parse"; "typing"; "annotation"; "pretyping"; "compilation" ]
    @ [ "typing"; "unexpected annotation" ]
  in
  match
    List.find_opt (fun prefix -> starts_with ~prefix message) candidates
  with
  | Some kind -> kind
  | None -> ""

(* The first error of a compiler log, with every occurrence of the corpus
   root removed from its message so both sides name a file alike. *)
let compiler_first ~root log =
  let found = ref None and other = ref None and group = ref None in
  let close () =
    match !group with
    | None -> ()
    | Some (l, c, message) ->
        group := None;
        if
          !found = None
          && (not (starts_with ~prefix:"warning" message))
          && not (is_blank message)
        then
          let message =
            replace_all (collapse message) ~sub:(root ^ "/") ~by:""
          in
          let kind = compiler_kind message in
          found :=
            Some { line = l; column = c; kind; message = strip_kind message }
  in
  List.iter
    (fun line ->
      match location_line line with
      | Some (l, c, text) ->
          close ();
          let rec trim t =
            if t <> "" && (t.[0] = ' ' || t.[0] = '\t') then trim (drop 1 t)
            else t
          in
          group := Some (l, c, trim text)
      | None -> (
          match !group with
          | Some (l, c, message) ->
              if is_blank line then close ()
              else group := Some (l, c, message ^ " " ^ line)
          | None ->
              if !other = None && not (is_blank line) then other := Some line))
    (String.split_on_char '\n' log);
  close ();
  match (!found, !other) with
  | Some first, _ -> first
  | None, Some text -> { nothing with message = collapse text }
  | None, None -> nothing

type record = {
  r_line : int;
  r_column : int;
  r_message : string;
  r_kind : string;
  r_sub : string;
  r_bare : bool;
  r_beyond : bool;
      (** The record names a pass after the checkpoint, read from its text. *)
}

(* The release's [negative.expected]: per file, one block per architecture
   it is checked on, in that order. *)
let read_records text =
  let table = Hashtbl.create 256 in
  let add path record =
    let known = Option.value ~default:[] (Hashtbl.find_opt table path) in
    Hashtbl.replace table path (known @ [ record ])
  in
  let finish path (l, c, message) =
    let message = collapse message in
    let plain =
      match index_from message 0 " in function " with
      | Some i -> (
          match String.index_from_opt message (i + 13) ':' with
          | Some j -> String.sub message 0 i ^ ":" ^ drop (j + 1) message
          | None -> message)
      | None -> message
    in
    let kind, sub, rest =
      let compilation =
        List.find_opt
          (fun prefix -> starts_with ~prefix plain)
          [ "internal compilation error"; "compilation error" ]
      in
      match compilation with
      | Some kind -> (
          let rest = drop (String.length kind) plain in
          let rest =
            if starts_with ~prefix:":" rest then spaces (drop 1 rest) else rest
          in
          match leading_name rest with
          | Some (sub, after) -> (kind, sub, spaces after)
          | None -> (kind, "", rest))
      | None -> (
          match
            List.find_opt
              (fun prefix -> starts_with ~prefix plain)
              [
                "parse error";
                "annotation error";
                "typing error";
                "pretyping error";
                "typing:";
                "unexpected annotation";
              ]
          with
          | Some kind ->
              let rest = drop (String.length kind) plain in
              let rest =
                if starts_with ~prefix:":" rest then drop 1 rest else rest
              in
              (kind, "", spaces rest)
          | None -> ("", "", plain))
    in
    add path
      {
        r_line = l;
        r_column = c;
        r_message = message;
        r_kind = kind;
        r_sub = sub;
        r_bare = kind <> "" && is_blank rest;
        r_beyond =
          kind = "internal compilation error"
          || kind = "compilation error" && sub <> "" && sub <> "param expansion";
      }
  in
  let path = ref "" and state = ref `Out in
  List.iter
    (fun line ->
      if line <> "" && line.[0] <> '"' && ends_with ~suffix:".jazz:" line then (
        (match !state with `Message m -> finish !path m | _ -> ());
        path := String.sub line 0 (String.length line - 1);
        state := `Head)
      else
        match !state with
        | `Out -> ()
        | `Head ->
            if is_blank line then ()
            else if starts_with ~prefix:"\"" line && has_location line then
              let at =
                Option.value
                  ~default:(String.length line - 2)
                  (index_from line 0 "):")
              in
              let head = String.sub line 0 (at + 1) in
              let l = Option.value ~default:0 (number_after head "line ") in
              let c = Option.value ~default:0 (number_after head "(") in
              state :=
                `Message (l, c, drop (min (String.length line) (at + 2)) line)
            else state := `Out
        | `Message ((l, c, m) as group) ->
            if is_blank line then (
              finish !path group;
              state := `Out)
            else state := `Message (l, c, m ^ " " ^ line))
    (String.split_on_char '\n' text);
  (match !state with `Message m -> finish !path m | _ -> ());
  table

let record_for table path arch =
  match Hashtbl.find_opt table path with
  | None | Some [] -> None
  | Some records ->
      let nth n = List.nth_opt records n in
      let pick =
        match (List.length records, arch) with
        | 1, _ -> nth 0
        | _, "x86-64" -> nth 0
        | _, "arm-m4" -> nth 1
        | n, "riscv" when n >= 3 -> nth 2
        | _ -> nth 0
      in
      pick

(* ------------------------------------------------------------------ *)
(* One check *)

type row = {
  index : int;
  group : string;
  path : string;
  arch : string;
  label : string;
  c_status : int;
  c : first;
  o_status : int;
  o : first;
  raw : string list;  (** the checker's two streams and the compiler's log *)
}

let run_check ~root ~checker ~oracle ~records (index, (check : H.check)) =
  let output =
    H.run ~cwd:root checker [ "check"; "--arch"; check.arch; "--"; check.path ]
  in
  let o_status, o, log =
    match oracle with
    | Some compiler ->
        let log =
          H.run ~merge:true compiler
            [
              "-arch";
              check.arch;
              "-until_cstexp";
              Filename.concat root check.path;
            ]
        in
        (log.H.status, compiler_first ~root log.H.out, log.H.out)
    | None -> (
        match record_for records check.path check.arch with
        | Some r when not r.r_beyond ->
            ( 1,
              {
                line = r.r_line;
                column = r.r_column;
                kind = r.r_kind;
                message = r.r_message;
              },
              "" )
        | _ -> (0, nothing, ""))
  in
  {
    index;
    group = H.group_of check.path;
    path = check.path;
    arch = check.arch;
    label = check.label;
    c_status = output.H.status;
    c = checker_first output.H.out;
    o_status;
    o = { o with message = normalise o.message };
    raw = [ output.H.out; output.H.err; log ];
  }

(* ------------------------------------------------------------------ *)
(* The report *)

let ordered_groups rows =
  List.fold_left
    (fun acc r -> if List.mem r.group acc then acc else acc @ [ r.group ])
    [] rows

let report ~rows ~records ~from_record ~reference ~version ~checker ~corpus
    ~pending =
  let ref_ = if from_record then "the record" else "the reference" in
  let groups = ordered_groups rows in
  let counts = Hashtbl.create 64 in
  let bump key =
    Hashtbl.replace counts key
      (1 + Option.value ~default:0 (Hashtbl.find_opt counts key))
  in
  let get key = Option.value ~default:0 (Hashtbl.find_opt counts key) in
  let total key = List.fold_left (fun sum g -> sum + get (g, key)) 0 groups in
  let noted = Hashtbl.create 64 and notes = ref [] in
  let note r text =
    if not (Hashtbl.mem noted r.index) then (
      Hashtbl.add noted r.index ();
      notes := (r.group, text) :: !notes)
  in
  let broken = ref [] and breaks = ref [] in
  let where r = Printf.sprintf "%s (%s)" r.path r.arch in
  let same_first r =
    r.c.line = r.o.line && r.c.column = r.o.column && r.c.message = r.o.message
  in
  let boundary_line r =
    if r.o_status = 0 then
      Printf.sprintf "%s: %s accepts; the checker reports %s at %d:%d: %s"
        (where r) ref_ r.c.kind r.c.line (r.c.column + 1) r.c.message
    else if r.c_status = 0 then
      Printf.sprintf "%s: %s rejects at %d:%d (%s); the checker accepts"
        (where r) ref_ r.o.line (r.o.column + 1) r.o.message
    else
      Printf.sprintf
        "%s: %s rejects at %d:%d (%s); the checker rejects at %d:%d (%s)"
        (where r) ref_ r.o.line (r.o.column + 1) r.o.message r.c.line
        (r.c.column + 1) r.c.message
  in
  let complete =
    List.filter
      (fun r ->
        if r.o_status <> 0 && r.o_status <> 1 then (
          broken :=
            Printf.sprintf "%s: the reference exited %d" (where r) r.o_status
            :: !broken;
          false)
        else if r.c_status <> 0 && r.c_status <> 1 then (
          broken :=
            Printf.sprintf "%s: the checker exited %d" (where r) r.c_status
            :: !broken;
          false)
        else true)
      rows
  in
  List.iter
    (fun r ->
      let g = r.group in
      let same = r.c_status = 0 = (r.o_status = 0) in
      bump (g, "checks");
      bump (g, if same then "agree" else "differ");
      let side = if r.o_status = 0 then "accepts" else "rejects" in
      bump (side, "checks");
      if same then (
        bump (side, "verdict");
        if r.c_status <> 0 && same_first r then bump (side, "diagnostic"));
      if if r.o_status = 0 then not same else (not same) || not (same_first r)
      then breaks := (side, r) :: !breaks;
      if r.o_status = 0 then (
        bump (g, "positive");
        if r.c_status = 0 then bump (g, "positive accepted")
        else (
          bump (g, "positive errored");
          note r
            (Printf.sprintf
               "%s: %s accepts; the checker reports %s at %d:%d: %s" (where r)
               ref_ r.c.kind r.c.line (r.c.column + 1) r.c.message)))
      else (
        bump (g, "negative");
        if r.c_status <> 0 then
          if r.o.line = 0 then (
            bump (g, "unlocated");
            note r
              (Printf.sprintf
                 "%s: %s's first error carries no location; the checker \
                  reports %s at %d:%d"
                 (where r) ref_ r.c.kind r.c.line (r.c.column + 1)))
          else if r.c.line = r.o.line && r.c.column = r.o.column then
            bump (g, "same")
          else (
            bump (g, "different");
            note r
              (Printf.sprintf
                 "%s: %s's first error is at %d:%d; the checker's is at %d:%d"
                 (where r) ref_ r.o.line (r.o.column + 1) r.c.line
                 (r.c.column + 1)))
        else (
          bump (g, "negative accepted");
          note r
            (Printf.sprintf "%s: %s rejects at %d:%d (%s); the checker accepts"
               (where r) ref_ r.o.line (r.o.column + 1) r.o.message))))
    complete;
  (* The release's record of each check that names one, compared where the
     reference gives that record at the checkpoint. *)
  let apart = ref [] and unrecorded = ref [] in
  let location_fail = ref [] and kind_fail = ref [] and message_fail = ref [] in
  let recorded = ref 0
  and r_location = ref 0
  and r_message = ref 0
  and r_kind = ref 0 in
  let named = ref 0 in
  List.iter
    (fun r ->
      match record_for records r.path r.arch with
      | None ->
          if r.o_status = 1 && r.group = "fail" then
            unrecorded :=
              Printf.sprintf
                "%s: the release records no first diagnostic for it" (where r)
              :: !unrecorded
      | Some p ->
          let located = r.o.line = p.r_line && r.o.column = p.r_column in
          let kind_ok =
            if p.r_kind = "" then r.o.message = normalise p.r_message
            else
              word p.r_kind = word r.o.kind
              && (p.r_sub = "" || p.r_sub = sub_of r.o.message)
          in
          if not (r.o_status = 1 && located && kind_ok) then (
            if from_record && r.o_status = 0 then incr named;
            let told =
              if r.o_status = 0 then
                "the reference accepts the check at the checkpoint"
              else
                Printf.sprintf "the reference gives %d:%d, %s" r.o.line
                  (r.o.column + 1) r.o.message
            in
            apart :=
              Printf.sprintf "%s: the release records %s at %d:%d; %s" (where r)
                p.r_message p.r_line (p.r_column + 1) told
              :: !apart)
          else (
            incr recorded;
            if r.c.line = p.r_line && r.c.column = p.r_column then
              incr r_location
            else (
              location_fail :=
                Printf.sprintf "%s: recorded at %d:%d, the checker gives %s"
                  (where r) p.r_line (p.r_column + 1)
                  (if r.c_status = 0 then "no error"
                   else Printf.sprintf "%d:%d" r.c.line (r.c.column + 1))
                :: !location_fail;
              note r
                (Printf.sprintf
                   "%s: the release's recorded location is not reproduced"
                   (where r)));
            if p.r_kind <> "" && p.r_bare then
              if r.c_status <> 0 && word r.c.kind = word r.o.kind then
                incr r_kind
              else (
                kind_fail :=
                  Printf.sprintf
                    "%s: the release records %s; the reference prints %s, the \
                     checker prints %s"
                    (where r) p.r_kind r.o.kind
                    (if r.c_status = 0 then "no error" else r.c.kind)
                  :: !kind_fail;
                note r
                  (Printf.sprintf
                     "%s: the checker's kind is not the reference's" (where r)))
            else if r.c.message = normalise p.r_message then incr r_message
            else (
              message_fail :=
                Printf.sprintf "%s: recorded %s | the checker gives %s"
                  (where r) (normalise p.r_message) r.c.message
                :: !message_fail;
              note r
                (Printf.sprintf
                   "%s: the release's recorded message is not reproduced"
                   (where r)))))
    complete;
  let files =
    List.length (List.sort_uniq compare (List.map (fun r -> r.path) rows))
  in
  Printf.printf
    "# parity\ncorpus    %s\nchecker   %s\noracle    %s\nversion   %s\n" corpus
    checker
    (if from_record then "none; the release's negative.expected stands in"
     else reference)
    version;
  Printf.printf "checks    %d over %d files\n" (List.length rows) files;
  Printf.printf
    "excluded  %d .jazz files under pending/, which the release does not run\n\n"
    pending;
  let table title header keys =
    Printf.printf "== %s ==\n%-20s" title "group";
    List.iter (fun h -> Printf.printf " %10s" h) header;
    print_newline ();
    List.iter
      (fun g ->
        if get (g, List.hd keys) > 0 then (
          Printf.printf "%-20s" g;
          List.iter (fun k -> Printf.printf " %10d" (get (g, k))) keys;
          print_newline ()))
      groups
  in
  table
    (Printf.sprintf "the positive corpus: every check %s accepts" ref_)
    [ "checks"; "accepted"; "errored" ]
    [ "positive"; "positive accepted"; "positive errored" ];
  Printf.printf
    "positive checks: %d; the checker accepts %d; with an error %d\n\n"
    (total "positive")
    (total "positive accepted")
    (total "positive errored");
  table "the negative corpus: the first diagnostic's location"
    [ "checks"; "same"; "different"; "accepted"; "unlocated" ]
    [ "negative"; "same"; "different"; "negative accepted"; "unlocated" ];
  Printf.printf
    "negative checks: %d; the same line and column %d; a different one %d; the \
     checker accepts %d\n\n"
    (total "negative") (total "same") (total "different")
    (total "negative accepted");
  table "verdicts up to the end of preprocessing"
    [ "checks"; "agree"; "differ" ]
    [ "checks"; "agree"; "differ" ];
  Printf.printf "verdicts: %d checks; the same verdict %d; a different one %d\n"
    (total "checks") (total "agree") (total "differ");
  Printf.printf
    "recorded first diagnostic: %d compared, inside the checkpoint; by the \
     checker: location %d, message %d, kind %d\n"
    !recorded !r_location !r_message !r_kind;
  Printf.printf
    "records of a pass after the checkpoint, or not given there, not compared: \
     %d\n\n"
    (List.length !apart);
  Printf.printf "== the split by the reference's verdict ==\n";
  Printf.printf "%-24s %8s %22s %22s\n" "" "checks" "same verdict"
    "same first diagnostic";
  Printf.printf "%-24s %8d %22s %22s\n" "the reference rejects"
    (get ("rejects", "checks"))
    (H.share (get ("rejects", "verdict")) (get ("rejects", "checks")))
    (H.share (get ("rejects", "diagnostic")) (get ("rejects", "checks")));
  Printf.printf "%-24s %8d %22s %22s\n\n" "the reference accepts"
    (get ("accepts", "checks"))
    (H.share (get ("accepts", "verdict")) (get ("accepts", "checks")))
    "-";
  if from_record then
    Printf.printf
      "No checkpoint: the reference is the release's record of a whole \
       compilation; %d accepted checks have a record of a later pass.\n\n"
      !named;
  let section title lines =
    if lines <> [] then (
      Printf.printf "== %s ==\n" title;
      List.iter (Printf.printf "  %s\n") lines;
      print_newline ())
  in
  let breaks = List.rev !breaks in
  section
    (Printf.sprintf "checks %s rejects that do not match" ref_)
    (List.filter_map
       (fun (side, r) ->
         if side = "rejects" then Some (boundary_line r) else None)
       breaks);
  section
    (Printf.sprintf "checks %s accepts that the checker rejects" ref_)
    (List.filter_map
       (fun (side, r) ->
         if side = "accepts" then Some (boundary_line r) else None)
       breaks);
  let notes = List.rev !notes in
  Printf.printf "== differences ==\n";
  if notes = [] then print_endline "none"
  else
    List.iter
      (fun g ->
        List.iter
          (fun (g', text) -> if g = g' then Printf.printf "  %s\n" text)
          notes)
      groups;
  print_newline ();
  section "recorded locations the checker does not give"
    (List.rev !location_fail);
  section "recorded kinds the checker does not give" (List.rev !kind_fail);
  section "recorded messages the checker does not give" (List.rev !message_fail);
  section
    "records not compared: of a pass after the checkpoint, or not given there"
    (List.rev !apart);
  section "negative cases the release records no first diagnostic for"
    (List.rev !unrecorded);
  section "checks that did not complete" (List.rev !broken);
  let differences = List.length notes and broken = List.length !broken in
  if differences = 0 && broken = 0 then (
    print_endline "parity: every check agrees";
    true)
  else (
    Printf.printf "parity: %d differing checks, %d that did not complete\n"
      differences broken;
    false)

(* ------------------------------------------------------------------ *)
(* The run *)

let folders = [ "success"; "fail"; "sct-checker"; "safety"; "exec"; "warning" ]

let run (args : H.args) =
  H.name := "parity";
  let corpus = H.directory (Option.value ~default:"" (args.value "--corpus")) in
  let root =
    let tests = Filename.concat corpus "compiler/tests" in
    if Sys.file_exists tests && Sys.is_directory tests then tests else corpus
  in
  let checker =
    match args.value "--checker" with
    | Some path -> H.executable ~what:"--checker" path
    | None -> H.built "lsp-server/native/main.exe"
  in
  let expected = Filename.concat root "negative.expected" in
  let records =
    if Sys.file_exists expected then read_records (H.read_file expected)
    else Hashtbl.create 1
  in
  let oracle, version =
    match args.value "--jasminc" with
    | Some _ as value ->
        let path, version = H.compiler value in
        (Some path, version)
    | None ->
        if not (Sys.file_exists expected) then
          H.die "no --jasminc, and no %s: nothing to compare" expected;
        (None, "the release record, negative.expected")
  in
  let in_root path =
    if starts_with ~prefix:(root ^ "/") path then
      drop (String.length root + 1) path
    else path
  in
  let checks =
    match args.value "--checks" with
    | Some list ->
        List.map
          (fun (c : H.check) -> { c with path = in_root c.path })
          (H.read_checks list)
    | None ->
        List.concat_map
          (fun folder ->
            let dir = Filename.concat root folder in
            if Sys.file_exists dir && Sys.is_directory dir then
              List.concat_map
                (fun file ->
                  let path = folder ^ "/" ^ file in
                  let label =
                    if H.group_of path = "fail" then "reject" else "accept"
                  in
                  List.map
                    (fun arch -> { H.label; path; arch })
                    (H.arches_of path))
                (H.files dir)
            else [])
          folders
  in
  if checks = [] then
    H.die "no checks: the corpus has none of %s" (String.concat " " folders);
  let pending =
    let dir = Filename.concat root "pending" in
    if Sys.file_exists dir then List.length (H.files dir) else 0
  in
  let results =
    H.parallel ~jobs:(H.jobs args)
      (run_check ~root ~checker ~oracle ~records)
      (List.mapi (fun i c -> (i + 1, c)) checks)
  in
  let rows =
    List.map
      (function
        | Ok row -> row | Error why -> H.die "a check wrote no result: %s" why)
      results
  in
  Option.iter
    (fun file ->
      H.write_file file
        (String.concat ""
           (List.map
              (fun r ->
                Printf.sprintf
                  "%d\t%s\t%s\t%s\t%s\t%d\t%d\t%d\t%s\t%s\t%d\t%d\t%d\t%s\t%s\n"
                  r.index r.group r.path r.arch r.label r.c_status r.c.line
                  r.c.column r.c.kind r.c.message r.o_status r.o.line r.o.column
                  r.o.message r.o.kind)
              rows)))
    (args.value "--tsv");
  let reference = Option.value ~default:"negative.expected" oracle in
  let agrees =
    report ~rows ~records ~from_record:(oracle = None) ~reference ~version
      ~checker ~corpus:root ~pending
  in
  Option.iter
    (fun dir ->
      if not agrees then (
        if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
        List.iter
          (fun r ->
            List.iteri
              (fun i text ->
                if text <> "" then
                  H.write_file
                    (Filename.concat dir
                       (Printf.sprintf "check-%d.%s" r.index
                          (List.nth [ "c"; "c.err"; "o" ] i)))
                    text)
              r.raw)
          rows))
    (args.value "--keep");
  if agrees then (
    H.say "every check agrees with %s" reference;
    exit 0)
  else (
    H.say "the corpus does not agree with %s" reference;
    exit 1)

(* ------------------------------------------------------------------ *)
(* The readers, on the shapes the two tools and the record print. *)

let selftest () =
  let failures = ref 0 in
  let check name ok =
    if not ok then (
      incr failures;
      Printf.printf "FAIL %s\n" name)
  in
  check "checker: first error, column zero-based, escapes read"
    (checker_first
       "a.jazz:3:5: warning: unused\n\
        a.jazz:4:2: typing: bad\\ntype   here\n\
        a.jazz: 1 error\n"
    = { line = 4; column = 1; kind = "typing"; message = "bad type here" });
  check "checker: no error" (checker_first "a.jazz: ok\n" = nothing);
  check "compiler: a range, a continuation, the root removed"
    (compiler_first ~root:"/c"
       "\"/c/a.jazz\", line 1 (0) to line 4 (1): warning: w\n\n\
        \"/c/a.jazz\", line 3 (2-15):\n\
        typing error: in /c/b.jinc\n\
       \  more\n"
    = {
        line = 3;
        column = 2;
        kind = "typing error";
        message = "in b.jinc more";
      });
  check "compiler: no location"
    (compiler_first ~root:"/c" "\nFatal error: x\n"
    = { nothing with message = "Fatal error: x" });
  check "normalise: prefix dropped, a bare kind kept"
    (normalise "parse error:  unexpected   token" = "unexpected token"
    && normalise "parse error" = "parse error");
  check "sub_of: the pass after a function"
    (sub_of "in function f: stack allocation: x" = "stack allocation");
  let records =
    read_records
      "fail/a.jazz:\n\n\
       \"fail/a.jazz\", line 2 (4-6): typing error: bad\n\
      \  more\n\n\
       fail/a.jazz:\n\n\
       \"fail/a.jazz\", line 3 (1-2):\n\
       compilation error: stack allocation:\n\n"
  in
  check "record: two blocks, by architecture"
    (match
       ( record_for records "fail/a.jazz" "x86-64",
         record_for records "fail/a.jazz" "arm-m4" )
     with
    | Some a, Some b ->
        a.r_line = 2 && a.r_column = 4 && a.r_kind = "typing error"
        && a.r_message = "typing error: bad more"
        && (not a.r_beyond)
        && b.r_sub = "stack allocation"
        && b.r_beyond && b.r_bare
    | _ -> false);
  if !failures = 0 then print_endline "parity: selftest ok"
  else (
    Printf.printf "parity: %d selftests failed\n" !failures;
    exit 1)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "--selftest" ] -> selftest ()
  | arguments ->
      run
        (H.parse_args ~usage ~flags:[]
           ~options:
             [
               "--corpus";
               "--checks";
               "--jasminc";
               "--checker";
               "--jobs";
               "--tsv";
               "--keep";
             ]
           arguments)
