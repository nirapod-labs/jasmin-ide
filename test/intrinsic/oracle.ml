(* The native Jasmin compiler as the oracle of the arity comparison (arity.ml).

   The compiler checks a call in this order: the lvalues against the number of
   results of the instruction, then the arguments against its number of
   arguments, then the types. A call with a wrong count therefore gets a count
   message that names the number the compiler wants, and a call with the right
   count gets none. [answer] reads those numbers from the compiler's own
   messages, and says so when the compiler refuses the name instead:

     - "unknown primitive", or a size annotation the name does not take
       ("only accepts the following size annotations", "accepts no size
       annotation"), is a refusal.
     - The four names the compiler types by a rule of its own, before its
       table ([spill], [unspill], [randombytes], [declassify]), carry no
       instruction description and so no count. They are checked to be known
       to the compiler, and to draw no count message: [Rule].

   The reading is checked, not trusted. Once the counts are read, a call of
   exactly that shape is compiled and must draw no count message. When it does
   draw one, the reading is contradicted, and the arity is taken from the one
   shape among all those up to [bound] that the compiler accepts by its counts.
   A compiler that refuses the shape it has just named, that accepts no shape or
   more than one, whose message cannot be read, or that exits other than 0 or 1
   (a crash), stops the run with [Failed]. *)

type answer =
  | Described of { args : int; results : int }
      (** the instruction takes [args] arguments and gives [results] results *)
  | Refused of string  (** the compiler's own words for the refusal *)
  | Rule  (** typed by a rule of the compiler's own, with no description *)

exception Failed of string

let failf fmt = Printf.ksprintf (fun message -> raise (Failed message)) fmt

(* The release the vendored library is pinned to (vendor/jasmin.lock). The
   compiler must be the same release, or its counts are not the library's. *)
let release = "2026.09.0"

(* ------------------------------------------------------------------ *)
(* Text *)

let find text needle =
  let length = String.length text and width = String.length needle in
  let rec from index =
    if index + width > length then None
    else if String.equal (String.sub text index width) needle then Some index
    else from (index + 1)
  in
  from 0

let read_file path =
  In_channel.with_open_bin path (fun channel ->
      really_input_string channel (in_channel_length channel))

(* The count message [what] ("lvalues" or "arguments") carries, as the
   (provided, wanted) pair the compiler prints, or [None] when it has none. *)
let count_message text ~what =
  let prefix = Printf.sprintf "invalid number of %s, " what in
  match find text prefix with
  | None -> None
  | Some index -> (
      let start = index + String.length prefix in
      let rest = String.sub text start (String.length text - start) in
      try
        Some
          (Scanf.sscanf rest "%d provided instead of %d" (fun provided wanted ->
               (provided, wanted)))
      with Scanf.Scan_failure _ | Failure _ | End_of_file ->
        failf "the compiler's %s count message is not read: %S" what rest)

let refusal_markers =
  [
    "unknown primitive";
    "only accepts the following size annotations";
    "accepts no size annotation";
  ]

(* The first line of [text] that says the compiler refuses the name. *)
let refusal text =
  String.split_on_char '\n' text
  |> List.find_map (fun line ->
         if
           List.exists
             (fun marker -> Option.is_some (find line marker))
             refusal_markers
         then Some (String.trim line)
         else None)

(* ------------------------------------------------------------------ *)
(* The compiler *)

type t = {
  jasminc : string;
  dir : string;  (** where the probe programs are written *)
  cache : (string * string, answer) Hashtbl.t;
  mutable runs : int;  (** compiler runs so far, for the report *)
}

(* Checks the compiler answers as the pinned release. A compiler that cannot
   print its version, or prints another one, is not an oracle for this library,
   and the run stops before it is used. *)
let create ~jasminc ~dir =
  if not (Sys.file_exists jasminc) then failf "no compiler at %s" jasminc;
  let out = Filename.concat dir "version.out" in
  let status =
    Sys.command
      (Printf.sprintf "%s -version > %s 2>&1" (Filename.quote jasminc)
         (Filename.quote out))
  in
  let text = String.trim (read_file out) in
  Sys.remove out;
  if status <> 0 || Option.is_none (find text release) then
    failf "%s is not Jasmin %s: exit %d, %S" jasminc release status text;
  ({ jasminc; dir; cache = Hashtbl.create 4096; runs = 0 }, text)

(* One compiler run on [source] under [arch]. Exit status 0 and 1 are the
   compiler's two normal outcomes; anything else is a crash of the compiler,
   and its message is the failure. *)
let compile t ~arch source =
  t.runs <- t.runs + 1;
  let base = Filename.concat t.dir (Printf.sprintf "probe%05d" t.runs) in
  let file = base ^ ".jazz" and out = base ^ ".out" in
  Out_channel.with_open_bin file (fun channel ->
      Out_channel.output_string channel source);
  let status =
    Sys.command
      (Printf.sprintf "%s -arch %s %s > %s 2>&1" (Filename.quote t.jasminc)
         (Filename.quote arch) (Filename.quote file) (Filename.quote out))
  in
  let text = read_file out in
  Sys.remove file;
  Sys.remove out;
  if status <> 0 && status <> 1 then
    failf "the compiler exited %d on %s under %s, not with a typing result: %s"
      status file arch (String.trim text);
  text

(* A probe: [lvalues] lvalues and [arguments] arguments to the spelling
   [written] (the name after the [#], suffix included), in a function whose
   only variables are of type u64. *)
let source ~lvalues ~arguments ~written =
  let declarations =
    List.init lvalues (fun index ->
        Printf.sprintf "\n  reg u64 v%d;" (index + 1))
    |> String.concat ""
  in
  let targets =
    List.init lvalues (fun index -> Printf.sprintf "v%d" (index + 1))
    |> String.concat ", "
  in
  let arguments = List.init arguments (fun _ -> "x") |> String.concat ", " in
  Printf.sprintf
    "fn probe() -> reg u64 {\n\
    \  reg u64 x;%s\n\
    \  (%s) = #%s(%s);\n\
    \  return x;\n\
     }\n"
    declarations targets written arguments

let no_count text =
  count_message text ~what:"lvalues" = None
  && count_message text ~what:"arguments" = None

(* The largest count tried when every shape is tried. The table's largest
   instruction takes nine arguments, and no instruction gives more results than
   six; the bound leaves room above both. *)
let bound = 12

(* Every shape up to [bound], keeping the ones the compiler accepts by its
   counts. A name with one accepted shape has that arity, whatever order its
   checks run in. This is the fallback for a name whose count messages do not
   read as the fast reading expects: the compiler's rule for [swap] checks its
   arguments and then its lvalues, both with one message that says "arguments",
   so a miscount of lvalues reads as a miscount of arguments. *)
let by_grid t ~arch ~written =
  let accepted = ref [] in
  for results = 0 to bound do
    for arguments = 0 to bound do
      let text =
        compile t ~arch (source ~lvalues:results ~arguments ~written)
      in
      (match refusal text with
      | Some words -> failf "the compiler refuses %s: %s" written words
      | None -> ());
      if no_count text then accepted := (arguments, results) :: !accepted
    done
  done;
  match !accepted with
  | [ (arguments, results) ] -> Described { args = arguments; results }
  | [] -> failf "the compiler accepts no shape of %s" written
  | shapes ->
      failf "the compiler accepts %d shapes of %s, not one" (List.length shapes)
        written

(* The arity of one spelling, read from the compiler's count messages, and
   checked: the counts read must be accepted by the compiler. A spelling whose
   reading is contradicted falls back to [by_grid]. *)
let describe t ~arch ~written =
  let first = compile t ~arch (source ~lvalues:1 ~arguments:0 ~written) in
  match refusal first with
  | Some words -> Refused words
  | None ->
      (* The lvalues are checked first: a count message says how many the
         compiler wants, and with no message one lvalue is the number. *)
      let results, after_lvalues =
        match count_message first ~what:"lvalues" with
        | None -> (1, first)
        | Some (provided, wanted) ->
            if provided <> 1 then
              failf "the compiler counts %d lvalues of one" provided;
            if wanted = 1 then
              failf "the compiler wants one lvalue and also counts one";
            let text =
              compile t ~arch (source ~lvalues:wanted ~arguments:0 ~written)
            in
            (match refusal text with
            | Some words -> failf "the compiler refuses %s: %s" written words
            | None -> ());
            (wanted, text)
      in
      (* The arguments follow, and a call with no arguments is counted as
         zero: the argument check is reached once the lvalues pass. *)
      let arguments =
        match count_message after_lvalues ~what:"arguments" with
        | None -> 0
        | Some (provided, wanted) ->
            if provided <> 0 then
              failf "the compiler counts %d arguments of none" provided;
            wanted
      in
      (* The check: the counts read are the counts the compiler accepts. *)
      let exact =
        compile t ~arch (source ~lvalues:results ~arguments ~written)
      in
      (match refusal exact with
      | Some words -> failf "the compiler refuses %s: %s" written words
      | None -> ());
      if no_count exact then Described { args = arguments; results }
      else by_grid t ~arch ~written

(* A name the compiler types by a rule of its own: it must be known to the
   compiler and draw no count message. *)
let rule t ~arch ~written =
  let text = compile t ~arch (source ~lvalues:1 ~arguments:1 ~written) in
  (match refusal text with
  | Some words ->
      failf "the compiler does not know the rule name %s: %s" written words
  | None -> ());
  if not (no_count text) then
    failf "the compiler counts the rule name %s" written;
  Rule

(* The compiler's answer for one spelling. [rule] is true for the four names
   the compiler types by a rule; they are not probed for counts. Cached per
   architecture and spelling. *)
let answer t ~arch ~written ~rule:is_rule =
  match Hashtbl.find_opt t.cache (arch, written) with
  | Some answer -> answer
  | None ->
      let answer =
        if is_rule then rule t ~arch ~written else describe t ~arch ~written
      in
      Hashtbl.replace t.cache (arch, written) answer;
      answer
