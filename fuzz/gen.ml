(** Seeded generators for the two fuzzing surfaces.

    A case derives its own stream from the run's seed, the surface and the
    case's ordinal, so case 37 of seed 1 is the same input on every run, on
    any host, however long the run before it lasted. That is what makes a
    failing case replayable: ask for at least thirty-eight cases with the
    same seed. *)

type surface = Text | Edit

let tag = function Text -> 1 | Edit -> 2

type state = Random.State.t

let make ~(seed : int) ~(surface : surface) ~(case : int) : state =
  Random.State.make [| seed; tag surface; case |]

let int (st : state) bound = if bound <= 0 then 0 else Random.State.int st bound
let bool (st : state) = Random.State.bool st

let pick (st : state) (choices : 'a array) =
  choices.(int st (Array.length choices))

let between (st : state) low high =
  if high <= low then low else low + int st (high - low + 1)

let list (st : state) ~count (f : state -> 'a) : 'a list =
  List.init count (fun _ -> f st)

let chars = [| "a"; "b"; "c"; "p"; "q"; "s"; "t"; "x"; "y"; "z" |]

let identifier st =
  pick st chars ^ if bool st then string_of_int (int st 10) else ""

let int_types =
  [| "u8"; "u16"; "u32"; "u64"; "bool"; "int"; "word"; "sptr"; "ptr" |]

let keywords =
  [|
    "fn";
    "export";
    "inline";
    "param";
    "reg";
    "stack";
    "mem";
    "global";
    "require";
    "return";
    "if";
    "while";
    "for";
    "while";
    "else";
    "let";
    "in";
    "true";
    "false";
    "and";
    "or";
    "not";
    "sub";
    "ptr";
    "word";
    "eval";
    "call";
    "jmp";
    "jz";
    "jnz";
    "module";
    "type";
    "import";
    "from";
    "as";
  |]

let punctuation =
  [|
    "(";
    ")";
    "{";
    "}";
    "[";
    "]";
    ";";
    ",";
    "=";
    ":";
    "->";
    "+";
    "-";
    "*";
    "/";
    "&&";
    "||";
    "!";
    "#";
    "//";
    "/*";
    "*/";
    "\"";
    "\\";
    ".";
    "_";
    "@";
    "$";
    "?";
    "|";
    "&";
    "<";
    ">";
    "%";
    "~";
    "`";
    "'";
  |]

let blanks = [| " "; "  "; "\t"; "\n"; "\r\n"; "\n\n"; "\r"; "\t\t" |]

(** Text that is valid UTF-8 but not ASCII, a byte-order mark and a NUL: the
    three things a reader that assumes one byte per character trips over. *)
let exotic =
  [|
    "\xef\xbb\xbf";
    "\xc3\xa9";
    "\xe2\x82\xac";
    "\xf0\x9f\x98\x80";
    "\x00";
    "\xf4\x8f\xbf\xbf";
    "\x7f";
  |]

let junk st =
  let buffer = Buffer.create 64 in
  for _ = 1 to between st 1 24 do
    match int st 4 with
    | 0 -> Buffer.add_string buffer (pick st keywords)
    | 1 -> Buffer.add_string buffer (pick st punctuation)
    | 2 -> Buffer.add_string buffer (pick st blanks)
    | _ -> Buffer.add_string buffer (pick st exotic)
  done;
  Buffer.contents buffer

(** A program of the shape the checker accepts, with the identifiers, the
    types, the statement count and the spacing drawn from the stream. *)
let program st =
  let type_ = pick st int_types in
  let buffer = Buffer.create 256 in
  let name = "f" ^ identifier st in
  let first = identifier st in
  Buffer.add_string buffer
    (Printf.sprintf "fn %s(reg %s %s) -> reg %s {\n" name type_ first type_);
  let current = ref first in
  for _ = 1 to int st 4 do
    let local = identifier st ^ string_of_int (between st 0 99) in
    Buffer.add_string buffer
      (Printf.sprintf "%sreg %s %s;%s%s = %s;%s" (pick st blanks) type_ local
         (pick st blanks) local !current (pick st blanks));
    current := local
  done;
  Buffer.add_string buffer
    (Printf.sprintf "%sreturn %s;%s}%s" (pick st blanks) !current
       (pick st blanks) (pick st blanks));
  Buffer.contents buffer

(* One alteration of a text at a byte boundary. The bytes need not fall on a
   character boundary: a file on disk may hold anything, and the library has
   to survive being handed it. *)
let mutate_once st (text : string) : string =
  let length = String.length text in
  let at () = int st (max 1 length) in
  let width () = between st 1 16 in
  match int st 7 with
  | 0 when length > 0 ->
      let start = at () in
      let count = min (width ()) (length - start) in
      String.sub text 0 start
      ^ String.sub text (start + count) (length - start - count)
  | 1 ->
      let start = at () in
      String.sub text 0 start ^ junk st ^ String.sub text start (length - start)
  | 2 when length > 0 ->
      let start = at () in
      let count = min (width ()) (length - start) in
      String.sub text 0 start
      ^ String.sub text start count
      ^ String.sub text start (length - start)
  | 3 when length > 0 ->
      let start = at () in
      let count = min (width ()) (length - start) in
      String.sub text 0 start ^ junk st
      ^ String.sub text (start + count) (length - start - count)
  | 4 when length > 1 ->
      let start = at () in
      let stop = start + min (width ()) (length - start) in
      String.sub text stop (length - stop)
  | 5 when length > 0 ->
      let position = at () in
      let others = [| '\000'; '\n'; '{'; '}'; ';'; '\255'; '"'; '\t' |] in
      let replacement = pick st others in
      let bytes = Bytes.of_string text in
      Bytes.set bytes position replacement;
      Bytes.to_string bytes
  | _ -> text ^ junk st

let mutate st (text : string) : string =
  let rec apply remaining text =
    if remaining <= 0 then text else apply (remaining - 1) (mutate_once st text)
  in
  apply (int st 4) text

(** A source text: a program, junk, one of the two joined, and, when the run
    was given a corpus, an altered unit of it. *)
let source ?(corpus = [||]) st =
  if Array.length corpus > 0 && bool st then
    mutate st corpus.(int st (Array.length corpus))
  else
    match int st 4 with
    | 0 -> program st
    | 1 -> junk st
    | 2 -> program st ^ junk st
    | _ -> mutate st (program st)

(** Text for the edit surface: what a client may put in a document, including
    astral characters, NUL bytes, carriage returns and very long lines. *)
let document_text ?(corpus = [||]) st =
  match int st 6 with
  | 0 -> source ~corpus st
  | 1 ->
      String.concat ""
        (list st ~count:(between st 1 8) (fun st -> pick st exotic))
  | 2 ->
      let line =
        String.concat ""
          (list st ~count:(between st 1 40) (fun st -> pick st punctuation))
      in
      String.concat ""
        (list st ~count:(between st 1 6) (fun _ -> line ^ "\r\n"))
  | 3 -> String.make (between st 0 2000) (pick st [| 'a'; '\t'; '\n'; ' ' |])
  | 4 -> ""
  | _ ->
      mutate st
        (pick st
           [|
             program st;
             String.concat ""
               (list st ~count:(between st 1 30) (fun st -> pick st exotic));
           |])

let rec files_under directory =
  match Sys.readdir directory with
  | exception Sys_error _ -> []
  | entries ->
      Array.sort String.compare entries;
      Array.to_list entries
      |> List.concat_map (fun entry ->
             let path = Filename.concat directory entry in
             if Sys.is_directory path then files_under path
             else if Filename.check_suffix entry ".jazz" then [ path ]
             else [])

let read_capped path limit =
  match
    In_channel.with_open_bin path (fun channel -> In_channel.input_all channel)
  with
  | text ->
      Some
        (if String.length text > limit then String.sub text 0 limit else text)
  | exception Sys_error _ -> None

(** The `.jazz` units of a corpus, each capped, read once so that a case costs
    no filesystem work. *)
let corpus ?(max_files = 2000) ?(max_bytes = 65536) directory =
  let paths = files_under directory in
  let paths =
    if List.length paths > max_files then
      List.filteri (fun index _ -> index < max_files) paths
    else paths
  in
  Array.of_list (List.filter_map (fun path -> read_capped path max_bytes) paths)
