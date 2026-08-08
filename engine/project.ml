(* The project file, [jasmin.project]: an INI file with one optional
   [[jasmin]] section, one [key = value] per line, whole-line comments and
   repeated keys that accumulate in file order. It is found by walking up from
   the file being analysed, and every problem in it is reported at the line it
   is on rather than only in a log. *)

type t = {
  arch : Jide_jasmin.Adapter.arch option;
  call_conv : Jide_jasmin.Adapter.call_conv option;
  idirs : (string * string) list;  (** in the order the file gives them *)
  entries : string list;
}

let empty = { arch = None; call_conv = None; idirs = []; entries = [] }
let file_name = "jasmin.project"

type problem = { span : Jide_lang.Span.t; message : string }

type loaded = {
  path : string option;
  text : string;
  bom : int;
  project : t;
  problems : problem list;
}

(* No file: no path, no text, no setting and nothing wrong. *)
let absent = { path = None; text = ""; bom = 0; project = empty; problems = [] }
let blank_span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0

(* The same rule as the engine's: a byte-order mark is not part of the file's
   own text and every byte offset moves back by it. *)
let strip_bom text =
  let length = String.length text in
  if
    length >= 3
    && Char.code text.[0] = 0xEF
    && Char.code text.[1] = 0xBB
    && Char.code text.[2] = 0xBF
  then (3, String.sub text 3 (length - 3))
  else (0, text)

(* A blank within a line: a space, a tab, or a carriage return that no line
   terminator took. *)
let is_blank character = character = ' ' || character = '\t' || character = '\r'

(* The part of a line that is not blank at either end, as the offsets left.
   Every span below is taken from the result, so a problem covers the text it
   is about and never the whitespace round it. *)
let trim text start stop =
  let start = ref start and stop = ref stop in
  while !start < !stop && is_blank text.[!start] do
    incr start
  done;
  while !stop > !start && is_blank text.[!stop - 1] do
    decr stop
  done;
  (!start, !stop)

(* The content of every line as [start, stop) byte offsets: the line feed
   excluded, and a carriage return before it too. A text that ends in a line
   feed has a final empty line, which is blank and so passed over. *)
let line_bounds text =
  let length = String.length text in
  let rec loop index acc =
    if index > length then List.rev acc
    else
      match String.index_from_opt text index '\n' with
      | None -> List.rev ((index, length) :: acc)
      | Some stop ->
          let content_end =
            if stop > index && text.[stop - 1] = '\r' then stop - 1 else stop
          in
          loop (stop + 1) ((index, content_end) :: acc)
  in
  loop 0 []

(* The first [jasmin.project] in this directory or in the nearest directory
   above it. The filesystem root stops the walk, and it does so by itself:
   [Filename.dirname "/"] is "/". *)
let rec find_above directory =
  let candidate = Filename.concat directory file_name in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then None else find_above parent

(* The names of the values the file may hold. Their order is the order the
   message that refuses a bad one lists the accepted values in. *)
let arch_names = Jide_jasmin.Adapter.arch_names
let call_conv_names = [ "linux"; "windows" ]
let quoted names = String.concat ", " names

(* A path relative to the project file's directory, as an absolute one. Without
   a named file there is no directory to resolve against, and the interface
   says a relative path is then left as written rather than guessed against the
   process's working directory. *)
let resolve ~directory path =
  match directory with
  | None -> path
  | Some directory ->
      if Filename.is_relative path then Filename.concat directory path else path

(* One problem for a file that is there and could not be read: the text is
   empty, the defaults apply and the reader is told, which is what the
   interface promises. *)
let unreadable path exn =
  {
    path = Some path;
    text = "";
    bom = 0;
    project = empty;
    problems =
      [
        {
          span = blank_span;
          message = "cannot be read: " ^ Printexc.to_string exn;
        };
      ];
  }

(* A file that is not there is the ordinary case: the defaults apply and
   nothing is wrong. [In_channel.with_open_bin] raises [Sys_error] for that
   case and for every other read failure, so the two are told apart before
   the read, with [Sys.file_exists]. A file removed between the two calls is
   reported as unreadable, which is the honest answer: it was there when the
   project was looked for. *)
let load path =
  if not (Sys.file_exists path) then
    { path = None; text = ""; bom = 0; project = empty; problems = [] }
  else
    match In_channel.with_open_bin path In_channel.input_all with
    | text -> (
        match parse ~path text with
        | loaded -> { loaded with path = Some path }
        | exception exn -> unreadable path exn)
    | exception exn -> unreadable path exn

let diagnostics ~uri (loaded : loaded) =
  List.map
    (fun (problem : problem) ->
      Jide_lang.Diagnostic.make ~code:"project"
        ~severity:Jide_lang.Diagnostic.Error problem.span problem.message)
    loaded.problems
