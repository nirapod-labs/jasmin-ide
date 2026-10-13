(* The compiler's printed intrinsic list, read by running the compiler.

   The intrinsic harness and the completion harness both need the same reading
   of the same output: the seven groups [Help.show_intrinsics] writes, the
   names in them, and the bytes a compiler run answers. The reading lives here
   once, so a difference in the printer is seen by both measurements the same
   way. *)

(* ------------------------------------------------------------------ *)
(* The compiler's printed list *)

(* The seven headers [Help.show_intrinsics] writes, exactly as it writes them
   ([help.ml]). A compiler whose printer words them differently is not
   silently compared: the run stops and says which header it did not see. *)
let headers =
  [
    "Intrinsics accepting no size suffix:";
    "Intrinsics accepting one optional size suffix, e.g., “_64”:";
    "Intrinsics accepting one signed size suffix, e.g. “_s16” or “_u32”:";
    "Intrinsics accepting a zero/sign extend suffix, e.g., “_u32u16”:";
    "Intrinsics accepting one vector description suffix, e.g., “_4u64”:";
    "Intrinsics accepting two vector description suffixes, e.g., “_2u16_2u64”:";
    "Intrinsics accepting a flag setting suffix (i.e. “S”) and a condition \
     suffix (i.e. “cc”):";
  ]

exception Unparsable of string

let strip_cr line =
  let length = String.length line in
  if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1)
  else line

(* [entry_of line] is the name an entry line carries, and [None] for any other
   line. Format writes each entry as [" - NAME"] -- one space of box
   indentation, then the hyphen -- so the leading spaces are skipped before the
   hyphen is read. A header carries none, which is how the two are told apart. *)
let entry_of (line : string) =
  let length = String.length line in
  let rec start index =
    if index < length && line.[index] = ' ' then start (index + 1) else index
  in
  let first = start 0 in
  if first + 1 < length && line.[first] = '-' && line.[first + 1] = ' ' then
    Some (String.sub line (first + 2) (length - first - 2))
  else None

(* [groups_of_help text] is the compiler's printed list as (header, names)
   pairs in printed order, one per group. A line that is neither a header nor
   a " - name" entry is a printer this program does not know. *)
let groups_of_help (text : string) =
  let groups = ref [] and current = ref None and names = ref [] in
  let flush () =
    match !current with
    | None -> ()
    | Some header ->
        groups := (header, List.rev !names) :: !groups;
        names := []
  in
  String.split_on_char '\n' text
  |> List.iter (fun raw ->
         let line = strip_cr raw in
         if String.equal line "" then ()
         else
           match entry_of line with
           | Some name -> names := name :: !names
           | None ->
               flush ();
               current := Some line);
  flush ();
  List.rev !groups

let check_headers ~(arch : string) (groups : (string * string list) list) =
  if List.length groups <> List.length headers then
    raise
      (Unparsable
         (Printf.sprintf
            "%s: the compiler printed %d groups, not %d (headers: %s)" arch
            (List.length groups) (List.length headers)
            (String.concat " | " (List.map fst groups))));
  List.iter2
    (fun expected (header, _) ->
      if not (String.equal expected header) then
        raise
          (Unparsable
             (Printf.sprintf "%s: the compiler's header is %S, expected %S" arch
                header expected)))
    headers groups

(* ------------------------------------------------------------------ *)
(* Running the compiler *)

let scratch = ref []

let remember path =
  scratch := path :: !scratch;
  path

let read_file path =
  In_channel.with_open_bin path (fun channel -> In_channel.input_all channel)

let write_file path text =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel text)

let temp_suffix = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

(* [run ~compiler ~arguments] runs the compiler once and answers its status
   and the bytes it wrote on both streams. Its output goes to a file, which is
   read after the process is reaped, so a chatty compiler cannot fill a pipe
   nobody drains. *)
let run ~(compiler : string) ~(arguments : string list) =
  let log = remember (Filename.temp_file "intrinsics-run-" ".log") in
  let out =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let argv = Array.of_list (compiler :: arguments) in
  let pid = Unix.create_process compiler argv devnull out out in
  Unix.close out;
  Unix.close devnull;
  let status = snd (Unix.waitpid [] pid) in
  (temp_suffix status, read_file log)

let help_of ~(compiler : string) ~(arch : string) =
  match run ~compiler ~arguments:[ "-arch"; arch; "-help-intrinsics" ] with
  | 0, output -> output
  | status, output ->
      raise
        (Unparsable
           (Printf.sprintf "%s: -help-intrinsics exited %d: %s" arch status
              (String.trim output)))

(* The names a compiler prints for [arch], in the printed order, after the
   printer's seven headers are checked. *)
let names_of ~(compiler : string) ~(arch : string) =
  let groups = groups_of_help (help_of ~compiler ~arch) in
  check_headers ~arch groups;
  List.concat_map snd groups
