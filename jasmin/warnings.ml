(** Warnings are printed, not returned.

    The library has no warning hook: [Utils.warning] prints through
    [Format.eprintf] to [Format.err_formatter]. During a check this module
    points that formatter at a buffer and parses back the location prefix
    the printer writes, so a warning becomes a value the engine can map to
    a range.

    The printer writes a location as [Location.pp_iloc] does, one line per
    entry of the stack: ["file", line N (a-b)] first, [from ...] for the
    entries above it, then [warning: ...] on a line of its own because the
    enclosing box is vertical. The single-line form
    ["file", line N (a-b): warning: ...] is accepted too. Only a line that
    starts with a quoted file name supplies a location, because a message
    can quote one, and a stacked location keeps the first entry, which is
    the one the warning is about.

    A warning with a dummy location prints the text only. It is returned
    with the file being checked and line 0, so the engine still knows which
    document it came from. *)

type t = {
  file : string;
      (** The file the printer named, or the file being checked when it
          named none. *)
  line : int;
      (** The compiler's own line number, which starts at 1; 0 when the
          printer wrote no location. *)
  start_column : int;  (** Bytes from the start of the line, as printed. *)
  end_column : int;
  message : string;
}

let with_capture (f : unit -> 'a) : 'a * string =
  let buffer = Buffer.create 256 in
  let add s = Buffer.add_string buffer s in
  let old = Format.pp_get_formatter_out_functions Format.err_formatter () in
  let redirect =
    {
      Format.out_string =
        (fun s position length -> add (String.sub s position length));
      out_flush = (fun () -> ());
      out_newline = (fun () -> add "\n");
      out_spaces = (fun count -> add (String.make (max 0 count) ' '));
      out_indent = (fun count -> add (String.make (max 0 count) ' '));
    }
  in
  Format.pp_set_formatter_out_functions Format.err_formatter redirect;
  let restore () =
    (* Put the formatter back before flushing: bytes that were queued on it
       before the capture belong on stderr, not in the buffer. *)
    Format.pp_set_formatter_out_functions Format.err_formatter old;
    Format.pp_print_flush Format.err_formatter ()
  in
  let result = Fun.protect ~finally:restore f in
  (result, Buffer.contents buffer)

let read_digits s index =
  let length = String.length s in
  let stop = ref index in
  while !stop < length && s.[!stop] >= '0' && s.[!stop] <= '9' do
    incr stop
  done;
  if !stop = index then None
  else
    match int_of_string_opt (String.sub s index (!stop - index)) with
    | Some value -> Some (value, !stop)
    | None -> None

let find_substring_from s sub from =
  let n = String.length s and m = String.length sub in
  let rec loop index =
    if index + m > n then None
    else if String.sub s index m = sub then Some index
    else loop (index + 1)
  in
  loop from

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let message_after prefix line =
  if starts_with ~prefix line then
    Some
      (String.sub line (String.length prefix)
         (String.length line - String.length prefix))
  else None

let strip_uid_suffix message =
  let length = String.length message in
  let index = ref length in
  while
    !index > 0 && message.[!index - 1] >= '0' && message.[!index - 1] <= '9'
  do
    decr index
  done;
  if !index < length && !index > 0 && message.[!index - 1] = '.' then
    String.sub message 0 (!index - 1)
  else message

(* The location prefix the printer writes at the start of a line,
   ["file", line N (a-b)]. The second component is the rest of the line,
   which is either empty or the [": warning: ..."] of the single-line form.

   The line must start with the quote: a message can quote a location, and
   a message line starts with [warning: ], so only a line that begins with
   a file name can supply one. The closing paren must be followed by the
   end of the line or by the single-line form's prefix, so a file name that
   itself contains ["", line N (a-b)] cannot pass itself off as a
   location. *)
let parse_location (line : string) :
    ((string * int * int * int) * string) option =
  if String.length line = 0 || line.[0] <> '"' then None
  else
    match find_substring_from line "\", line " 1 with
    | None -> None
    | Some marker -> (
        let file = String.sub line 1 (marker - 1) in
        let index = marker + String.length "\", line " in
        match read_digits line index with
        | None -> None
        | Some (number, index) -> (
            let spaced =
              index + 1 < String.length line
              && line.[index] = ' '
              && line.[index + 1] = '('
            in
            if not spaced then None
            else
              let index = index + 2 in
              match read_digits line index with
              | None -> None
              | Some (start_column, index) ->
                  let end_column, index =
                    if index < String.length line && line.[index] = '-' then
                      match read_digits line (index + 1) with
                      | Some (value, index) -> (value, index)
                      | None -> (start_column, index)
                    else (start_column, index)
                  in
                  if index >= String.length line || line.[index] <> ')' then
                    None
                  else
                    let rest =
                      String.sub line (index + 1)
                        (String.length line - index - 1)
                    in
                    if rest = "" || starts_with ~prefix:": warning: " rest then
                      Some ((file, number, start_column, end_column), rest)
                    else None))

(* The entries of a stacked location after the first are printed as
   [from ...], with or without the space the break consumes. *)
let is_continuation line =
  starts_with ~prefix:" from " line || starts_with ~prefix:"from " line

let parse ?(file = "") (text : string) : t list =
  let lines = String.split_on_char '\n' text in
  let make file line start_column end_column message =
    { file; line; start_column; end_column; message = strip_uid_suffix message }
  in
  let rec loop acc remaining =
    match remaining with
    | [] -> List.rev acc
    | line :: rest -> (
        match parse_location line with
        | Some (file, number, start_column, end_column) -> (
            match rest with
            | next :: rest' -> (
                match message_after "warning: " next with
                | Some message ->
                    loop
                      (make file number start_column end_column message :: acc)
                      rest'
                | None -> (
                    match message_after "warning: " line with
                    | Some message ->
                        loop
                          (make file number start_column end_column message
                          :: acc)
                          rest
                    | None -> loop acc rest))
            | [] -> loop acc [])
        | None -> (
            match message_after "warning: " line with
            | Some message -> loop (make "" 0 0 0 message :: acc) rest
            | None -> loop acc rest))
  in
  loop [] lines
