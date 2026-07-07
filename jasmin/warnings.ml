(** Warnings are printed, not returned.

    The library has no warning hook: [Utils.warning] prints through
    [Format.eprintf] to [Format.err_formatter]. During a check this module
    points that formatter at a buffer and parses back the location prefix
    the printer writes, so a warning becomes a value the engine can map to
    a range.

    The printer writes the location as [Location.pp_iloc] does, which is
    ["file", line N (a-b)] on one line and [warning: ...] on the next,
    because the enclosing box is vertical. A warning with a dummy location
    prints the text only and keeps no range. *)

type t = {
  file : string;
  line : int;
  start_column : int;
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
    Format.pp_print_flush Format.err_formatter ();
    Format.pp_set_formatter_out_functions Format.err_formatter old
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
  else Some (int_of_string (String.sub s index (!stop - index)), !stop)

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

