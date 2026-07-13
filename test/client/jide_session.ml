(* Session and fake-server scripts share one block format: blocks separated
   by blank lines, a block starting with '#' is a directive, anything else is
   one JSON-RPC message, which may be spread over several lines. *)

type step =
  | Message of Yojson.Safe.t
  | Sleep of int
  | Close
  | Expect_timeout of int

let words text =
  String.split_on_char ' ' text |> List.concat_map (String.split_on_char '\t')
  |> List.filter (fun word -> word <> "")

let parse_milliseconds name argument =
  match int_of_string_opt argument with
  | Some milliseconds when milliseconds >= 0 -> Ok milliseconds
  | Some _ | None ->
    Error (Printf.sprintf "%s needs a number of milliseconds, got %S" name argument)

let parse_directive text =
  match words text with
  | [ "#close" ] -> Ok Close
  | [ "#sleep"; argument ] -> Result.map (fun ms -> Sleep ms) (parse_milliseconds "#sleep" argument)
  | [ "#expect-timeout"; argument ] ->
    Result.map (fun ms -> Expect_timeout ms) (parse_milliseconds "#expect-timeout" argument)
  | [ name ] ->
    if name = "#sleep" || name = "#expect-timeout" then
      Error (Printf.sprintf "%s needs a number of milliseconds" name)
    else Error (Printf.sprintf "unknown directive %S" name)
  | name :: _ -> Error (Printf.sprintf "unknown directive %S" name)
  | [] -> Error "empty directive"

(* Blank lines separate blocks; a block keeps its own line breaks. *)
let blocks text =
  let rec go finished current = function
    | [] -> List.rev (if current = [] then finished else List.rev current :: finished)
    | line :: rest ->
      let line = if String.length line > 0 && line.[String.length line - 1] = '\r' then
          String.sub line 0 (String.length line - 1)
        else line
      in
      if String.trim line = "" then
        go (if current = [] then finished else List.rev current :: finished) [] rest
      else go finished (line :: current) rest
  in
  go [] [] (String.split_on_char '\n' text)

let parse_block block =
  let text = String.concat "\n" block in
  let trimmed = String.trim text in
  if String.length trimmed > 0 && trimmed.[0] = '#' then parse_directive trimmed
  else begin
    match Yojson.Safe.from_string text with
    | json -> Ok (Message json)
    | exception Yojson.Json_error message -> Error (Printf.sprintf "not JSON: %s" message)
  end

let parse_string text =
  let rec go steps = function
    | [] -> Ok (List.rev steps)
    | block :: rest -> (
      match parse_block block with
      | Error message -> Error message
      | Ok step -> go (step :: steps) rest)
  in
  go [] (blocks text)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let parse_file path =
  match read_file path with
  | text -> parse_string text
  | exception Sys_error message -> Error message

(* The fake server sends replies; it cannot ask the client to expect a
   timeout. *)
let check_script steps =
  if List.exists (function Expect_timeout _ -> true | _ -> false) steps then
    Error "a fake-server script cannot use the #expect-timeout directive"
  else Ok steps

let parse_script_string text = Result.bind (parse_string text) check_script
let parse_script_file path = Result.bind (parse_file path) check_script
