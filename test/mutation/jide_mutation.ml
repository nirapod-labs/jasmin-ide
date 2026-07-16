(** The mutation harness of the batch checker.

    One Jasmin file is altered in one place at a time: one token deleted, one
    token inserted, one token duplicated, [;] and [}] swapped, or the text cut
    at a token boundary. Every altered text goes through the checker, and the
    run reports the three properties a recovering front end has to keep:

    - no exception and no hang, in every case;
    - the outline of the items away from the edit unchanged, in every case
      whose altered text still lexes;
    - at most two errors per single-token mutation, in at least 95% of the
      single-token cases, with the error-count distribution reported.

    The tokens are the compiler's own: the harness drives the vendored lexer,
    so a token boundary is one the compiler would see. The outline is the
    sequence of item headers read from those tokens ([fn], [proc], [param],
    [global], [type] and [require]), each a keyword followed by the name the
    grammar puts there. Reading it from tokens rather than from a parse tree
    keeps it available for a text the parser refuses, which is the normal
    state of a mutated file; a text the lexer itself refuses has no outline
    and is counted as such rather than as a difference. *)

type token = { start : int; stop : int; kind : kind }

and kind =
  | Fn
  | Param
  | Global
  | Type
  | Require
  | Ident of string
  | Text of string
  | Lparen
  | Equals
  | Other

let kind_of (token : Jasmin.Parser.token) : kind option =
  match token with
  | Jasmin.Parser.EOF -> None
  | Jasmin.Parser.FN -> Some Fn
  | Jasmin.Parser.PARAM -> Some Param
  | Jasmin.Parser.GLOBAL -> Some Global
  | Jasmin.Parser.TYPE -> Some Type
  | Jasmin.Parser.REQUIRE -> Some Require
  | Jasmin.Parser.NID name -> Some (Ident name)
  | Jasmin.Parser.STRING text -> Some (Text text)
  | Jasmin.Parser.LPAREN -> Some Lparen
  | Jasmin.Parser.EQ -> Some Equals
  | _ -> Some Other

let lex (text : string) : token list option =
  let lexbuf = Lexing.from_string text in
  let rec loop acc =
    match Jasmin.Lexer.main lexbuf with
    | exception _ -> None
    | token -> (
        let start = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
        let stop = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
        match kind_of token with
        | None -> Some (List.rev acc)
        | Some kind -> loop ({ start; stop; kind } :: acc))
  in
  loop []

type item = { kind : string; name : string; start : int }
(** One item of the outline: what it is, what it is called and where its
    header starts in the text. *)

let outline_of_tokens (tokens : token list) : item list =
  let tokens = Array.of_list tokens in
  let length = Array.length tokens in
  let at index = if index < length then Some tokens.(index) else None in
  let header kind name index acc =
    ({ kind; name; start = tokens.(index).start } : item) :: acc
  in
  (* A declaration is [keyword type* name = ...], so the name is the last
     identifier before the equals sign. The search is bounded because the
     type is a short sequence and an unbounded one would read the rest of the
     file to find the next assignment. *)
  let rec name_before_equals index fuel =
    if fuel = 0 || index + 1 >= length then None
    else
      match (tokens.(index).kind, tokens.(index + 1).kind) with
      | Ident name, Equals -> Some name
      | _ -> name_before_equals (index + 1) (fuel - 1)
  in
  let rec declaration kind index acc =
    match name_before_equals (index + 1) 6 with
    | None -> loop acc (index + 1)
    | Some name -> loop (header kind name index acc) (index + 2)
  and loop acc index =
    if index >= length then List.rev acc
    else
      match (tokens.(index).kind, at (index + 1), at (index + 2)) with
      | Fn, Some { kind = Ident name; _ }, Some { kind = Lparen; _ } ->
          loop (header "fn" name index acc) (index + 3)
      | Param, _, _ -> declaration "param" index acc
      | Global, _, _ -> declaration "global" index acc
      | Type, _, _ -> declaration "type" index acc
      | Require, Some { kind = Text name; _ }, _ ->
          loop (header "require" name index acc) (index + 2)
      | _ -> loop acc (index + 1)
  in
  loop [] 0

let outline (text : string) : item list option =
  match lex text with
  | None -> None
  | Some tokens -> Some (outline_of_tokens tokens)

type mutation = { label : string; text : string; start : int; stop : int }
(** One alteration of one text. [start] and [stop] are the bytes of the
    original text the alteration replaces; an insertion has [stop = start]. *)

let splice text start stop inserted =
  String.sub text 0 start ^ inserted
  ^ String.sub text stop (String.length text - stop)

let token_text text (token : token) =
  String.sub text token.start (token.stop - token.start)

(** The tokens an insertion may put in. None of them is an item keyword, so
    an insertion can never add or remove an outline entry of its own: the
    outline is what the mutation must not disturb, not what it edits. *)
let insertions =
  [|
    ";";
    "=";
    "(";
    ")";
    "{";
    "}";
    "reg";
    "u64";
    "return";
    "x";
    ",";
    "[";
    "]";
    "+";
    "-";
    "/* edited */";
  |]

let deletion text (tokens : token array) state =
  let count = Array.length tokens in
  if count = 0 then []
  else
    let token = tokens.(Random.State.int state count) in
    [
      {
        label = "delete";
        text = splice text token.start token.stop "";
        start = token.start;
        stop = token.stop;
      };
    ]

let insertion text (tokens : token array) state =
  let count = Array.length tokens in
  if count = 0 then []
  else
    let token = tokens.(Random.State.int state count) in
    let inserted =
      insertions.(Random.State.int state (Array.length insertions))
    in
    let at = token.stop in
    [
      {
        label = "insert";
        text = splice text at at (" " ^ inserted);
        start = at;
        stop = at;
      };
    ]

(** A duplicated token is the one insertion that carries a name from the
    file: it is the compiler's own text for the token, put back beside it. *)
let duplication text (tokens : token array) state =
  let count = Array.length tokens in
  if count = 0 then []
  else
    let token = tokens.(Random.State.int state count) in
    let at = token.stop in
    [
      {
        label = "duplicate";
        text = splice text at at (" " ^ token_text text token);
        start = at;
        stop = at;
      };
    ]

let swap text (tokens : token array) state =
  let candidates =
    List.filter
      (fun (token : token) ->
        match token_text text token with ";" | "}" -> true | _ -> false)
      (Array.to_list tokens)
  in
  match candidates with
  | [] -> []
  | _ ->
      let token : token =
        List.nth candidates (Random.State.int state (List.length candidates))
      in
      let from = token_text text token in
      let to_ = if from = ";" then "}" else ";" in
      [
        {
          label = "swap";
          text = splice text token.start token.stop to_;
          start = token.start;
          stop = token.stop;
        };
      ]

(** Four cuts, at a fifth, two fifths, three fifths and four fifths of the
    tokens, each at the start of a token. *)
let truncations text (tokens : token array) =
  let count = Array.length tokens in
  if count = 0 then []
  else
    List.filter_map
      (fun fifth ->
        let index = count * fifth / 5 in
        if index >= count then None
        else
          let token : token = tokens.(index) in
          Some
            {
              label = "truncate";
              text = String.sub text 0 token.start;
              start = token.start;
              stop = String.length text;
            })
      [ 1; 2; 3; 4 ]

let mutations ~(seed : int) ~(text : string) ~(tokens : token list) :
    mutation list =
  let state = Random.State.make [| seed |] in
  let tokens = Array.of_list tokens in
  deletion text tokens state
  @ insertion text tokens state
  @ duplication text tokens state
  @ swap text tokens state @ truncations text tokens

let is_single_token (mutation : mutation) = mutation.label <> "truncate"

(* The criteria. *)

let contains ~fragment text =
  let fragment_length = String.length fragment in
  let text_length = String.length text in
  let rec loop index =
    index + fragment_length <= text_length
    && (String.sub text index fragment_length = fragment || loop (index + 1))
  in
  loop 0

(** The architecture the corpus's own test records choose: a directory named
    [arm-m4], [armv8a] or [risc-v] selects it and everything else is x86-64. *)
let arch_of_file path =
  let name =
    if contains ~fragment:"/arm-m4/" path then "arm-m4"
    else if contains ~fragment:"/armv8a/" path then "armv8a"
    else if contains ~fragment:"/risc-v/" path then "risc-v"
    else "x86-64"
  in
  match Jide_jasmin.Adapter.arch_of_string name with
  | Some arch -> arch
  | None -> Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")

let call_conv = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

exception Timeout

(** The per-case wall clock. It is far above a check of any corpus unit, so a
    case that reaches it is a hang rather than a slow file. *)
let timeout = 5.0

let with_timeout seconds f =
  let off = { Unix.it_interval = 0.0; it_value = 0.0 } in
  ignore (Unix.setitimer Unix.ITIMER_REAL { off with Unix.it_value = seconds });
  Fun.protect
    ~finally:(fun () -> ignore (Unix.setitimer Unix.ITIMER_REAL off))
    f

let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))

type case = {
  file : string;
  label : string;
  errors : int option;
      (** the error count, or [None] when the checker raised *)
  raised : string option;
  hung : bool;
  outline : [ `Same | `Different | `Unavailable ] option;
  single : bool;
}

type report = {
  seed : int;
  files : int;
  cases : int;
  raised : (string * string * string) list;  (** file, mutation, message *)
  hung : (string * string) list;
  outline_same : int;
  outline_different : (string * string) list;
  outline_unavailable : (string * string) list;
  histogram : (int * int) list;
  single_histogram : (int * int) list;
  within_two : int;
  single_within_two : int;
  single_over_two : (string * string * int) list;
  single_cases : int;
}

(** [touched_item items start] is the outline entry whose region holds the
    edit, where a region starts at the entry's header and runs to the next
    header. [None] means the edit sits before the first header, which is a
    part of no item. *)
let touched_item (items : item list) ~(start : int) : item option =
  match items with
  | [] -> None
  | first :: _ when start < first.start -> None
  | _ ->
      List.fold_left
        (fun candidate (item : item) ->
          if item.start <= start then Some item else candidate)
        None items

let signature (items : item list) =
  List.map (fun (item : item) -> (item.kind, item.name)) items

let rec is_subsequence (needle : (string * string) list)
    (haystack : (string * string) list) =
  match (needle, haystack) with
  | [], _ -> true
  | _, [] -> false
  | entry :: rest, candidate :: candidates ->
      if entry = candidate then is_subsequence rest candidates
      else is_subsequence needle candidates

let rec is_prefix (candidate : (string * string) list)
    (reference : (string * string) list) =
  match (candidate, reference) with
  | [], _ -> true
  | _ :: _, [] -> false
  | entry :: rest, expected :: expected_rest ->
      entry = expected && is_prefix rest expected_rest

(** The outline of the original away from the edit, against the outline of the
    altered text.

    A deletion, an insertion or a duplication edits one item: the item whose
    region holds the edit is the one the alteration is allowed to change.
    Every other entry must still be reported by the altered text's outline,
    with the same kind and the same name, in the same order. An entry may be
    added or dropped where the edit sits — an edited header still reads as an
    item when its keyword and its name survive, and stops reading as one when
    they do not — but nothing away from the edit may go missing or change.

    A cut leaves a prefix: the entries away from the edit are the ones the
    prefix still holds in full, so the altered outline must be the original's
    first entries. The entry the cut runs into has no complete header in the
    prefix and is the one it may lack.

    Two items that share a name are two entries here: the touched item is
    identified by where its header starts, not by its name. *)
let outline_away ~(items : item list) ~(mutated : item list option)
    ~(touched : item option) ~(start : int) ~(cut : bool) =
  match mutated with
  | None -> `Unavailable
  | Some mutated ->
      if cut then
        if is_prefix (signature mutated) (signature items) then `Same
        else `Different
      else
        let touched =
          match touched with
          | Some item -> Some item
          | None -> touched_item items ~start
        in
        let away =
          match touched with
          | None -> items
          | Some touched ->
              List.filter
                (fun (item : item) -> item.start <> touched.start)
                items
        in
        if is_subsequence (signature away) (signature mutated) then `Same
        else `Different

let errors_of_check ~file ~text =
  let result =
    Jide_jasmin.Adapter.check (arch_of_file file) call_conv ~file ~text
      ~idirs:[]
  in
  List.length
    (List.filter
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.severity = Jide_lang.Diagnostic.Error)
       result.Jide_jasmin.Adapter.diagnostics)

(* The check runs in a child process, once per case. Two reasons. A check that
   hangs can then be killed from outside; in this process the library's own
   handler catches every exception, so a check that never returns would be
   reported as one parse error instead of as a hang. And a check that dies on
   a signal is then one case's outcome instead of the harness's death. *)

type checked = { errors : int option; raised : string option; hung : bool }

let read_all (descriptor : Unix.file_descr) : string =
  let buffer = Buffer.create 64 in
  let bytes = Bytes.create 512 in
  let rec loop () =
    match Unix.read descriptor bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let write_all (descriptor : Unix.file_descr) (text : string) : unit =
  let bytes = Bytes.of_string text in
  let rec loop offset =
    if offset < Bytes.length bytes then
      match
        Unix.write descriptor bytes offset (Bytes.length bytes - offset)
      with
      | count -> loop (offset + count)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

(** What the child reports, one line, so that the message of an exception
    survives the process boundary. *)
let report_in_child ~(file : string) ~(text : string) : string =
  match errors_of_check ~file ~text with
  | errors -> Printf.sprintf "errors %d" errors
  | exception exn ->
      let message =
        String.map
          (fun character ->
            match character with '\n' | '\r' -> ' ' | character -> character)
          (Printexc.to_string exn)
      in
      Printf.sprintf "raised %s" message

let wait_for_child ~(seconds : float) (pid : int) =
  let wait () =
    match Unix.waitpid [] pid with
    | _, status -> `Status status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> raise Timeout
  in
  match with_timeout seconds wait with
  | outcome -> outcome
  | exception Timeout -> (
      (* The alarm can fire as the child exits, so ask again before calling the
         case a hang. *)
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ ->
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
          `Hung
      | _, status -> `Status status)

let measure ~(file : string) ~(text : string) : checked =
  let read_end, write_end = Unix.pipe () in
  match Unix.fork () with
  | exception Unix.Unix_error (code, _, _) ->
      Unix.close read_end;
      Unix.close write_end;
      {
        errors = None;
        raised = Some ("cannot fork: " ^ Unix.error_message code);
        hung = false;
      }
  | 0 ->
      Unix.close read_end;
      let line =
        match report_in_child ~file ~text with
        | line -> line
        | exception _ -> "raised an exception with no message"
      in
      (try write_all write_end line with Unix.Unix_error _ -> ());
      (try Unix.close write_end with Unix.Unix_error _ -> ());
      Unix._exit 0
  | pid -> (
      Unix.close write_end;
      let outcome = wait_for_child ~seconds:timeout pid in
      let line = try read_all read_end with Unix.Unix_error _ -> "" in
      Unix.close read_end;
      let no_report =
        {
          errors = None;
          raised = Some ("the check wrote no report (" ^ String.trim line ^ ")");
          hung = false;
        }
      in
      match outcome with
      | `Hung -> { errors = None; raised = None; hung = true }
      | `Status (Unix.WEXITED 0) -> (
          match String.split_on_char ' ' (String.trim line) with
          | [ "errors"; count ] -> (
              match int_of_string_opt count with
              | Some errors ->
                  { errors = Some errors; raised = None; hung = false }
              | None -> no_report)
          | "raised" :: message ->
              {
                errors = None;
                raised = Some (String.concat " " message);
                hung = false;
              }
          | _ -> no_report)
      | `Status (Unix.WEXITED code) ->
          {
            errors = None;
            raised = Some (Printf.sprintf "the check exited %d" code);
            hung = false;
          }
      | `Status (Unix.WSIGNALED signal) ->
          {
            errors = None;
            raised = Some (Printf.sprintf "the check died on signal %d" signal);
            hung = false;
          }
      | `Status (Unix.WSTOPPED signal) ->
          {
            errors = None;
            raised =
              Some (Printf.sprintf "the check stopped on signal %d" signal);
            hung = false;
          })

let check_case ~file ~label ~single ~text ~items ~touched ~start ~cut =
  let checked = measure ~file ~text in
  let mutated, _ = Jide_jasmin.Warnings.with_capture (fun () -> outline text) in
  {
    file;
    label;
    errors = checked.errors;
    raised = checked.raised;
    hung = checked.hung;
    outline = Some (outline_away ~items ~mutated ~touched ~start ~cut);
    single;
  }

let histogram (cases : case list) =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (case : case) ->
      match case.errors with
      | None -> ()
      | Some errors ->
          let previous =
            match Hashtbl.find_opt table errors with
            | Some count -> count
            | None -> 0
          in
          Hashtbl.replace table errors (previous + 1))
    cases;
  List.sort
    (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun errors count acc -> (errors, count) :: acc) table [])

