(** The intrinsic measurement: the engine's table against the
    compiler's own.

    The requirement is that the intrinsic list for each architecture equals
    [jasminc -arch <arch> -help-intrinsics]. The compiler half of that is
    read here from a real compiler binary, never from the linked library:
    [--compiler] names a command that takes the compiler's arguments (a
    wrapper that runs the compiler somewhere else works as well as a binary),
    and every answer below comes from running it.

    Two questions are asked, because the printed list answers only the first.

    - The names, and the suffix class each name is printed under. The
      compiler prints seven groups, one per shape of suffix (none, a plain
      size, a signed size, a zero/sign extension, one vector description, two
      vector descriptions, and the ARM flag and condition pair), and sorts
      each group. The engine derives the same class from each entry's suffix
      list, so a name that moved groups is a difference even when both sides
      list it.
    - The suffixes themselves. The printed list does not carry them, so each
      name is asked directly. For a name whose class is not the ARM pair, one
      probe writes a suffix of another class: [Pretyping.tt_prim] then raises
      [PrimWrongSuffix], whose message is the compiler printing that name's
      whole valid suffix list ([pretyping.ml], the [tt_prim] of a [PrimX86]
      name). For a name in the ARM pair the constructor is [PrimARM], which
      reports only "unknown primitive", so the four spellings the grammar
      allows (none, [S], [cc], [Scc]) are each asked for and the ones the
      compiler resolves are the accepted set.

    A probe is one compiler run, and a run costs a process; [--jobs] bounds how
    many run at once. Every run's source, its written name and the compiler's
    own words are kept when [--raw-dir] names a directory, so the evidence a
    difference rests on can be read back.

    Usage:

    {v
      intrinsics --compiler <command> [--arch <arch>]... [--jobs <n>]
                 [--raw-dir <dir>]
      intrinsics --selftest
    v}

    Exit status: 0 when every architecture's names and suffixes are equal,
    1 when one is not, 2 when the run cannot be made (no compiler, a compiler
    that printed something this program does not understand, a bad
    argument). *)

let usage =
  String.concat "\n"
    [
      "usage: intrinsics --compiler <command> [--arch <arch>]... [--jobs <n>]";
      "                  [--raw-dir <dir>]";
      "       intrinsics --selftest";
      "";
      "For each architecture, compare the compiler's own intrinsic list";
      "(`-arch <arch> -help-intrinsics`) and the suffix each name accepts with";
      "the engine's table (Jide_jasmin.Intrinsics.all).";
    ]

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
(* The class a name is printed under *)

(* [Help.show_intrinsics] groups by the suffix class of the constructor's
   first entry, and writes the seven groups in the order [headers] lists.
   This is that classification read back from a suffix string, which is the
   shape the interface documents: a plain size, a signed size, a zero/sign
   extension, one vector description, two vector descriptions, and the ARM
   pair (the empty string, [S], [cc] and [Scc]). *)
let class_of_suffix (suffix : string) =
  let is_digits text =
    String.length text > 0
    && String.for_all
         (fun character -> character >= '0' && character <= '9')
         text
  in
  let signs part =
    let found = ref [] in
    String.iteri
      (fun index character ->
        if character = 'u' || character = 's' then found := index :: !found)
      part;
    List.rev !found
  in
  let sub text from to_ = String.sub text from (to_ - from) in
  let class_of_part part =
    match signs part with
    | [] -> if is_digits part then 1 else -1
    | [ index ] when index = 0 ->
        if is_digits (sub part 1 (String.length part)) then 2 else -1
    | [ index ] ->
        if
          is_digits (sub part 0 index)
          && is_digits (sub part (index + 1) (String.length part))
        then 4
        else -1
    | [ first; second ]
      when first = 0
           && is_digits (sub part 1 second)
           && is_digits (sub part (second + 1) (String.length part)) ->
        3
    | _ -> -1
  in
  if String.equal suffix "" then 6
  else if List.mem suffix [ "S"; "cc"; "Scc" ] then 6
  else
    match String.split_on_char '_' suffix with
    | [ part ] -> class_of_part part
    | [ one; two ] ->
        if class_of_part one = 4 && class_of_part two = 4 then 5 else -1
    | _ -> -1

(* The class of a whole suffix list: none for an empty one, the common class
   when every suffix has the same shape, and -1 when the list mixes shapes a
   group cannot describe. *)
let class_of_suffixes = function
  | [] -> 0
  | first :: rest ->
      let class_ = class_of_suffix first in
      if List.for_all (fun suffix -> class_of_suffix suffix = class_) rest then
        class_
      else -1

(* ------------------------------------------------------------------ *)
(* Asking the compiler about one name *)

type probe =
  | Report  (** write a suffix of another class; the answer is the list *)
  | Spelling of string
      (** write this whole name; the answer is accepted or not *)

(* The suffix a [Report] probe writes: one class away from the name's own, so
   that [tt_prim] answers with the valid list instead of resolving. A name
   with no suffix list answers "accepts no size annotation", which is the
   empty list. *)
let probe_suffix = function
  | 1 -> "s16" (* a signed size, against a plain-size list *)
  | 4 | 5 -> "s16" (* against a vector-description list *)
  | _ -> "64" (* against a list of signed sizes, an extension or nothing *)

let source ~(written : string) =
  Printf.sprintf
    "export fn probe(reg u64 x) -> reg u64 {\n\
    \  reg u64 y;\n\
    \  y = #%s(x);\n\
    \  return y;\n\
     }\n"
    written

let contains ~(needle : string) (haystack : string) =
  let rec find index =
    if index + String.length needle > String.length haystack then false
    else if
      String.equal (String.sub haystack index (String.length needle)) needle
    then true
    else find (index + 1)
  in
  find 0

(* The suffixes [Pretyping.pp_suffix] writes, read back from the compiler's
   own message: everything after the colon is the list, comma separated and
   wrapped over as many lines as the formatter needed. *)
let suffixes_of_message (text : string) =
  let marker = "only accepts the following size annotations:" in
  let rec find index =
    if index + String.length marker > String.length text then None
    else if String.equal (String.sub text index (String.length marker)) marker
    then Some (index + String.length marker)
    else find (index + 1)
  in
  match find 0 with
  | None -> None
  | Some start ->
      let rest = String.sub text start (String.length text - start) in
      let words =
        String.split_on_char ',' rest
        |> List.map (fun part ->
               String.to_seq part
               |> Seq.filter (fun character ->
                      match character with
                      | ' ' | '\t' | '\r' | '\n' | '"' -> false
                      | _ -> true)
               |> String.of_seq)
        |> List.filter (fun word -> String.length word > 0)
      in
      Some
        (List.map
           (fun suffix ->
             if String.length suffix > 0 && suffix.[0] = '_' then
               String.sub suffix 1 (String.length suffix - 1)
             else suffix)
           words)

type answer =
  [ `Known of string list
    (** the compiler printed the name's whole valid suffix list *)
  | `Unknown  (** "unknown primitive": this spelling resolves to nothing *)
  | `Other of string  (** the run said something else; [string] is why *)
  | `Accepted  (** a spelling probe resolved: the compiler accepts it *)
  | `Refused  (** a spelling probe the compiler calls unknown *)
  | `Broken of string  (** the wrapper failed, so nothing was measured *) ]

(* [label_of_answer] is an answer as the raw record writes it. It is total over
   [answer] on purpose: the record keeps every probe, spelling probes included,
   so that a difference can be read back against the run that produced it. *)
let label_of_answer (answer : answer) =
  match answer with
  | `Known suffixes -> "known:" ^ String.concat "," suffixes
  | `Unknown -> "unknown"
  | `Other reason -> "other:" ^ reason
  | `Accepted -> "accepted"
  | `Refused -> "refused"
  | `Broken reason -> "broken:" ^ reason

(* [classify ~probe ~status ~output] is what one run answered. A status other
   than 0 or 1 is not the compiler speaking: it is the wrapper failing, and it
   is reported rather than read as an answer. *)
let classify ~(probe : probe) ~(status : int) ~(output : string) : answer =
  match probe with
  | Spelling _ -> (
      match status with
      | 0 -> `Accepted
      | 1 when contains ~needle:"unknown primitive" output -> `Refused
      | 1 -> `Accepted
      | other -> `Broken (Printf.sprintf "exit %d" other))
  | Report -> (
      match status with
      | 0 -> `Other "the probe resolved, so the compiler printed no list"
      | 1 when contains ~needle:"unknown primitive" output -> `Unknown
      | 1 -> (
          match suffixes_of_message output with
          | Some suffixes -> `Known suffixes
          | None ->
              if contains ~needle:"accepts no size annotation" output then
                `Known []
              else
                let message =
                  match String.split_on_char '\n' output with
                  | [] -> "no message"
                  | lines -> (
                      let lines =
                        List.filter
                          (fun line -> not (String.equal line ""))
                          lines
                      in
                      match lines with
                      | [] -> "no message"
                      | line :: _ -> String.trim line)
                in
                `Other message)
      | other -> `Broken (Printf.sprintf "exit %d" other))

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

