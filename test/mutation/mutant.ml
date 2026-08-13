(** Single-token mutations of one source text.

    Four families, as the recovery criterion names them:

    - {!Delete}: one token is removed;
    - {!Duplicate}: one token is copied in place, directly after itself;
    - {!Swap}: a [;] becomes [}] or a [}] becomes [;];
    - {!Truncate}: the text is cut at the start of a token, so every token
      from there on is gone.

    A mutation records the byte range of the original text it touches and
    two functions from mutant offsets back to original ones: one for where an
    entry starts and one for where it ends. The outline comparison needs
    both, because deleting a token moves every byte after it and an outline
    entry that did not change still has new offsets. The two differ at the
    edit's boundary: an offset there is the end of the text before the edit
    and the start of the text after it, so a start and an end that meet at
    the boundary map to different sides of it.

    Generation depends on nothing but the source and the token index, so two
    runs over the same corpus apply the same mutations in the same order. *)

type family = Delete | Duplicate | Swap | Truncate

let family_name = function
  | Delete -> "delete"
  | Duplicate -> "duplicate"
  | Swap -> "swap"
  | Truncate -> "truncate"

let families = [ Delete; Duplicate; Swap; Truncate ]

(** A single-token family changes exactly one token; [Truncate] removes every
    token from its site on, so the two-error criterion is measured on the
    families this answers [true] for. *)
let is_single_token = function Truncate -> false | _ -> true

type t = {
  family : family;
  site : int;  (** Index of the token the mutation starts from. *)
  text : string;  (** The mutated source. *)
  edit_start : int;  (** First byte of the touched range, in the original. *)
  edit_end : int;  (** One past its last byte, in the original. *)
  map_start : int -> int;
      (** A mutant byte offset where an entry starts, mapped back into the
          original text. At the boundary it maps to the side the text after
          the edit is on. *)
  map_end : int -> int;
      (** A mutant byte offset where an entry ends, mapped back into the
          original text. At the boundary it maps to the side the text before
          the edit is on. *)
  label : string;  (** One line naming the mutation, for a report. *)
}

let splice source start_byte end_byte replacement =
  String.sub source 0 start_byte
  ^ replacement
  ^ String.sub source end_byte (String.length source - end_byte)

let delete ~source (tokens : Token.t array) index =
  let token = tokens.(index) in
  let start_byte = token.start_byte and end_byte = token.end_byte in
  let removed = end_byte - start_byte in
  {
    family = Delete;
    site = index;
    text = splice source start_byte end_byte "";
    edit_start = start_byte;
    edit_end = end_byte;
    map_start =
      (fun offset -> if offset < start_byte then offset else offset + removed);
    map_end =
      (fun offset -> if offset <= start_byte then offset else offset + removed);
    label = Printf.sprintf "delete token %d `%s`" index (Token.show token);
  }

let duplicate ~source (tokens : Token.t array) index =
  let token = tokens.(index) in
  let start_byte = token.start_byte and end_byte = token.end_byte in
  let copied = end_byte - start_byte in
  {
    family = Duplicate;
    site = index;
    text = splice source end_byte end_byte token.text;
    edit_start = start_byte;
    edit_end = end_byte;
    map_start =
      (fun offset -> if offset < end_byte then offset else offset - copied);
    map_end =
      (fun offset -> if offset <= end_byte then offset else offset - copied);
    label = Printf.sprintf "duplicate token %d `%s`" index (Token.show token);
  }

let swap ~source (tokens : Token.t array) index =
  let token = tokens.(index) in
  let replacement = if token.text = ";" then "}" else ";" in
  {
    family = Swap;
    site = index;
    text = splice source token.start_byte token.end_byte replacement;
    edit_start = token.start_byte;
    edit_end = token.end_byte;
    map_start = (fun offset -> offset);
    map_end = (fun offset -> offset);
    label =
      Printf.sprintf "swap token %d `%s` for `%s`" index (Token.show token)
        replacement;
  }

let truncate ~source (tokens : Token.t array) index =
  let token = tokens.(index) in
  {
    family = Truncate;
    site = index;
    text = String.sub source 0 token.start_byte;
    edit_start = token.start_byte;
    edit_end = String.length source;
    map_start = (fun offset -> offset);
    map_end = (fun offset -> offset);
    label =
      Printf.sprintf "truncate at token %d of %d" index (Array.length tokens);
  }

let make ~source tokens = function
  | Delete -> delete ~source tokens
  | Duplicate -> duplicate ~source tokens
  | Swap -> swap ~source tokens
  | Truncate -> truncate ~source tokens

(** The token indices a family applies to. [Swap] has a site only where a
    [;] or a [}] stands; [Truncate] takes every [~every]th token. *)
let sites ~every tokens = function
  | Delete | Duplicate -> List.init (Array.length tokens) (fun index -> index)
  | Swap ->
      let found = ref [] in
      Array.iteri
        (fun index token ->
          if Token.is_swap_candidate token then found := index :: !found)
        tokens;
      List.rev !found
  | Truncate ->
      let count = Array.length tokens in
      let stride = if every < 1 then 1 else every in
      List.init ((count + stride - 1) / stride) (fun index -> index * stride)

(** [sample ~maximum sites] keeps at most [maximum] of [sites], spread evenly
    over the list, and answers them with the number of sites that existed.
    A corpus file may hold thousands of tokens; a bound that keeps the first
    ones would measure one corner of the file and not the rest. *)
let sample ~maximum sites =
  let total = List.length sites in
  if maximum <= 0 || total <= maximum then (sites, total)
  else
    let array = Array.of_list sites in
    (List.init maximum (fun index -> array.(index * total / maximum)), total)
