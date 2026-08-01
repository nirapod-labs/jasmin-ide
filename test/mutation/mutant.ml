(** Single-token mutations of one source text.

    Four families, as the recovery criterion names them:

    - {!Delete}: one token is removed;
    - {!Duplicate}: one token is copied in place, directly after itself;
    - {!Swap}: a [;] becomes [}] or a [}] becomes [;];
    - {!Truncate}: the text is cut at the start of a token, so every token
      from there on is gone.

    A mutation records the byte range of the original text it touches and a
    function from mutant offsets back to original ones. The outline
    comparison needs both, because deleting a token moves every byte after
    it and an outline entry that did not change still has new offsets.

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
  map_back : int -> int;
      (** A mutant byte offset, mapped back into the original text. *)
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
    map_back =
      (fun offset -> if offset < start_byte then offset else offset + removed);
    label = Printf.sprintf "delete token %d `%s`" index (Token.show token);
  }

