(** The proof that an outline case cannot be met by any parser.

    A case is an original text [A], a mutation of it and the mutated text
    [M]. The outline criterion asks a parser of [M] for the items of [A] that
    the mutation left alone. A parser sees [M] only, so when a second text
    [B], which the compiler's parser accepts, gives the same [M] under a
    mutation of a kind the generator makes, the parser has to answer [B]'s
    criterion with the same outline. If the two criteria ask for outlines no
    single parse can give, no parser meets both, and the case says nothing
    about the parser under test. [B] is the witness.

    Nothing here is taken on trust. [B] is built as text, parsed by the
    compiler's own parser ({!Jasmin.Parseio}), mutated by the same functions
    the harness uses ({!Mutant}), and the result is compared with [M] byte
    for byte. The witness's outline is read off that parse. The proof is
    syntactic, as a parser is: [B] need not type.

    Two requirements contradict when no outline of one parse meets both:
    the same item (kind and qualified name) at two extents that overlap but
    differ, since two items of a parse are nested or disjoint and an item
    never nests in one with its own qualified name; or two different items
    at one extent, since a parse has one item per extent. The extents are
    those an entry of [M] must have to map back onto the original's entry,
    under that original's own mutation. *)

type requirement = {
  kind : string;
  name : string;
  extents : (int * int) list;
      (** The byte ranges of [M] an entry may cover to meet it. *)
}

type difference = Extent | Name

type t = {
  family : Mutant.family;  (** The mutation that takes [B] to [M]. *)
  site : int;  (** Its token index in [B]. *)
  text : string;  (** [B]. *)
  how : string;  (** How [B] is made from [M], in words. *)
  ours : requirement;  (** What the case's own original asks for. *)
  theirs : requirement;  (** What [B] asks for. *)
  difference : difference;
}

let family_name = Mutant.family_name

let difference_name = function
  | Extent -> "the same item at another extent"
  | Name -> "another item at the same extent"

(** The offsets of [M] that [map] sends to [target]. Every family moves an
    offset by nothing or by the length the text changed, so those are the
    only candidates. *)
let preimages ~map ~shift ~length target =
  List.sort_uniq compare
    (List.filter
       (fun offset -> offset >= 0 && offset <= length && map offset = target)
       [ target; target - shift; target + shift ])

(** What an original asks of a parser of [M]: each entry away from its
    mutation's edit, with the extents of [M] that map back onto it. *)
let requirements ~(outline : Outline.entry list) ~(mutation : Mutant.t)
    ~original_length =
  let length = String.length mutation.Mutant.text in
  let shift = abs (original_length - length) in
  List.filter_map
    (fun (entry : Outline.entry) ->
      let starts =
        preimages ~map:mutation.Mutant.map_start ~shift ~length
          entry.Outline.start_byte
      and ends =
        preimages ~map:mutation.Mutant.map_end ~shift ~length
          entry.Outline.end_byte
      in
      let extents =
        List.concat_map
          (fun start -> List.map (fun stop -> (start, stop)) ends)
          starts
      in
      if extents = [] then None
      else
        Some { kind = entry.Outline.kind; name = entry.Outline.name; extents })
    (Outline.away_from outline ~edit_start:mutation.Mutant.edit_start
       ~edit_end:mutation.Mutant.edit_end)

let overlap (a_start, a_end) (b_start, b_end) =
  a_start < b_end && b_start < a_end

let contradiction ours theirs =
  List.find_map
    (fun (a : requirement) ->
      List.find_map
        (fun (b : requirement) ->
          let pairs =
            List.concat_map
              (fun x -> List.map (fun y -> (x, y)) b.extents)
              a.extents
          in
          if a.kind = b.kind && a.name = b.name then
            if List.for_all (fun (x, y) -> overlap x y && x <> y) pairs then
              Some (a, b, Extent)
            else None
          else
            match (a.extents, b.extents) with
            | [ x ], [ y ] when x = y -> Some (a, b, Name)
            | _ -> None)
        theirs)
    ours

(** The compiler parser's outline of [text], or [None] when it refuses it. *)
let parse text =
  match
    Jasmin.Parseio.parse_program ~name:"witness.jazz"
      (Jasmin.Utils.IO.input_string text)
  with
  | items -> Some (Outline.of_items items)
  | exception _ -> None

let insert text at word =
  String.sub text 0 at ^ word ^ String.sub text at (String.length text - at)

let remove text start stop =
  String.sub text 0 start ^ String.sub text stop (String.length text - stop)

(** The texts [B] tried, each with the mutation that should take it back to
    [M]: for every family, the texts that family can make [M] from. *)
let candidates ~(mutation : Mutant.t) mutant =
  let tokens = Array.of_list (Token.tokenize mutant) in
  let count = Array.length tokens in
  let length = String.length mutant in
  let near offset =
    (* The search goes outward from the edit, which is where a witness
       usually is; every position is still tried. *)
    abs (offset - mutation.Mutant.edit_start)
  in
  let boundaries =
    List.sort_uniq compare
      (0 :: length
      :: List.concat_map
           (fun (token : Token.t) -> [ token.start_byte; token.end_byte ])
           (Array.to_list tokens))
    |> List.stable_sort (fun a b -> compare (near a) (near b))
  in
  (* Words a deletion may have taken out: the brackets and the [;] that
     decide structure, and a name, which decides qualified names. The word
     the case's own deletion took out is added by the caller. *)
  let words extra = extra @ [ "}"; "{"; ";"; "z" ] in
  let deletions extra =
    List.concat_map
      (fun at ->
        List.map
          (fun word ->
            ( Mutant.Delete,
              insert mutant at word,
              at,
              Printf.sprintf "the mutated text with `%s` written at byte %d"
                (String.escaped word) at ))
          (words extra))
      boundaries
  in
  let duplicates =
    List.concat
      (List.init count (fun index ->
           let token = tokens.(index) in
           let pair =
             if
               index + 1 < count
               && tokens.(index + 1).Token.text = token.Token.text
               && tokens.(index + 1).Token.start_byte = token.Token.end_byte
             then
               [
                 ( Mutant.Duplicate,
                   remove mutant tokens.(index + 1).Token.start_byte
                     tokens.(index + 1).Token.end_byte,
                   token.Token.start_byte,
                   Printf.sprintf
                     "the mutated text with the second of the two `%s` at byte \
                      %d taken out"
                     (String.escaped token.Token.text)
                     token.Token.start_byte );
               ]
             else []
           in
           let text = token.Token.text in
           let half = String.length text / 2 in
           let fused =
             if
               token.Token.kind = Token.Ident
               && String.length text >= 2
               && String.length text mod 2 = 0
               && String.sub text 0 half = String.sub text half half
             then
               [
                 ( Mutant.Duplicate,
                   remove mutant
                     (token.Token.start_byte + half)
                     token.Token.end_byte,
                   token.Token.start_byte,
                   Printf.sprintf
                     "the mutated text with `%s` at byte %d written `%s`" text
                     token.Token.start_byte (String.sub text 0 half) );
               ]
             else []
           in
           pair @ fused))
  in
  let swaps =
    List.concat
      (List.init count (fun index ->
           let token = tokens.(index) in
           match token.Token.text with
           | ";" | "}" ->
               let other = if token.Token.text = ";" then "}" else ";" in
               [
                 ( Mutant.Swap,
                   insert
                     (remove mutant token.Token.start_byte token.Token.end_byte)
                     token.Token.start_byte other,
                   token.Token.start_byte,
                   Printf.sprintf
                     "the mutated text with `%s` at byte %d written `%s`"
                     token.Token.text token.Token.start_byte other );
               ]
           | _ -> []))
  in
  let truncations =
    List.map
      (fun suffix ->
        ( Mutant.Truncate,
          mutant ^ suffix,
          length,
          Printf.sprintf "the mutated text followed by `%s`"
            (String.escaped suffix) ))
      [ "fn w() {}\n"; "}\n"; "}}\n"; "} fn w() {}\n"; "}} fn w() {}\n" ]
  in
  fun extra -> deletions extra @ duplicates @ swaps @ truncations

(** [find ~original ~outline ~mutation] is a witness for the case, if one
    exists among the texts tried. [outline] is the original's. *)
let find ~original ~(outline : Outline.entry list) ~(mutation : Mutant.t) =
  let mutant = mutation.Mutant.text in
  let ours =
    requirements ~outline ~mutation ~original_length:(String.length original)
  in
  let extra =
    match mutation.Mutant.family with
    | Mutant.Delete ->
        let word =
          String.sub original mutation.Mutant.edit_start
            (mutation.Mutant.edit_end - mutation.Mutant.edit_start)
        in
        [ word ]
    | _ -> []
  in
  let tried = (candidates ~mutation mutant) extra in
  List.find_map
    (fun (family, text, at, how) ->
      if String.equal text original then None
      else
        let tokens = Array.of_list (Token.tokenize text) in
        (* The mutation that takes [B] back to [M] starts at the token that
           begins at [at]. *)
        let site =
          let rec find index =
            if index >= Array.length tokens then None
            else if tokens.(index).Token.start_byte = at then Some index
            else find (index + 1)
          in
          find 0
        in
        match site with
        | None -> None
        | Some site -> (
            let back = Mutant.make ~source:text tokens family site in
            if not (String.equal back.Mutant.text mutant) then None
            else
              match parse text with
              | None -> None
              | Some theirs_outline -> (
                  let theirs =
                    requirements ~outline:theirs_outline ~mutation:back
                      ~original_length:(String.length text)
                  in
                  match contradiction ours theirs with
                  | None -> None
                  | Some (ours, theirs, difference) ->
                      Some { family; site; text; how; ours; theirs; difference }
                  )))
    tried

let show_requirement (requirement : requirement) =
  Printf.sprintf "%s %s %s" requirement.kind requirement.name
    (String.concat " or "
       (List.map
          (fun (start, stop) -> Printf.sprintf "%d..%d" start stop)
          requirement.extents))

(** One line for a proven case. *)
let describe witness =
  Printf.sprintf
    "witness: %s; %s token %d of it gives the same text; here %s, there %s"
    witness.how
    (family_name witness.family)
    witness.site
    (show_requirement witness.ours)
    (show_requirement witness.theirs)
