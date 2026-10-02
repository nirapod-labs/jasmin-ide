(** One single-token mutant per corpus file per family, for the first-error
    parity run, which puts each mutated file in front of the checker and the
    compiler. {!Mutant} defines the families; this chooses the sites.

    The site is a function of the seed, the file's path below the corpus and
    the family's name, so the same seed over the same corpus gives the same
    set, in the same order, on every run and every host. {!Mutant.Truncate}
    is not a single-token mutation and is not emitted. *)

type t = {
  index : int;
  family : Mutant.family;
  relative : string;  (** The file's path below the corpus. *)
  label : string;
  text : string;  (** The mutated text. *)
}

let families = [ Mutant.Delete; Mutant.Duplicate; Mutant.Swap ]

(* FNV-1a over the seed, the path and the family. A hash rather than the
   standard library's generator, so a site cannot move when a library version
   changes: the set has to be replayable from the seed alone. *)
let hash text =
  let offset = Int64.of_string "0xcbf29ce484222325" in
  let prime = Int64.of_string "0x100000001b3" in
  let result = ref offset in
  String.iter
    (fun character ->
      result := Int64.logxor !result (Int64.of_int (Char.code character));
      result := Int64.mul !result prime)
    text;
  (* 62 bits, the positive range of an [int] on a 64-bit host. *)
  Int64.(to_int (logand !result 0x3fff_ffff_ffff_ffffL))

let site ~seed ~relative family sites =
  match Array.of_list sites with
  | [||] -> None
  | candidates ->
      let key = seed ^ "\000" ^ relative ^ "\000" ^ Mutant.family_name family in
      Some candidates.(hash key mod Array.length candidates)

(** At most [maximum] of [files], spread evenly over the list, so a bounded
    run measures the whole corpus and not its first files. *)
let spread maximum files =
  let total = List.length files in
  if maximum <= 0 || total <= maximum then files
  else
    let array = Array.of_list files in
    List.init maximum (fun index -> array.(index * total / maximum))

(** [emit ~seed ~files ~read relatives] is the mutants of [relatives], read
    with [read], in order. [files] bounds how many are mutated (0: all). *)
let emit ~seed ~files ~read relatives =
  let index = ref 0 in
  List.concat_map
    (fun relative ->
      match read relative with
      | None -> []
      | Some text ->
          let tokens = Array.of_list (Token.tokenize text) in
          if Array.length tokens = 0 then []
          else
            List.filter_map
              (fun family ->
                match
                  site ~seed ~relative family
                    (Mutant.sites ~every:1 tokens family)
                with
                | None -> None
                | Some chosen ->
                    let mutation =
                      Mutant.make ~source:text tokens family chosen
                    in
                    let number = !index in
                    incr index;
                    Some
                      {
                        index = number;
                        family;
                        relative;
                        label = mutation.Mutant.label;
                        text = mutation.Mutant.text;
                      })
              families)
    (spread files relatives)
