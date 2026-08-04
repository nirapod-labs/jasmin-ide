(** What one run measured, and how to print it.

    One case is one mutation of one file. A case is inside the criteria when
    its check returned (no exception, no hang, no crash), when the outline of
    the items away from the edit is unchanged, and, for the single-token
    families, when the check reported at most two errors. The three
    percentages are printed with their counts, so a reader can check them
    without trusting a percentage. *)

type case = {
  path : string;  (** The file, relative to the corpus root it came from. *)
  mutation : Mutant.t;
  outcome : Isolate.outcome;
  comparison : Outline.comparison;
  enclosing : (string * bool) option;
      (** The item the edit fell inside, and whether it is still there. *)
}

type totals = {
  found : int;  (** .jazz files under the corpus roots. *)
  clean : int;  (** Files the base check accepted with no error. *)
  sampled : int;
  cases : case list;
}

let returned case =
  match case.outcome with Isolate.Returned _ -> true | _ -> false

let errors case =
  match case.outcome with
  | Isolate.Returned { result; _ } -> Some result.Probe.errors
  | _ -> None

let single_token case = Mutant.is_single_token case.mutation.family

let outline_unchanged case =
  returned case && case.comparison.Outline.identity_ok
  && case.comparison.Outline.span_ok

let within_two case =
  match errors case with Some count -> count <= 2 | None -> false

let percent passed total =
  if total = 0 then 0. else 100. *. float_of_int passed /. float_of_int total

(** The measured error counts, one list per family. *)
let counts_by_family cases =
  List.map
    (fun family ->
      ( family,
        List.filter_map
          (fun case ->
            if case.mutation.Mutant.family = family then errors case else None)
          cases ))
    Mutant.families

let mean values =
  match values with
  | [] -> 0.
  | _ ->
      float_of_int (List.fold_left ( + ) 0 values)
      /. float_of_int (List.length values)

