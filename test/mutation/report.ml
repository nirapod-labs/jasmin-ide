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
  proof : Witness.t option;
      (** For a case whose outline changed, the witness that no parser could
          have kept it (see {!Witness}); [None] otherwise. *)
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

(** A case whose outline changed and that a witness proves no parser could
    have kept. Only a case that returned an outline can be proven: a hang or
    a crash is a failure of the parser under test whatever the text. *)
let proven case =
  returned case && (not (outline_unchanged case)) && case.proof <> None

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

let median values =
  match List.sort compare values with
  | [] -> 0.
  | sorted ->
      let count = List.length sorted in
      let at index = List.nth sorted index in
      if count mod 2 = 1 then float_of_int (at (count / 2))
      else
        (float_of_int (at ((count / 2) - 1)) +. float_of_int (at (count / 2)))
        /. 2.

let maximum values = List.fold_left max 0 values
let bucket count = if count >= 5 then 5 else count
let buckets = [ 0; 1; 2; 3; 4; 5 ]
let bucket_label = function 5 -> "5+" | count -> string_of_int count

let header roots arch call_conv budget =
  Printf.printf "# recovery mutation harness\n\n";
  Printf.printf "corpus roots:  %s\n" (String.concat ", " roots);
  Printf.printf
    "target:        arch %s, calling convention %s, no include dirs\n" arch
    call_conv;
  Printf.printf
    "hang budget:   %.1f s per check, measured around a child process\n" budget;
  Printf.printf
    "mutations:     delete one token, duplicate one token, swap ; and },\n";
  Printf.printf "               truncate at every k-th token\n\n"

let sample totals =
  Printf.printf "## the sample\n\n";
  Printf.printf
    "  %d .jazz files under the roots, %d accepted by the base check,\n"
    totals.found totals.clean;
  Printf.printf "  %d sampled for mutation\n\n" totals.sampled

let distribution cases =
  let single = List.filter single_token cases in
  let by_family = counts_by_family single in
  let all = List.concat_map snd by_family in
  let measured = List.length all in
  let skipped = List.length single - measured in
  Printf.printf "## errors per single-token mutation\n\n";
  Printf.printf "  %-6s %8s %10s %8s %8s %8s\n" "errors" "delete" "duplicate"
    "swap" "total" "share";
  List.iter
    (fun limit ->
      let counts =
        List.map
          (fun (_, values) ->
            List.length (List.filter (fun value -> bucket value = limit) values))
          by_family
      in
      let total = List.fold_left ( + ) 0 counts in
      Printf.printf "  %-6s %8d %10d %8d %8d %7.1f%%\n" (bucket_label limit)
        (List.nth counts 0) (List.nth counts 1) (List.nth counts 2) total
        (percent total measured))
    buckets;
  Printf.printf "  %-6s %8d %10d %8d %8d %7.1f%%\n" "total"
    (List.nth by_family 0 |> snd |> List.length)
    (List.nth by_family 1 |> snd |> List.length)
    (List.nth by_family 2 |> snd |> List.length)
    measured
    (percent measured measured);
  Printf.printf
    "\n  mean %.2f, median %.1f, maximum %d, over %d measured cases\n"
    (mean all) (median all) (maximum all) measured;
  if skipped > 0 then
    Printf.printf
      "  %d single-token cases have no error count: the check did not return\n"
      skipped;
  Printf.printf "\n"

let truncation cases =
  let values =
    List.filter_map
      (fun case ->
        if case.mutation.Mutant.family = Mutant.Truncate then errors case
        else None)
      cases
  in
  Printf.printf
    "## errors per truncation (not part of the two-error criterion)\n\n";
  if values = [] then Printf.printf "  no truncation case returned\n\n"
  else
    Printf.printf "  cases %d, mean %.2f, median %.1f, maximum %d\n\n"
      (List.length values) (mean values) (median values) (maximum values)

let criteria cases =
  let total = List.length cases in
  let quiet = List.length (List.filter returned cases) in
  let unchanged = List.length (List.filter outline_unchanged cases) in
  let impossible = List.length (List.filter proven cases) in
  let compared =
    List.length
      (List.filter (fun case -> case.comparison.Outline.away > 0) cases)
  in
  let away =
    List.fold_left
      (fun total case -> total + case.comparison.Outline.away)
      0 cases
  in
  let single = List.filter single_token cases in
  let two = List.length (List.filter within_two single) in
  let single_total = List.length single in
  let with_items =
    List.length
      (List.filter
         (fun case ->
           match case.outcome with
           | Isolate.Returned { result; _ } -> result.Probe.outline <> []
           | _ -> false)
         cases)
  in
  let enclosing = List.filter_map (fun case -> case.enclosing) cases in
  let enclosing_here =
    List.length (List.filter (fun (_, present) -> present) enclosing)
  in
  let holds_quiet = total > 0 && quiet = total in
  (* The outline criterion holds when every case keeps its outline or is
     proven impossible for any parser; a changed outline with no witness
     fails it. *)
  let holds_outline = total > 0 && unchanged + impossible = total in
  (* The two-error criterion holds from 95%: a single bad mutation in a
     hundred is inside it. The other two hold only at 100%. *)
  let holds_two = single_total > 0 && percent two single_total >= 95.0 in
  Printf.printf "## criteria\n\n";
  let line name passed count holds =
    Printf.printf "  %-34s %6d/%-6d %6.1f%%  %s\n" name passed count
      (percent passed count)
      (if holds then "PASS" else "FAIL")
  in
  line "no exception or hang" quiet total holds_quiet;
  Printf.printf "  %-34s %6d/%-6d %6.1f%%\n"
    "outline away from the edit unchanged" unchanged total
    (percent unchanged total);
  Printf.printf "  %-34s %6d/%-6d         (a witness each, listed below)\n"
    "  the rest proven impossible" impossible (total - unchanged);
  line "outline: unchanged or impossible" (unchanged + impossible) total
    holds_outline;
  Printf.printf "  %-34s %6d/%-6d %6.1f%%  %s  (needs 95%%)\n"
    "at most two errors (single token)" two single_total
    (percent two single_total)
    (if holds_two then "PASS" else "FAIL");
  Printf.printf "  %-34s %6d/%-6d %6.1f%%  (the rest returned no item)\n"
    "the mutated file still has an item" with_items total
    (percent with_items total);
  Printf.printf "  %-34s %6d cases compared %d items; %d had none away\n" ""
    compared away (total - compared);
  if enclosing <> [] then
    Printf.printf "  %-34s %6d/%-6d %6.1f%%  (measured, not a criterion)\n"
      "the enclosing item is still there" enclosing_here (List.length enclosing)
      (percent enclosing_here (List.length enclosing));
  Printf.printf "\n";
  (holds_quiet, holds_outline, holds_two)

let families cases =
  Printf.printf "## per family\n\n";
  Printf.printf "  %-10s %8s %10s %12s %12s\n" "family" "cases" "no except."
    "outline same" "at most two";
  List.iter
    (fun family ->
      let of_family =
        List.filter (fun case -> case.mutation.Mutant.family = family) cases
      in
      let total = List.length of_family in
      let quiet = List.length (List.filter returned of_family) in
      let unchanged = List.length (List.filter outline_unchanged of_family) in
      let two = List.length (List.filter within_two of_family) in
      let two_note =
        if Mutant.is_single_token family then
          Printf.sprintf "%d/%d %.1f%%" two total (percent two total)
        else "-"
      in
      Printf.printf "  %-10s %8d %10d %12d %12s\n"
        (Mutant.family_name family)
        total quiet unchanged two_note)
    Mutant.families;
  Printf.printf "\n"

(** Why the cases outside the criteria are outside them, by kind. A run whose
    outline criterion fails on every damaged text and a run that fails on one
    case show the same "FAIL" line; the counts here say which one it was, and
    the first count in particular says whether the adapter returned any item
    at all for the mutated text. *)
let causes cases =
  let no_items = ref 0 and differs = ref 0 and unquiet = ref 0 in
  let too_many = ref 0 in
  List.iter
    (fun case ->
      match case.outcome with
      | Isolate.Returned { result; _ } -> (
          if not (outline_unchanged case) then
            if result.Probe.outline = [] then incr no_items else incr differs;
          match errors case with
          | Some count when single_token case && count > 2 -> incr too_many
          | _ -> ())
      | _ -> incr unquiet)
    cases;
  Printf.printf "## why a case is outside the criteria\n\n";
  Printf.printf "  %-40s %6d\n" "the mutant produced no item at all" !no_items;
  Printf.printf "  %-40s %6d\n" "the mutant's outline differs" !differs;
  Printf.printf "  %-40s %6d\n" "  of these, proven impossible"
    (List.length (List.filter proven cases));
  Printf.printf "  %-40s %6d\n" "the check raised, hung or crashed" !unquiet;
  Printf.printf "  %-40s %6d\n" "more than two errors (single token)" !too_many;
  Printf.printf
    "\n  the first three counts are the outline criterion; the last one\n";
  Printf.printf "  overlaps them and is the two-error criterion\n\n"

let failure case =
  match case.outcome with
  | Isolate.Timed_out { elapsed; _ } ->
      Some (Printf.sprintf "hang: no reply after %.1f s" elapsed)
  | Isolate.Raised { message; _ } -> Some ("exception: " ^ message)
  | Isolate.Crashed { detail; _ } -> Some ("crash: " ^ detail)
  | Isolate.Returned _ ->
      let reasons = ref [] in
      if proven case then ()
      else if not case.comparison.Outline.identity_ok then
        reasons :=
          ("outline changed: " ^ case.comparison.Outline.detail) :: !reasons
      else if not case.comparison.Outline.span_ok then
        reasons :=
          ("outline moved: " ^ case.comparison.Outline.detail) :: !reasons;
      (match errors case with
      | Some count when single_token case && count > 2 ->
          reasons := Printf.sprintf "%d errors" count :: !reasons
      | _ -> ());
      if !reasons = [] then None
      else Some (String.concat "; " (List.rev !reasons))

let failures ~limit cases =
  let outside =
    List.filter_map
      (fun case ->
        match failure case with
        | None -> None
        | Some reason -> Some (case, reason))
      cases
  in
  Printf.printf "## cases outside the criteria\n\n";
  if outside = [] then Printf.printf "  none\n\n"
  else (
    Printf.printf "  %d cases\n\n" (List.length outside);
    let printed = ref 0 in
    List.iter
      (fun (case, reason) ->
        if limit <= 0 || !printed < limit then (
          incr printed;
          Printf.printf "  %s: %s: %s\n" case.path case.mutation.Mutant.label
            reason))
      outside;
    if limit > 0 && List.length outside > limit then
      Printf.printf
        "\n  and %d more not printed (--list-all prints every one)\n"
        (List.length outside - limit);
    Printf.printf "\n")

(** The sites each family had, and how many of them were used. *)
let sites rows =
  Printf.printf "## mutation sites\n\n";
  Printf.printf "  %-10s %8s %10s\n" "family" "used" "available";
  List.iter
    (fun (family, used, available) ->
      Printf.printf "  %-10s %8d %10d\n"
        (Mutant.family_name family)
        used available)
    rows;
  Printf.printf "\n"

let starts_with ~prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

(** The reason a file was left out, with its number collapsed, so that a
    hundred files with one error each print as one line. *)
let reason_kind reason =
  if starts_with ~prefix:"cannot be read" reason then "cannot be read"
  else if reason = "fewer than three tokens" then "fewer than three tokens"
  else if reason = "fewer than two items to compare" then
    "fewer than two items to compare"
  else if starts_with ~prefix:"the unmutated file has" reason then
    "the unmutated file has errors"
  else if starts_with ~prefix:"the unmutated file raises" reason then
    "the unmutated file raises an exception"
  else if starts_with ~prefix:"the unmutated file hangs" reason then
    "the unmutated file hangs"
  else if starts_with ~prefix:"the unmutated file crashes" reason then
    "the unmutated file crashes"
  else "other"

let files ?(skipped = []) paths =
  Printf.printf "## files sampled\n\n";
  List.iter (fun path -> Printf.printf "  %s\n" path) paths;
  if skipped <> [] then (
    Printf.printf "\n## files not sampled\n\n";
    let kinds =
      List.sort_uniq compare
        (List.map (fun (_, reason) -> reason_kind reason) skipped)
    in
    List.iter
      (fun kind ->
        let count =
          List.length
            (List.filter (fun (_, reason) -> reason_kind reason = kind) skipped)
        in
        Printf.printf "  %-40s %5d files\n" kind count)
      kinds;
    Printf.printf "\n  first 20:\n";
    List.iteri
      (fun index (path, reason) ->
        if index < 20 then Printf.printf "    %s: %s\n" path reason)
      skipped;
    if List.length skipped > 20 then
      Printf.printf "    ... and %d more\n" (List.length skipped - 20));
  Printf.printf "\n"

let total_errors cases =
  List.fold_left
    (fun total case ->
      match errors case with Some count -> total + count | None -> total)
    0 cases

let summary ~ran cases =
  Printf.printf "## summary\n\n";
  Printf.printf "  %d mutations, %d errors in all, %.1f s of wall clock\n"
    (List.length cases) (total_errors cases) ran;
  Printf.printf "\n"

(** The cases proven impossible, by the mutation that makes the witness and
    by what the two criteria disagree on, then one line each. *)
let impossible cases =
  let proofs =
    List.filter_map
      (fun case ->
        match case.proof with
        | Some witness when proven case -> Some (case, witness)
        | _ -> None)
      cases
  in
  Printf.printf "## cases proven impossible for any parser\n\n";
  if proofs = [] then Printf.printf "  none\n\n"
  else
    let classes =
      List.sort_uniq compare
        (List.map
           (fun (_, (witness : Witness.t)) ->
             (Witness.family_name witness.family, witness.difference))
           proofs)
    in
    Printf.printf "  %-12s %-36s %6s\n" "witness by"
      "what the two outlines need" "cases";
    List.iter
      (fun (family, difference) ->
        let count =
          List.length
            (List.filter
               (fun (_, (witness : Witness.t)) ->
                 Witness.family_name witness.family = family
                 && witness.difference = difference)
               proofs)
        in
        Printf.printf "  %-12s %-36s %6d\n" family
          (Witness.difference_name difference)
          count)
      classes;
    Printf.printf "  %-12s %-36s %6d\n\n" "total" "" (List.length proofs);
    List.iter
      (fun (case, witness) ->
        Printf.printf "  %s: %s: %s\n" case.path case.mutation.Mutant.label
          (Witness.describe witness))
      proofs;
    Printf.printf "\n"
