(* The oracle test for the intrinsic table.

   The interface promises that [Intrinsics.all ~arch] is every intrinsic the
   linked Jasmin accepts for [arch], in the order the compiler's table lists
   them, and that [Intrinsics.compiler_help ~arch] is exactly the bytes
   [Help.show_intrinsics] writes. This test holds both promises by
   re-deriving each architecture's list from the compiler's own printer,
   in-process, and comparing.

   What the comparison is worth: [Help.show_intrinsics] reads the same
   [Sopn.prim_string] field [Intrinsics.all] reads, so the comparison
   proves the projection -- same names, none dropped, none invented, no
   duplicate -- and the grouping [all] claims to preserve. It cannot prove
   that the compiler's table itself is right, because there is nothing
   above it to consult; that the field is the compiler's own is the point
   of reading it instead of a transcription.

   The printer groups by suffix class and sorts each group, so the check is
   on the multiset of names per group, plus the count of every group, and
   the total. A name that moved between groups, appeared twice, or vanished
   fails one of them. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* The names each of the printer's seven group headers introduces, in
   printed order, together with the header that introduced them. The
   printer writes one header per group whether or not the group is empty,
   then one [" - name"] line per name. *)
let printed_groups help =
  let lines = String.split_on_char '\n' help in
  let groups = ref [] and current = ref None and names = ref [] in
  let flush () =
    match !current with
    | None -> ()
    | Some header ->
        groups := (header, List.rev !names) :: !groups;
        names := []
  in
  List.iter
    (fun line ->
      let line = String.trim line in
      if String.length line > 2 && String.equal (String.sub line 0 2) "- " then
        names := String.sub line 2 (String.length line - 2) :: !names
      else if line <> "" then (
        flush ();
        current := Some line))
    lines;
  flush ();
  List.rev !groups

(* The suffix class the compiler's printer sorts a name into. This mirrors
   the [index] function of [help.ml], which is the compiler's own grouping,
   so the test can say which group each of our entries belongs to and
   compare it with what the printer actually wrote. *)
let printed_class (constructor : _ Sopn.prim_constructor) =
  match constructor with
  | Sopn.PrimARM _ -> 6
  | Sopn.PrimX86 (suffixes, _) -> (
      match suffixes with
      | [] -> 0
      | Sopn.PVp _ :: _ -> 1
      | Sopn.PVs _ :: _ -> 2
      | Sopn.PVx _ :: _ -> 3
      | Sopn.PVv _ :: _ | Sopn.PVsv _ :: _ -> 4
      | Sopn.PVvv _ :: _ -> 5)

(* The class of one of our own entries, read back from its suffixes. This is
   the part that would catch a wrong suffix rendering: a [PVp] printed as
   ["64"] and a [PVs] printed as ["s64"] must land in different groups, and
   they do only if the rendering round-trips through the classifier. *)
let class_of_suffixes = function
  | [] -> 0
  | suffix :: _ -> (
      (* A plain size suffix is the compiler's [PVp]; one that starts with
         "s" or "u" and is otherwise digits is [PVs]; a vector suffix is
         one carrying the "_ve_sz" or "ve_sz_ve_sz" shape; and an
         extension suffix names two sizes joined by a second sign. *)
      let is_digits s =
        String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s
      in
      match String.split_on_char '_' suffix with
      | [ one ] when is_digits one -> 1
      | [ one ]
        when String.length one > 1
             && (one.[0] = 's' || one.[0] = 'u')
             && is_digits (String.sub one 1 (String.length one - 1)) ->
          2
      | [ one; two ]
        when (String.length one > 1 && (one.[0] = 's' || one.[0] = 'u'))
             && String.length two > 1
             && (two.[0] = 's' || two.[0] = 'u') ->
          3
      | [ one ] -> 4
      | [ _; _ ] -> 5
      | _ -> 4)

(* One architecture's checks. *)
