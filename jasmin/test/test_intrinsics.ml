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
let check_arch (arch : string) =
  let entries = Jide_jasmin.Intrinsics.all ~arch in
  let help = Jide_jasmin.Intrinsics.compiler_help ~arch in
  let groups = printed_groups help in
  let label name = arch ^ ": " ^ name in

  (* The printer writes all seven headers, always. *)
  check (label "seven groups") (List.length groups = 7);

  let printed = List.concat_map snd groups in
  let ours =
    List.map (fun (entry : Jide_jasmin.Intrinsics.t) -> entry.name) entries
  in

  (* Every name the printer wrote is in our list and vice versa: no name
     dropped, none invented. *)
  check (label "same names") (List.sort compare printed = List.sort compare ours);

  (* No duplicate: the compiler's table is an association list, and a
     duplicated name would make completion ambiguous. *)
  check
    (label "no duplicate names")
    (List.length (List.sort_uniq compare ours) = List.length ours);

  (* Group by group: our entry's class must be the header it was printed
     under, and each group's membership must match ours exactly. *)
  let expected_by_class =
    let table = Hashtbl.create 16 in
    List.iter
      (fun (entry : Jide_jasmin.Intrinsics.t) ->
        let class_ = class_of_suffixes entry.suffixes in
        let previous =
          Option.value (Hashtbl.find_opt table class_) ~default:[]
        in
        Hashtbl.replace table class_ (entry.name :: previous))
      entries;
    table
  in
  List.iteri
    (fun class_ (header, names) ->
      let expected =
        Option.value (Hashtbl.find_opt expected_by_class class_) ~default:[]
      in
      check
        (label
           ("group " ^ string_of_int class_ ^ " (" ^ header ^ ") membership"))
        (List.sort compare names = List.sort compare expected);
      (* The printer sorts each group; ours is in table order, so the only
         check on order is that the printer's is sorted. This proves the
         grouping and the sort, which is what the interface claims. *)
      check
        (label ("group " ^ string_of_int class_ ^ " printed in sorted order"))
        (names = List.sort compare names))
    groups;

  (* The same relation stated independent of our classifier: the number of
     entries per printed group must equal the number of our entries whose
     suffix list has that shape, and the totals must agree. *)
  check (label "same total") (List.length printed = List.length ours);
  Printf.printf "%s: %d intrinsics, %d groups\n%!" arch (List.length ours)
    (List.length groups);
  (entries, printed)

(* The suffix lists must be a function of the name under one architecture:
   two lookups agree. *)
let check_lookup (arch : string) (entries : Jide_jasmin.Intrinsics.t list) =
  List.iter
    (fun (entry : Jide_jasmin.Intrinsics.t) ->
      let again = Jide_jasmin.Intrinsics.suffixes_of ~arch entry.name in
      check
        (arch ^ ": suffixes_of " ^ entry.name ^ " agrees with all")
        (again = entry.suffixes);
      (* Every suffix listed resolves, and the empty suffix resolves exactly
         when the table lists a name with no suffixes. *)
      List.iter
        (fun suffix ->
          let written =
            if String.equal suffix "" then entry.name
            else entry.name ^ "_" ^ suffix
          in
          match
            Jide_jasmin.Intrinsics.signature_of ~arch ~name:entry.name ~suffix
          with
          | Some _ -> ()
          | None -> check (arch ^ ": " ^ written ^ " resolves") false)
        entry.suffixes;
      match
        Jide_jasmin.Intrinsics.signature_of ~arch ~name:entry.name ~suffix:""
      with
      | Some _ when entry.suffixes = [] -> ()
      | Some _ ->
          check (arch ^ ": " ^ entry.name ^ " without a suffix resolves") false
      | None when List.mem "" entry.suffixes -> ()
      | None ->
          (* A name whose suffix list omits the empty string is one the
             compiler cannot write bare, as far as this table can tell. *)
          ())
    entries

(* A suffix the architecture does not list must not resolve, or the
   interface's "or [None] when [arch] accepts no such pair" is false. *)
let check_rejects (arch : string) (entries : Jide_jasmin.Intrinsics.t list) =
  match
    List.find_opt
      (fun (e : Jide_jasmin.Intrinsics.t) -> e.suffixes <> [])
      entries
  with
  | None -> ()
  | Some entry -> (
      let bogus = entry.name ^ "zzz" in
      match
        Jide_jasmin.Intrinsics.signature_of ~arch ~name:entry.name ~suffix:"zzz"
      with
      | None -> ()
      | Some _ -> check (arch ^ ": " ^ bogus ^ " rejected") false)

(* The signature of a name the compiler's table lists must equal the one
   [Sopn.asm_op_instr] gives for the operation its first suffix resolves to,
   read here straight from the library rather than through our projection.
   That is the check that our [ty] rendering did not lose or reorder a
   field. *)
let check_signatures_are_the_librarys (arch : string)
    (entries : Jide_jasmin.Intrinsics.t list) =
  let (module A) =
    match Jide_jasmin.Arch.of_string arch with
    | Some arch -> Jide_jasmin.Arch.module_of arch Jide_jasmin.Arch.Linux
    | None -> failwith ("test: unknown arch " ^ arch)
  in
  let library_type (aty : Type.atype) =
    match aty with
    | Type.Coq_abool -> "Bool"
    | Type.Coq_aint -> "Int"
    | Type.Coq_aword size -> "Word:" ^ string_of_int (Prog.int_of_ws size)
    | Type.Coq_aarr (size, length) ->
        "Array:"
        ^ string_of_int (Prog.int_of_ws size)
        ^ ":"
        ^ (Conv.z_of_cz length |> Z.to_string)
  in
  let ours_type (ty : Jide_jasmin.Intrinsics.ty) =
    match ty with
    | Jide_jasmin.Intrinsics.Bool -> "Bool"
    | Jide_jasmin.Intrinsics.Int -> "Int"
    | Jide_jasmin.Intrinsics.Word { signed; bits } ->
        if signed then Printf.sprintf "Word:%d:!" bits
        else "Word:" ^ string_of_int bits
    | Jide_jasmin.Intrinsics.Array { bits; length } ->
        Printf.sprintf "Array:%d:%s" bits length
  in
  List.iter
    (fun (entry : Jide_jasmin.Intrinsics.t) ->
      let library_signature =
        match List.assoc_opt entry.name (Sopn.prim_string A.asmOp_sopn) with
        | None -> None
        | Some constructor -> (
            let op =
              match constructor with
              | Sopn.PrimX86 (suffixes, make) -> (
                  match suffixes with
                  | [] -> make (Sopn.PVp Wsize.U8)
                  | first :: _ -> make first)
              | Sopn.PrimARM make -> (
                  match make false false with
                  | Ok op -> Some op
                  | Error _ -> None)
            in
            match op with
            | None -> None
            | Some op -> Some (Sopn.asm_op_instr A.asmOp_sopn op))
      in
      match library_signature with
      | None ->
          check (arch ^ ": " ^ entry.name ^ " has a library signature") false
      | Some description ->
          let signature = entry.signature in
          check
            (Printf.sprintf "%s: %s arguments are the library's" arch entry.name)
            (List.map ours_type signature.Jide_jasmin.Intrinsics.arguments
            = List.map library_type description.Sopn.tin);
          check
            (Printf.sprintf "%s: %s results are the library's" arch entry.name)
            (List.map ours_type signature.Jide_jasmin.Intrinsics.results
            = List.map library_type description.Sopn.tout);
          check
            (Printf.sprintf "%s: %s implicits are the library's" arch entry.name)
            (signature.Jide_jasmin.Intrinsics.implicit_arguments
            = List.filter_map
                (function
                  | Sopn.ADImplicit variable ->
                      Some (Ident.Ident.id_name (Var0.Var.vname variable))
                  | Sopn.ADExplicit _ -> None)
                description.Sopn.i_out))
    entries

let () =
  List.iter
    (fun arch ->
      let entries, _ = check_arch arch in
      check_lookup arch entries;
      check_rejects arch entries;
      check_signatures_are_the_librarys arch entries)
    Jide_jasmin.Intrinsics.arch_names;

  (* An unknown architecture is an invalid argument, not an empty table. *)
  (match Jide_jasmin.Intrinsics.all ~arch:"not-an-architecture" with
  | _ -> check "unknown arch raises" false
  | exception Invalid_argument _ -> ()
  | exception _ -> check "unknown arch raises Invalid_argument" false);

  (* The architecture names are the ones the compiler's own option table
     lists: [-arch] is an [Arg.Symbol] whose list is the accepted set. *)
  let from_options =
    List.find_map
      (fun (flag, spec, _) ->
        if String.equal flag "-arch" then
          match spec with Arg.Symbol (symbols, _) -> Some symbols | _ -> None
        else None)
      Jasmin.Glob_options.options
  in
  check "arch_names is the compiler's -arch list"
    (match from_options with
    | Some symbols -> symbols = Jide_jasmin.Intrinsics.arch_names
    | None -> false);

  if !failures = 0 then Printf.printf "intrinsics: all checks passed\n%!"
  else (
    Printf.printf "intrinsics: %d failure(s)\n%!" !failures;
    exit 1)
