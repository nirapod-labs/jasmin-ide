(* The intrinsic table of the linked Jasmin, read from the library's own
   data rather than transcribed.

   The compiler keeps two tables per architecture. [Arch.asmOp] is the
   assembly-operation table: its [prim_string] field is the association
   list of an intrinsic's name and the constructor that completes a
   written suffix into a concrete operation. [Arch.asmOp_sopn] is that
   same table lifted over [Sopn.sopn], and is what the compiler hands to
   [Help.show_intrinsics] and to [Pretyping.tt_prim]. The lift re-exports
   the underlying [prim_string] unchanged, so reading it here gives exactly
   the names and suffix descriptors the compiler accepts.

   Signatures come from [Sopn.asm_op_instr], the field [Pretyping.prim_sig]
   maps into a typing judgement. Suffixes are rendered with the library's
   own printers, so a name the table spells the compiler's way is spelled
   the same way here. *)

open Jasmin

(* -------------------------------------------------------------------- *)
(* The table's shape. The interface documents it; the definitions are
   here so the rest of the file can name them. *)

type ty =
  | Bool
  | Int
  | Word of { signed : bool; bits : int }
  | Array of { bits : int; length : string }

type t_signature = {
  arguments : ty list;
  results : ty list;
  implicit_arguments : string list;
}

type t = { name : string; suffixes : string list; signature : t_signature }

(* -------------------------------------------------------------------- *)
(* The architectures.                                                     *)

(* [Arch.module_of] is the library's own architecture table, resolved through
   the module that holds the one difference between the two channels:
   [Main_compiler] uses the same table to pick the module whose [asmOp_sopn] it
   prints for [-help-intrinsics], and [Arch.check] uses it to type intrinsics.
   Reading [prim_string] from it therefore covers exactly the architectures the
   linked compiler accepts. *)
let module_of_arch (arch : Arch.t) : (module Arch_full.Arch) =
  Arch.module_of arch Arch.Linux

(* -------------------------------------------------------------------- *)
(* Rendering a suffix the compiler's way.                                 *)

(* [Pretyping.extract_size] reads a written suffix back to a
   [Sopn.prim_x86_suffix]; its inverse [Pretyping.pp_suffix] is not
   exported, but the three printers it is built from are:
   [Wsize.string_of_wsize], [PrintCommon.string_of_signess] and
   [PrintCommon.string_of_velem]. Spelling a suffix from its constructor is
   therefore the library's own spelling, not a second one. *)
let wsize = Wsize.string_of_wsize

let suffix_string (suffix : Sopn.prim_x86_suffix) =
  let open Sopn in
  match suffix with
  | PVp size -> "_" ^ wsize size
  | PVs (signedness, size) ->
      "_" ^ PrintCommon.string_of_signess signedness ^ wsize size
  | PVv (element, size) ->
      "_" ^ PrintCommon.string_of_velem Wsize.Unsigned size element
  | PVsv (signedness, element, size) ->
      "_" ^ PrintCommon.string_of_velem signedness size element
  | PVx (size_of, size_to) -> "_u" ^ wsize size_of ^ "u" ^ wsize size_to
  | PVvv (element, size, element', size') ->
      "_"
      ^ PrintCommon.string_of_velem Wsize.Unsigned size element
      ^ "_"
      ^ PrintCommon.string_of_velem Wsize.Unsigned size' element'

(* The suffixes an ARM intrinsic accepts. [PrimARM] carries no list: its
   constructor takes the two flags [Tt_arm_m4.tt_prim] derives from the
   written name, after stripping a trailing ["cc"] and then a trailing
   ["S"], and answers [Error] for a combination the mnemonic does not
   support. So the accepted spellings are the ones it answers [Ok] for. *)
let arm_suffixes (make : bool -> bool -> (string, 'op) Utils0.result) =
  List.filter_map
    (fun (set_flags, conditional) ->
      match make set_flags conditional with
      | Error _ -> None
      | Ok _ ->
          Some
            ((if set_flags then "S" else "") ^ if conditional then "cc" else ""))
    [ (false, false); (true, false); (false, true); (true, true) ]

(* -------------------------------------------------------------------- *)
(* Signatures.                                                            *)

let ty_of_atype (aty : Type.atype) =
  match aty with
  | Type.Coq_abool -> Bool
  | Type.Coq_aint -> Int
  | Type.Coq_aword size -> Word { signed = false; bits = Prog.int_of_ws size }
  | Type.Coq_aarr (size, length) ->
      (* The length is the library's own placeholder, which [copy] carries
         as zero for insertion to fill in; it is reported as it stands
         rather than resolved to a number the library does not have. *)
      Array
        {
          bits = Prog.int_of_ws size;
          length = Conv.z_of_cz length |> Z.to_string;
        }

(* The registers and flags the compiler adds to a call, in the order
   [instruction_desc.i_out] lists them: [PrimARM]'s flags come first, as
   the interface documents. *)
let implicits (descriptions : Sopn.arg_desc list) =
  List.filter_map
    (function
      | Sopn.ADImplicit variable ->
          Some (Ident.Ident.id_name (Var0.Var.vname variable))
      | Sopn.ADExplicit _ -> None)
    descriptions

let signature_of_description (description : Sopn.instruction_desc) =
  {
    arguments = List.map ty_of_atype description.Sopn.tin;
    results = List.map ty_of_atype description.Sopn.tout;
    implicit_arguments = implicits description.Sopn.i_out;
  }

(* -------------------------------------------------------------------- *)
(* One entry.                                                             *)

(* The operation an intrinsic resolves to when no suffix is written. For
   [PrimX86] the constructor decides whether the empty suffix is accepted:
   [primM], used for a name whose list is empty, answers [Some] for any
   argument, and [primP] for the others is handed the size the compiler
   itself defaults to ([Pretyping.default_suffix] is the list's first
   element). *)
let first_op (constructor : 'op Sopn.prim_constructor) =
  match constructor with
  | Sopn.PrimX86 (suffixes, make) -> (
      match suffixes with
      | [] -> make (Sopn.PVp Wsize.U8)
      | first :: _ -> make first)
  | Sopn.PrimARM make -> (
      match make false false with Ok op -> Some op | Error _ -> None)

(* The interface writes a suffix without the leading underscore, because a
   caller writes [name], then ["_"], then [suffix], and the empty string is the
   bare name; the ARM rows below are already in that spelling. The compiler's
   own printer writes the underscore ([Pretyping.pp_suffix]), so it is dropped
   here, in the one place the two spellings meet. *)
let without_underscore (suffix : string) =
  if String.length suffix > 0 && suffix.[0] = '_' then
    String.sub suffix 1 (String.length suffix - 1)
  else suffix

let suffixes_of_constructor (constructor : 'op Sopn.prim_constructor) =
  match constructor with
  | Sopn.PrimX86 (suffixes, _) ->
      List.map
        (fun suffix -> without_underscore (suffix_string suffix))
        suffixes
  | Sopn.PrimARM make -> arm_suffixes make

let entries_of (module A : Arch_full.Arch) ~arch =
  List.map
    (fun (name, constructor) ->
      let suffixes = suffixes_of_constructor constructor in
      let signature =
        match first_op constructor with
        | None ->
            (* The compiler lists a name from [prim_string] whether or not
               its own constructor resolves it, so this is possible in
               principle. Such a name would be one the compiler prints but
               cannot type; saying so beats inventing a signature. Every
               name of the architectures the linked Jasmin has resolves, so
               this is unreachable today. *)
            invalid_arg
              ("Jide_jasmin.Intrinsics: " ^ arch ^ " lists " ^ name
             ^ " but its constructor resolves no operation")
        | Some op ->
            signature_of_description (Sopn.asm_op_instr A.asmOp_sopn op)
      in
      { name; suffixes; signature })
    (Sopn.prim_string A.asmOp_sopn)

(* -------------------------------------------------------------------- *)
(* The interface.                                                         *)

let module_of ~arch =
  match Arch.of_string arch with
  | Some arch -> module_of_arch arch
  | None -> invalid_arg ("Jide_jasmin.Intrinsics: unknown arch " ^ arch)

let all ~arch = entries_of (module_of ~arch) ~arch
let arch_names = Arch.names

(* The names [all ~arch] lists, in the same order and without the suffix
   resolution: the keys of the table, which is all completion needs. *)
let names ~arch =
  let (module A) = module_of ~arch in
  List.map fst (Sopn.prim_string A.asmOp_sopn)

let suffixes_of ~arch name =
  match Arch.of_string arch with
  | None -> []
  | Some arch -> (
      let (module A) = module_of_arch arch in
      match List.assoc_opt name (Sopn.prim_string A.asmOp_sopn) with
      | None -> []
      | Some constructor -> suffixes_of_constructor constructor)

(* The operation a suffix names, or [None] when the constructor does not take
   it. The suffix carries no leading underscore; [""] means none was written. *)
let op_of_suffix constructor ~suffix =
  match constructor with
  | Sopn.PrimX86 (suffixes, make) -> (
      match suffix with
      | "" -> first_op constructor
      | written -> (
          let wanted = "_" ^ written in
          match
            List.find_opt
              (fun candidate -> String.equal (suffix_string candidate) wanted)
              suffixes
          with
          | None -> None
          | Some candidate -> make candidate))
  | Sopn.PrimARM make -> (
      (* An ARM suffix is one of the flag and condition spellings
         [arm_suffixes] lists: ["S"] sets the flags, ["cc"] makes the
         operation conditional, ["Scc"] does both and [""] does neither. The
         name is not part of the suffix, so it is not compared here. *)
      let set_flags = List.mem suffix [ "S"; "Scc" ]
      and conditional = List.mem suffix [ "cc"; "Scc" ] in
      if not (List.mem suffix [ ""; "S"; "cc"; "Scc" ]) then None
      else
        match make set_flags conditional with
        | Error _ -> None
        | Ok op -> Some op)

(* [signature_of] resolves the pair the way [Pretyping.tt_prim] would, so a
   pair it reports is one the compiler accepts. *)
let signature_of ~arch ~name ~suffix =
  match Arch.of_string arch with
  | None -> None
  | Some arch -> (
      let (module A) = module_of_arch arch in
      match List.assoc_opt name (Sopn.prim_string A.asmOp_sopn) with
      | None -> None
      | Some constructor -> (
          match op_of_suffix constructor ~suffix with
          | None -> None
          | Some op ->
              Some
                (signature_of_description (Sopn.asm_op_instr A.asmOp_sopn op))))

(* The spelling a program writes for one concrete intrinsic. A [PrimX86] name
   takes an underscore before a non-empty suffix ([ADD_64]); a [PrimARM] name
   takes its flag and condition letters on its end with nothing between
   ([ADDScc]), and the empty suffix is the bare name. The constructor decides,
   so the spelling is the one the table's own name accepts, not a guess made
   from the architecture. *)
let written_of ~arch ~name ~suffix =
  match Arch.of_string arch with
  | None -> None
  | Some arch -> (
      let (module A) = module_of_arch arch in
      match List.assoc_opt name (Sopn.prim_string A.asmOp_sopn) with
      | None -> None
      | Some (Sopn.PrimX86 _) ->
          Some (if String.equal suffix "" then name else name ^ "_" ^ suffix)
      | Some (Sopn.PrimARM _) -> Some (name ^ suffix))

(* The written name, resolved by the compiler's own function. [tt_prim] takes
   the name as the parser keeps it, without the [\#], and returns the
   operation the compiler types; it reads the suffix table through
   [extract_size], simplifies a vector suffix the way [simplify_suffix]
   does and hands an arm name to [Tt_arm_m4.tt_prim], so nothing about the
   split is written a second time here. The location is the dummy one: a
   refusal is turned into [None] at once and nothing prints it, and the two
   warnings the function can emit are captured rather than shown.

   A name the table does not hold reaches the arm rule as well, which
   answers [Error] for it, and [tt_prim] turns that into a typing error that
   is caught here. So the only thing this can answer for garbage is [None].

   The description is taken inside the same function the operation is read
   in: an operation of [A.asm_op] names the module's type, so a value of it
   cannot leave the [let (module A) = ...] that introduced [A]. *)
let resolve_in (module A : Arch_full.Arch) (written : string) =
  let id = Location.mk_loc Location._dummy written in
  match Warnings.with_capture (fun () -> Pretyping.tt_prim A.asmOp_sopn id) with
  | exception Pretyping.TyError _ -> None
  | op, _warnings ->
      Some (signature_of_description (Sopn.asm_op_instr A.asmOp_sopn op))

let resolve ~arch (written : string) =
  match Arch.of_string arch with
  | None -> None
  | Some arch ->
      let (module A) = module_of_arch arch in
      resolve_in (module A) written

(* -------------------------------------------------------------------- *)
(* The compiler's own rendering.                                          *)

(* [Help.show_intrinsics] takes the formatter to print to, but the
   release's [help.mli] exports only the [unit -> unit] form, which writes
   to [Format.std_formatter]. Its output functions are swapped for a buffer
   for the length of the call and put back afterwards, so the bytes
   captured are the bytes the compiler would have written. *)
let compiler_help ~arch =
  let (module A) = module_of ~arch in
  let buffer = Buffer.create 4096 in
  let standard = Format.std_formatter in
  let saved = Format.pp_get_formatter_out_functions standard () in
  let capture =
    {
      Format.out_string =
        (fun bytes at length -> Buffer.add_substring buffer bytes at length);
      out_flush = (fun () -> ());
      out_newline = (fun () -> Buffer.add_char buffer '\n');
      out_spaces =
        (fun count -> Buffer.add_string buffer (String.make count ' '));
      out_indent =
        (fun count -> Buffer.add_string buffer (String.make count ' '));
    }
  in
  Format.pp_set_formatter_out_functions standard capture;
  Fun.protect
    ~finally:(fun () -> Format.pp_set_formatter_out_functions standard saved)
    (fun () ->
      Help.show_intrinsics A.asmOp_sopn ();
      Format.pp_print_flush standard ();
      Buffer.contents buffer)
