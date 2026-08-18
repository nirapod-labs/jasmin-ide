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

let suffixes_of ~arch name =
  match Arch.of_string arch with
  | None -> []
  | Some arch -> (
      let (module A) = module_of_arch arch in
      match List.assoc_opt name (Sopn.prim_string A.asmOp_sopn) with
      | None -> []
      | Some constructor -> suffixes_of_constructor constructor)

(* The operation a written suffix names, or [None] when [name] does not
   take it. The suffix carries no leading underscore; [""] means none was
   written. *)
let resolve constructor ~name ~suffix =
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
      (* [Tt_arm_m4.get_arm_prim] strips a trailing ["cc"] and then a
         trailing ["S"], so the suffix is read the same way here; what is
         left must be the table's own spelling of the name. *)
      let strip ending word =
        let ending_length = String.length ending
        and word_length = String.length word in
        if
          word_length >= ending_length
          && String.equal
               (String.sub word (word_length - ending_length) ending_length)
               ending
        then Some (String.sub word 0 (word_length - ending_length), true)
        else Some (word, false)
      in
      match strip "cc" suffix with
      | None -> None
      | Some (without_conditional, conditional) -> (
          match strip "S" without_conditional with
          | None -> None
          | Some (without_flags, set_flags) -> (
              if not (String.equal without_flags name) then None
              else
                match make set_flags conditional with
                | Error _ -> None
                | Ok op -> Some op)))

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
          match resolve constructor ~name ~suffix with
          | None -> None
          | Some op ->
              Some
                (signature_of_description (Sopn.asm_op_instr A.asmOp_sopn op))))

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
