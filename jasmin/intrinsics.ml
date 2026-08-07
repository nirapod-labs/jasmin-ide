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

(* [CoreArchFactory.get_arch_module] is the library's own architecture
   table: [Main_compiler] uses it to pick the module whose [asmOp_sopn] it
   prints for [-help-intrinsics], and [Arch.check] uses it to type
   intrinsics. Reading [prim_string] from it therefore covers exactly the
   architectures the linked compiler accepts. *)
let library_arch_of = function
  | Arch.X86_64 -> Utils.X86_64
  | Arch.Arm_m4 -> Utils.ARM_M4
  | Arch.Arm_v8a -> Utils.ARMv8A
  | Arch.Riscv -> Utils.RISCV

