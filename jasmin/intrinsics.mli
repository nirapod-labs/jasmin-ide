(** Every intrinsic the linked Jasmin accepts, per architecture.

    The data comes from the library's own tables, not from a transcription:
    each architecture's [Sopn.prim_string] row gives the name and the
    constructor that maps a suffix to the concrete operation, and
    [Sopn.asm_op_instr] gives that operation's signature. Nothing is
    hard-coded, so a Jasmin release that adds, drops or renames an intrinsic
    changes this table without a change here.

    The architectures are the library's own: {!arch_names} is
    {!Arch.names}, and each table is read through [Arch.asmOp_sopn], the
    value [Main_compiler] hands to [Help.show_intrinsics] and the one
    [Arch.check] types intrinsics against. This is the reachable path, not a
    fallback: the library exports the architecture table {!Arch.module_of}
    reads, and it returns an [Arch_full.Arch] whose [asmOp_sopn] is a public
    field. *)

(** What a result or an argument of an intrinsic is. *)
type ty =
  | Bool  (** a flag or a condition *)
  | Int  (** a wide integer, as [copy]'s length *)
  | Word of { signed : bool; bits : int }
      (** a word of [bits] bits; [signed] marks a signed view *)
  | Array of { bits : int; length : string }
      (** an array whose elements are [bits] bits wide *)

type t = {
  name : string;  (** the name as the compiler's table spells it *)
  suffixes : string list;
      (** The suffixes that complete [name] to an accepted operator, in the
          order the table lists them, without the leading underscore:
          ["64"; "u16u8"; "4u64"; "Scc"]. Empty when the name takes none.
          This is what completion offers after [name ^ "_"].

          For an x86 table this is [PrimX86]'s suffix list. For an ARM one
          [PrimARM] carries no list, so each of the four flag combinations
          [Tt_arm_m4.tt_prim] can derive is asked of the constructor and
          only the ones it answers [Ok] for are listed. The empty string,
          present exactly when the plain mnemonic resolves, means the bare
          name. *)
  signature : t_signature;
      (** what the intrinsic takes and returns when no suffix is written,
          or, for a name with suffixes, for the first suffix. *)
}
(** One intrinsic a program may name. *)

and t_signature = {
  arguments : ty list;  (** the explicit arguments, in order *)
  results : ty list;  (** the explicit results, in order *)
  implicit_arguments : string list;
      (** Registers and flags the compiler adds and does not write in the
          call, by the compiler's own spelling. Flags come first, as in the
          library's [instruction_desc.i_out]. *)
}

val all : arch:string -> t list
(** [all ~arch] is every intrinsic the linked Jasmin accepts for [arch],
    in the order the compiler's table lists them. [Help.show_intrinsics]
    re-sorts by its own suffix class before printing, so the relation to
    [jasminc -arch <arch> -help-intrinsics] is: same names, grouped and
    sorted by the compiler's printer. {!compiler_help} returns exactly the
    bytes that printer writes, and the test compares the two.

    Raises [Invalid_argument] when [arch] is not one of {!arch_names}. *)

val arch_names : string list
(** The architecture names the linked Jasmin accepts: exactly the strings
    [jasminc -arch] takes, in the order its option table lists them. *)

val names : arch:string -> string list
(** [names ~arch] is the name of every intrinsic {!all} lists for [arch], in
    the same order, without the suffixes. It is what completion offers after
    [#].

    Raises [Invalid_argument] when [arch] is not one of {!arch_names}. *)

val suffixes_of : arch:string -> string -> string list
(** [suffixes_of ~arch name] is the suffix list of [name] under [arch], or
    [[]] when [arch]'s table does not name it. *)

val signature_of :
  arch:string -> name:string -> suffix:string -> t_signature option
(** [signature_of ~arch ~name ~suffix] resolves one concrete intrinsic and
    returns its signature, or [None] when [arch] accepts no such pair. The
    suffix carries no leading underscore; [""] means none was written. *)

val written_of : arch:string -> name:string -> suffix:string -> string option
(** [written_of ~arch ~name ~suffix] is the spelling a program writes after
    the [\#] for the intrinsic [name] with [suffix], or [None] when [arch]'s
    table names no such [name]. An x86 name takes an underscore before a
    non-empty suffix, as in ["ADD_64"]; an ARM name takes its flag and
    condition letters on its end, as in ["ADDScc"], and the empty suffix is
    the bare name. The suffix carries no leading underscore. *)

val resolve : arch:string -> string -> t_signature option
(** [resolve ~arch written] is the signature of the intrinsic a program
    writes, [written] being the name after the [\#] with its suffix when it
    has one: ["ADD_64"], ["MOVZX_u64u32"], ["ADDcc"].

    The split between the name and the suffix is not made here: it is made
    by the compiler. [Pretyping.tt_prim] is the function [Pretyping] calls
    for a [\#] expression, and it holds the suffix table, the vector-suffix
    simplification and the arm rule that strips [S] and [cc], so a name this
    answers is one the compiler types, and the description is the one
    [Sopn.asm_op_instr] gives the operation the compiler picked. A name it
    refuses is one the compiler refuses. The warnings that resolution can
    print (a deprecated name, a simplified vector suffix) are captured and
    dropped rather than written to the process's error stream.

    [None] means [arch]'s table names no such intrinsic, which includes
    every form the compiler types by a rule of its own instead of a table
    row ([#spill], [#unspill], [#randombytes], [#declassify]): those have no
    instruction description, so there is no signature to give. It does not
    raise. *)

val compiler_help : arch:string -> string
(** The compiler's own rendering of the intrinsic table for [arch], exactly
    the bytes [Help.show_intrinsics] writes. The test compares {!all} against
    it; a tool may show it as documentation.

    The release's [help.mli] exports only the [unit -> unit] form, which
    writes to [Format.std_formatter]; this swaps that formatter's output
    functions for a buffer and puts them back, so the capture is
    in-process and free of stdout parsing. *)
