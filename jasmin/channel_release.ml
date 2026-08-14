(* The release channel's spelling of the linked Jasmin library's
   architectures: [Utils.architecture] and [CoreArchFactory.Core_arch_ARM], as
   upstream v2026.09.0 has them. This file and `channel_main.ml` are the only
   two files in [jasmin/] that name either; the build copies one of them to
   [channel.ml] and compiles only the copy. *)

(* [CoreArchFactory.get_arch_module] is the library's own architecture table.
   For ARM M-profile it is [Arch_full.Arch_from_Core_arch (Core_arch_ARM)],
   which is what this directory built by hand before this file existed, and it
   ignores the call convention for every architecture but x86-64, so passing
   the Linux one for the others changes nothing. *)
let module_of (arch : Jasmin.Utils.architecture)
    (call_conv : Jasmin.Glob_options.call_conv) : (module Jasmin.Arch_full.Arch)
    =
  Jasmin.CoreArchFactory.get_arch_module arch call_conv

let x86 call_conv = module_of Jasmin.Utils.X86_64 call_conv
let arm_m4 = module_of Jasmin.Utils.ARM_M4 Jasmin.Glob_options.Linux
let arm_v8a = module_of Jasmin.Utils.ARMv8A Jasmin.Glob_options.Linux
let riscv = module_of Jasmin.Utils.RISCV Jasmin.Glob_options.Linux
