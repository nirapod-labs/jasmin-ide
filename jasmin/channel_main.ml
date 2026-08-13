(* The edge channel's spelling of the linked Jasmin library's architectures:
   [Architecture.architecture] and [CoreArchFactory.core_arch_arm], as
   upstream main has them. This file and `channel_release.ml` are the only two
   files in [jasmin/] that name either; the build copies one of them to
   [channel.ml] and compiles only the copy. *)

(* Main's architecture type carries the core: [Architecture.ARM_M of
   Arm_mcore.t]. Our [Arm_m4] is upstream's `cortex-m4`, the core whose former
   command-line name is `arm-m4`, so it is the same target the release channel
   builds. [get_arch_module] is main's own table, and it ignores the call
   convention for every architecture but x86-64. *)
let module_of (arch : Jasmin.Architecture.architecture)
    (call_conv : Jasmin.Glob_options.call_conv) : (module Jasmin.Arch_full.Arch)
    =
  Jasmin.CoreArchFactory.get_arch_module arch call_conv

let x86 call_conv = module_of Jasmin.Architecture.X86_64 call_conv

let arm_m4 =
  module_of (Jasmin.Architecture.ARM_M Jasmin.Arm_mcore.cortex_m4)
    Jasmin.Glob_options.Linux

let arm_v8a = module_of Jasmin.Architecture.ARMv8A Jasmin.Glob_options.Linux
let riscv = module_of Jasmin.Architecture.RISCV Jasmin.Glob_options.Linux
