(* The linked Jasmin library's architecture modules, spelled the one way each
   channel spells them.

   This is the only file in [jasmin/] whose contents depend on which Jasmin is
   vendored, and [channel.ml] is the only module whose contents differ between
   the two channels. Everything else in the directory calls {!Arch.module_of},
   which calls this module and nothing channel-specific.

   The release (the tarball `vendor/fetch.sh` assembles by default) names its
   architectures with [Utils.architecture] and exposes the ARM M-profile
   back-end as a module, [CoreArchFactory.Core_arch_ARM]. Upstream main, which
   the edge channel builds against, removed both: the architecture type is
   [Architecture.architecture], whose ARM entry carries the core
   ([Architecture.ARM_M of Arm_mcore.t]), and the back-end is the function
   [CoreArchFactory.core_arch_arm]. Each spelling is an unbound module or
   constructor in the other channel, so one piece of source cannot name both;
   the choice is made here, once, and the compiler checks it against this
   interface in whichever channel is built.

   [channel.ml] is generated at build time from an implementation of this
   interface: `jasmin/channel_release.ml` by default, and
   `jasmin/channel_main.ml` when JIDE_JASMIN_CHANNEL is `edge`, which
   `scripts/edge.sh` sets for the edge build. Neither implementation is
   compiled except as the copy, so the release build never reads main's API
   and the edge build never reads the release's. *)

val x86 : Jasmin.Glob_options.call_conv -> (module Jasmin.Arch_full.Arch)
(** [x86 call_conv] is the library's x86-64 back-end for one call convention;
    the two conventions are two different modules, not one. *)

val arm_m4 : (module Jasmin.Arch_full.Arch)
(** The library's ARM M-profile back-end. Our [Arm_m4] is upstream's
    `cortex-m4`, the core whose former command-line name is `arm-m4`, so a
    program written for `-arch arm-m4` is the same target on both channels.
    The ARM back-end takes no call convention: the library fixes it to the
    Linux one in both channels. *)

val arm_v8a : (module Jasmin.Arch_full.Arch)
(** The library's ARMv8-A back-end. *)

val riscv : (module Jasmin.Arch_full.Arch)
(** The library's RISC-V back-end. *)
