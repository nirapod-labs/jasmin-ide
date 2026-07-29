open Jasmin

type t = X86_64 | Arm_m4 | Arm_v8a | Riscv
type call_conv = Linux | Windows

let all = [ X86_64; Arm_m4; Arm_v8a; Riscv ]

let to_string = function
  | X86_64 -> "x86-64"
  | Arm_m4 -> "arm-m4"
  | Arm_v8a -> "armv8a"
  | Riscv -> "riscv"

let names = List.map to_string all

let of_string = function
  | "x86-64" -> Some X86_64
  | "arm-m4" -> Some Arm_m4
  | "armv8a" -> Some Arm_v8a
  | "riscv" -> Some Riscv
  | _ -> None

let call_conv_to_string = function Linux -> "linux" | Windows -> "windows"

let call_conv_of_string = function
  | "linux" -> Some Linux
  | "windows" -> Some Windows
  | _ -> None

let glob_call_conv = function
  | Linux -> Glob_options.Linux
  | Windows -> Glob_options.Windows

let check :
    ?resolve:(string -> string option) ->
    t ->
    call_conv ->
    file:string ->
    text:string ->
    idirs:(string * string) list ->
    Check.result =
 fun ?(resolve = Check.read_file) arch call_conv ~file ~text ~idirs ->
  match arch with
  | X86_64 ->
      let module C =
        (val CoreArchFactory.core_arch_x86 (glob_call_conv call_conv))
      in
      let module A = Arch_full.Arch_from_Core_arch (C) in
      let module K = Check.Make (A) in
      K.check ~resolve ~file ~text ~idirs
  | Arm_m4 ->
      let module A =
        Arch_full.Arch_from_Core_arch (CoreArchFactory.Core_arch_ARM) in
      let module K = Check.Make (A) in
      K.check ~resolve ~file ~text ~idirs
  | Arm_v8a ->
      let module A =
        Arch_full.Arch_from_Core_arch (CoreArchFactory.Core_arch_ARMV8A) in
      let module K = Check.Make (A) in
      K.check ~resolve ~file ~text ~idirs
  | Riscv ->
      let module A =
        Arch_full.Arch_from_Core_arch (CoreArchFactory.Core_arch_RISCV) in
      let module K = Check.Make (A) in
      K.check ~resolve ~file ~text ~idirs
