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

(* The library's own module for one of our architectures and a call
   convention. [Channel] is where the two channels spell their architectures
   differently, and this is its only caller; every other module in this
   directory reads the architecture table through this function. *)
let module_of : t -> call_conv -> (module Arch_full.Arch) =
 fun arch call_conv ->
  match arch with
  | X86_64 -> Channel.x86 (glob_call_conv call_conv)
  | Arm_m4 -> Channel.arm_m4
  | Arm_v8a -> Channel.arm_v8a
  | Riscv -> Channel.riscv

let check :
    ?resolve:(string -> string option) ->
    context:string list ->
    t ->
    call_conv ->
    file:string ->
    text:string ->
    idirs:(string * string) list ->
    Check.result =
 fun ?(resolve = Check.read_file) ~context arch call_conv ~file ~text ~idirs ->
  let module A = (val module_of arch call_conv) in
  let module K = Check.Make (A) in
  K.check ~resolve ~context ~file ~text ~idirs
