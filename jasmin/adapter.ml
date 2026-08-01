type arch = Arch.t
type call_conv = Arch.call_conv

let arch_names = Arch.names
let arch_of_string = Arch.of_string
let call_conv_of_string = Arch.call_conv_of_string
let jasmin_version = Version.jasmin
let max_bytes = Scan.max_bytes

type parse_result = Parse.t

let parse = Parse.parse

type check_result = Check.result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
  facts : Facts.t;
}

let check = Arch.check
