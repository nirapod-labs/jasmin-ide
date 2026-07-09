(** The command line of the batch checker. *)

type t = {
  arch : Jide_jasmin.Adapter.arch;
  call_conv : Jide_jasmin.Adapter.call_conv;
  idirs : (string * string) list;
  files : string list;
}

type parsed = Run of t | Help | Invalid of string

let usage =
  String.concat "\n"
    [
      "usage: jasmin-lsp check [--arch NAME] [--call-conv NAME] [-I \
       name=path]... <file>...";
      "";
      "Parse and type each file and print a verdict for it.";
      "One line per diagnostic, then one summary line per file.";
      "";
      "options:";
      "  --arch NAME       target architecture (default: x86-64)";
      "  --call-conv NAME  calling convention (default: linux)";
      "  -I NAME=PATH      named include path, repeatable";
      "  -h, --help        print this text";
      "";
      "exit status: 0 when no file has an error, 1 when one does, 2 when the";
      "command line itself is wrong.";
    ]

let default_arch = "x86-64"
let default_call_conv = "linux"

type state = {
  arch : Jide_jasmin.Adapter.arch option;
  call_conv : Jide_jasmin.Adapter.call_conv option;
  idirs : (string * string) list;
  files : string list;
}

let empty = { arch = None; call_conv = None; idirs = []; files = [] }

(** The value of an option: either the next argument, or what follows
    [prefix=] inside this one. *)
let option_value ~prefix argument rest =
  let length = String.length prefix in
  if
    String.length argument > length
    && String.sub argument 0 length = prefix
    && argument.[length] = '='
  then
    Some
      ( String.sub argument (length + 1) (String.length argument - length - 1),
        rest )
  else match rest with [] -> None | value :: rest -> Some (value, rest)

(** [name=path], as the compiler's own include-path option writes it: exactly
    one equals sign, a name that cannot be empty and a path that may be. The
    compiler splits on a single [=] and refuses anything else, so a path that
    itself holds [=] is refused here too and the checker stays an exact
    oracle for the command line it shares with [jasminc]. *)
let split_include value =
  match String.index_opt value '=' with
  | None | Some 0 -> None
  | Some index ->
      let name = String.sub value 0 index in
      let path =
        String.sub value (index + 1) (String.length value - index - 1)
      in
      if String.contains path '=' then None else Some (name, path)

let starts_with ~prefix argument =
  String.length argument >= String.length prefix
  && String.sub argument 0 (String.length prefix) = prefix

