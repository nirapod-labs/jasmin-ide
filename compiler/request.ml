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

let parse arguments =
  let rec loop state rest =
    match rest with
    | [] -> finish state
    | ("-h" | "--help") :: _ -> Help
    | "--" :: rest -> finish { state with files = state.files @ rest }
    | (("--arch" | "--call-conv" | "-I") as option) :: rest -> (
        match option_value ~prefix:option option rest with
        | None -> Invalid (Printf.sprintf "missing value for %s" option)
        | Some (value, rest) -> (
            match option with
            | "--arch" -> set_arch state value rest
            | "--call-conv" -> set_call_conv state value rest
            | _ -> add_include state value rest))
    | argument :: rest when starts_with ~prefix:"-I" argument ->
        (* [-Iname=path], the form a caller writes without a space. The
           argument is longer than [-I], so the value is what follows. *)
        let value = String.sub argument 2 (String.length argument - 2) in
        let value =
          if String.length value > 0 && value.[0] = '=' then
            String.sub value 1 (String.length value - 1)
          else value
        in
        add_include state value rest
    | argument :: rest when starts_with ~prefix:"--arch=" argument ->
        set_arch state (String.sub argument 7 (String.length argument - 7)) rest
    | argument :: rest when starts_with ~prefix:"--call-conv=" argument ->
        set_call_conv state
          (String.sub argument 12 (String.length argument - 12))
          rest
    | argument :: _ when String.length argument > 0 && argument.[0] = '-' ->
        Invalid (Printf.sprintf "unknown option: %s" argument)
    | file :: rest -> loop { state with files = state.files @ [ file ] } rest
  and set_arch state name rest =
    match Jide_jasmin.Adapter.arch_of_string name with
    | Some arch -> loop { state with arch = Some arch } rest
    | None ->
        Invalid
          (Printf.sprintf "unknown architecture: %s (known: %s)" name
             (String.concat ", " Jide_jasmin.Adapter.arch_names))
  and set_call_conv state name rest =
    match Jide_jasmin.Adapter.call_conv_of_string name with
    | Some call_conv -> loop { state with call_conv = Some call_conv } rest
    | None ->
        Invalid
          (Printf.sprintf
             "unknown calling convention: %s (known: linux, windows)" name)
  and add_include state value rest =
    match split_include value with
    | Some pair -> loop { state with idirs = state.idirs @ [ pair ] } rest
    | None ->
        Invalid
          (Printf.sprintf "the -I argument needs name=path, got: %s" value)
  and finish state =
    let arch =
      match state.arch with
      | Some arch -> Some arch
      | None -> Jide_jasmin.Adapter.arch_of_string default_arch
    in
    let call_conv =
      match state.call_conv with
      | Some call_conv -> Some call_conv
      | None -> Jide_jasmin.Adapter.call_conv_of_string default_call_conv
    in
    match (arch, call_conv, state.files) with
    | _, _, [] -> Invalid "no input file"
    | None, _, _ ->
        Invalid
          (Printf.sprintf "this build has no %s architecture" default_arch)
    | _, None, _ ->
        Invalid
          (Printf.sprintf "this build has no %s calling convention"
             default_call_conv)
    | Some arch, Some call_conv, files ->
        Run { arch; call_conv; idirs = state.idirs; files }
  in
  loop empty arguments
