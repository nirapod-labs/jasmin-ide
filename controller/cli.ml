(** The command line of the server binary: what the first argument names, and
    the text the two informational forms print.

    The reader is the first argument alone, because the only mode with
    arguments of its own is [check] and its reader is
    {!Jide_check.Request.parse}. [CLI] does not depend on that library: the
    arguments after `check` are passed through unread, so the two readers
    cannot disagree about which one owns `check --help`. *)

type t =
  | Serve
  | Version
  | Help
  | Check of string list
  | Unknown of string
  | Unexpected of string

let parse = function
  | [] -> Serve
  | "check" :: arguments -> Check arguments
  | ("--version" | "-v") :: _ -> Version
  | ("--help" | "-h") :: _ -> Help
  | option :: _ when String.length option > 0 && option.[0] = '-' ->
      Unknown option
  | argument :: _ -> Unexpected argument

let usage_line =
  "usage: jasmin-lsp [--version | --help | check [options] <file>...]"

(** An argument that is not an option is refused with its own wording: it is
    not an unknown option, and it is not a file, because this program reads
    files only in the check mode, where the mode has to be named first. *)
let refusal = function
  | Unknown option -> [ "jasmin-lsp: unknown option: " ^ option; usage_line ]
  | Unexpected argument ->
      [
        "jasmin-lsp: unexpected argument: " ^ argument;
        "to check a file, name the mode: jasmin-lsp check <file>...";
        usage_line;
      ]
  | Serve | Version | Help | Check _ -> []

let server = Version.server_name
let server_version = Version.version
let jasmin_version = Jide_jasmin.Adapter.jasmin_version

(** The route phrase, from what the build recorded. Two fixed phrases: a
    route that cannot be told apart at run time is recorded at build time
    ({!Build_route.vendored_release}), not guessed here. *)
let route () =
  match Build_route.vendored_release with
  | Some release -> "built from vendored release " ^ release
  | None -> "built from installed library"

(** One line, always the same three parts in the same order. The Jasmin
    version is printed as the library reports it — `Jasmin Compiler
    2026.09.0` — and not wrapped in a second "Jasmin", so the line reads the
    same as the compiler's own `-version` output. *)
let version_line () =
  Printf.sprintf "%s %s (%s, %s)" server server_version jasmin_version
    (route ())

(** The usage block. The modes named are the two the dispatcher has — the
    default serve mode and [check] — and there is no third. The exit codes
    are the ones those two already return, plus 2, which [check] already uses
    for a command line of its own that is wrong, so one line covers both. *)
let help () =
  String.concat "\n"
    [
      "usage: " ^ server ^ " [check [options] <file>...]";
      "";
      "A language server for Jasmin. With no argument it serves the language";
      "server protocol on standard input and output, which is how an editor";
      "starts it.";
      "";
      "modes:";
      "  (none)            serve the language server protocol on stdin/stdout";
      "  check <file>...   parse and type each file and print a verdict for it";
      "";
      "options:";
      "  -v, --version     print the server version, the Jasmin version and";
      "                    the Jasmin source this build uses, then exit";
      "  -h, --help        print this text, then exit";
      "";
      "Run `" ^ server ^ " check --help` for the options of the check mode.";
      "";
      "exit status: 0 for a session that ended normally or a check with no";
      "error, 1 for a check with an error or a session without shutdown, and";
      "2 for a command line this program refuses.";
    ]
