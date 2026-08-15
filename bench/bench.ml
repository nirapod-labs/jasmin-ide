(* Timing for the two budgets a whole check decides: the whole check of a
   unit, and the interval from a keystroke to the diagnostics it produces.

   Both run the server's own code in this process, so a number is the work
   the server does and not a model of it. A build must not run beside a
   measurement: `make bench` refuses to start while one is running, and
   bench/run.sh says how it decides. *)

let now = Unix.gettimeofday
let default_runs = 20
let default_count = 5
let default_arch_name = "x86-64"
let default_call_conv_name = "linux"

let die message =
  prerr_endline ("bench: " ^ message);
  exit 2

let parse_arch name =
  match Jide_jasmin.Adapter.arch_of_string name with
  | Some arch -> arch
  | None ->
      die
        (Printf.sprintf "unknown architecture: %s (known: %s)" name
           (String.concat ", " Jide_jasmin.Adapter.arch_names))

let parse_call_conv name =
  match Jide_jasmin.Adapter.call_conv_of_string name with
  | Some call_conv -> call_conv
  | None ->
      die
        (Printf.sprintf "unknown calling convention: %s (known: linux, windows)"
           name)

let usage =
  String.concat "\n"
    [
      "usage: bench <command> [options]";
      "";
      "commands:";
      "  whole      time a whole check (parse and type) of each unit";
      "  keystroke  time a didChange from the protocol loop to the diagnostics";
      "  largest    print the largest Jasmin files under a directory";
      "";
      "options:";
      "  --units FILE      a table of units, one per line:";
      "                    label<TAB>arch<TAB>idirs<TAB>path";
      "                    arch is a name or `auto`; idirs is `-` or a";
      "                    comma-separated list of name=path";
      "  --arch NAME       architecture for the files on the command line";
      "                    (default: the first path component that names one)";
      "  --call-conv NAME  calling convention (default: linux)";
      "  -I NAME=PATH      an include path, repeatable";
      "  --label NAME      the row name for the files on the command line";
      "  --runs N          checks per unit (default: 20)";
      "  --samples         print every run as well as the summary";
      "  --count N         how many files `largest` prints (default: 5)";
      "  -h, --help        print this text";
      "";
      "A file named on the command line without --arch takes the architecture";
      "its path names, so a corpus that keeps one directory per architecture";
      "needs no options.";
      "";
      "The exit status is 0 when every unit ran, and 2 when the command line";
      "or an input is wrong.";
    ]

(* A unit is one file, the architecture it is checked for and the include
   paths its requires resolve through. [arch_name] is kept beside the parsed
   architecture because the adapter exposes the names and the parser, not
   the printer, and the table has to print what was used. *)
type unit_ = {
  label : string;
  arch_name : string;
  arch : Jide_jasmin.Adapter.arch;
  call_conv : Jide_jasmin.Adapter.call_conv;
  idirs : (string * string) list;
  file : string;
}

