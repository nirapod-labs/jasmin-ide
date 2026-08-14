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

