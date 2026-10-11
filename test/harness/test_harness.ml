(* The shared harness code: the architecture rules, the corpus walk, the
   compiler check that keeps a misnamed compiler from becoming a skip, and
   parallel work that never turns a fault into a result. *)

module H = Jide_harness.Harness

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* The exit status of [f ()] run in a process of its own, for the calls that
   exit. *)
let status_of f =
  match Unix.fork () with
  | 0 ->
      Unix.dup2 (Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0) Unix.stderr;
      (try f () with _ -> Unix._exit 99);
      Unix._exit 0
  | pid -> (
      match snd (Unix.waitpid [] pid) with Unix.WEXITED code -> code | _ -> -1)

let script dir file text =
  let path = Filename.concat dir file in
  H.write_file path text;
  Unix.chmod path 0o755;
  path

let () =
  check "arch: deepest component"
    (H.arch_of "success/x86-64/arm-m4/a.jazz" = "arm-m4");
  check "arch: risc-v is riscv" (H.arch_of "fail/risc-v/a.jazz" = "riscv");
  check "arch: none is x86-64" (H.arch_of "exec/a.jazz" = "x86-64");
  check "arch: common is not an architecture"
    (H.arch_of "success/arm-m4/common/a.jazz" = "arm-m4");
  check "arches: common is three"
    (H.arches_of "success/common/a.jazz" = [ "x86-64"; "arm-m4"; "riscv" ]);
  check "arches: a deeper one wins"
    (H.arches_of "success/common/x86-64/a.jazz" = [ "x86-64" ]);
  check "group: sct-checker keeps two parts"
    (H.group_of "sct-checker/success/a.jazz" = "sct-checker/success");
  check "group: a file at a second level is one part"
    (H.group_of "safety/a.jazz" = "safety");
  check "group: otherwise one" (H.group_of "success/common/a.jazz" = "success");
  let dir = H.temp_dir "test-harness" in
  Unix.mkdir (Filename.concat dir "b") 0o755;
  Unix.mkdir (Filename.concat dir ".hidden") 0o755;
  List.iter
    (fun file -> H.write_file (Filename.concat dir file) "")
    [ "b/z.jazz"; "a.jazz"; "b/y.jinc"; ".hidden/x.jazz"; "note.txt" ];
  check "files: sorted, relative, hidden skipped"
    (H.files dir = [ "a.jazz"; "b/z.jazz" ]);
  check "files: suffixes"
    (H.files ~suffixes:[ ".jazz"; ".jinc" ] dir
    = [ "a.jazz"; "b/y.jinc"; "b/z.jazz" ]);
  (* A compiler that is missing, does not run, fails or prints nothing exits
     2; only a working one is accepted. *)
  let good = script dir "good" "#!/bin/sh\necho 'Jasmin Compiler 1.0'\n" in
  let failing = script dir "failing" "#!/bin/sh\necho broken\nexit 3\n" in
  let silent = script dir "silent" "#!/bin/sh\nexit 0\n" in
  let plain = Filename.concat dir "a.jazz" in
  check "compiler: a working one"
    (match H.compiler (Some good) with
    | path, "Jasmin Compiler 1.0" -> Filename.basename path = "good"
    | _ -> false);
  List.iter
    (fun (what, value) ->
      check
        ("compiler: " ^ what ^ " exits 2")
        (status_of (fun () -> ignore (H.compiler value)) = 2))
    [
      ("none", None);
      ("missing", Some "/nonexistent/jasminc");
      ("not executable", Some plain);
      ("failing", Some failing);
      ("silent", Some silent);
    ];
  let output = H.run ~cwd:dir "/bin/sh" [ "-c"; "pwd; echo err >&2; exit 4" ] in
  check "run: status, both streams and the directory"
    (output.H.status = 4 && output.H.err = "err\n"
    && Filename.basename (String.trim output.H.out) = Filename.basename dir);
  check "run: a program that cannot start is 127"
    ((H.run "/nonexistent" []).H.status = 127);
  (* Results come back in order; a raise and an exit are errors, never a
     value. *)
  let results =
    H.parallel ~jobs:2
      (fun n ->
        if n = 3 then failwith "three" else if n = 4 then exit 5 else n * 10)
      [ 1; 2; 3; 4; 5 ]
  in
  check "parallel: order and faults"
    (match results with
    | [ Ok 10; Ok 20; Error raised; Error exited; Ok 50 ] ->
        raised = "raised Failure(\"three\")" && exited = "the process exited 5"
    | _ -> false);
  if !failures = 0 then print_endline "all harness tests passed"
  else (
    Printf.printf "%d harness tests failed\n" !failures;
    exit 1)
