let name = ref (Filename.remove_extension (Filename.basename Sys.argv.(0)))

let say format =
  Printf.ksprintf (fun line -> Printf.printf "%s: %s\n%!" !name line) format

let warn format =
  Printf.ksprintf (fun line -> Printf.eprintf "%s: %s\n%!" !name line) format

let die format =
  Printf.ksprintf
    (fun line ->
      Printf.eprintf "%s: %s\n%!" !name line;
      exit 2)
    format

let share part whole =
  if whole <= 0 then "n/a"
  else
    Printf.sprintf "%d of %d (%.1f%%)" part whole
      (100. *. float part /. float whole)

type args = {
  positional : string list;
  value : string -> string option;
  flag : string -> bool;
  values : string -> string list;
}

let parse_args ~usage ~flags ~options arguments =
  let fail message =
    Printf.eprintf "%s: %s\n%s\n" !name message usage;
    exit 2
  in
  let rec loop positional pairs = function
    | [] -> (List.rev positional, List.rev pairs)
    | ("-h" | "--help") :: _ ->
        print_endline usage;
        exit 0
    | key :: rest when List.mem key flags ->
        loop positional ((key, "") :: pairs) rest
    | key :: value :: rest when List.mem key options ->
        loop positional ((key, value) :: pairs) rest
    | key :: [] when List.mem key options -> fail (key ^ " wants a value")
    | argument :: _ when String.length argument > 1 && argument.[0] = '-' ->
        fail ("unknown option: " ^ argument)
    | argument :: rest -> loop (argument :: positional) pairs rest
  in
  let positional, pairs = loop [] [] arguments in
  let values key =
    List.filter_map (fun (k, v) -> if k = key then Some v else None) pairs
  in
  {
    positional;
    value =
      (fun key ->
        match List.rev (values key) with v :: _ -> Some v | [] -> None);
    flag = (fun key -> List.mem_assoc key pairs);
    values;
  }

let int_arg args key ~default =
  match args.value key with
  | None -> default
  | Some text -> (
      match int_of_string_opt text with
      | Some n when n >= 0 -> n
      | _ -> die "%s wants a number, not %S" key text)

let jobs args =
  match int_arg args "--jobs" ~default:4 with
  | 0 -> die "--jobs is below one"
  | n -> n

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let is_directory path = Sys.file_exists path && Sys.is_directory path

let directory path =
  if not (is_directory path) then die "no directory: %s" path;
  let here = Sys.getcwd () in
  Sys.chdir path;
  let resolved = Sys.getcwd () in
  Sys.chdir here;
  resolved

let tests_root path =
  let root = directory path in
  let candidates =
    [
      Filename.concat root "compiler/tests"; Filename.concat root "tests"; root;
    ]
  in
  match
    List.find_opt
      (fun dir -> is_directory (Filename.concat dir "success"))
      candidates
  with
  | Some dir -> dir
  | None ->
      die
        "no success folder under %s: name a release root, its compiler or its \
         tests directory"
        path

let files ?(suffixes = [ ".jazz" ]) root =
  let rec walk relative acc =
    let dir = if relative = "" then root else Filename.concat root relative in
    match Sys.readdir dir with
    | exception Sys_error _ -> acc
    | entries ->
        Array.fold_left
          (fun acc entry ->
            if entry = "" || entry.[0] = '.' then acc
            else
              let rel =
                if relative = "" then entry else relative ^ "/" ^ entry
              in
              let path = Filename.concat root rel in
              if is_directory path then walk rel acc
              else if List.exists (Filename.check_suffix entry) suffixes then
                rel :: acc
              else acc)
          acc entries
  in
  List.sort String.compare (walk "" [])

let deepest path =
  List.fold_left
    (fun found component ->
      match component with
      | "x86-64" | "arm-m4" | "armv8a" | "common" -> Some component
      | "riscv" | "risc-v" -> Some "riscv"
      | _ -> found)
    None
    (String.split_on_char '/' path)

let arch_of path =
  match
    deepest
      (String.concat "/"
         (List.filter (( <> ) "common") (String.split_on_char '/' path)))
  with
  | Some arch -> arch
  | None -> "x86-64"

let arches_of path =
  match deepest path with
  | None -> [ "x86-64" ]
  | Some "common" -> [ "x86-64"; "arm-m4"; "riscv" ]
  | Some arch -> [ arch ]

let group_of path =
  match String.split_on_char '/' path with
  | (("sct-checker" | "safety") as first) :: second :: _ :: _ ->
      first ^ "/" ^ second
  | first :: _ -> first
  | [] -> path

type check = { label : string; path : string; arch : string }

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path text =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel text)

let read_checks file =
  let text = try read_file file with Sys_error message -> die "%s" message in
  let checks =
    List.filter_map
      (fun line ->
        match String.split_on_char '\t' line with
        | label :: path :: rest when path <> "" ->
            let arch =
              match rest with a :: _ when a <> "" -> a | _ -> "x86-64"
            in
            Some { label; path; arch }
        | _ -> None)
      (String.split_on_char '\n' text)
  in
  if checks = [] then die "no checks in %s" file;
  checks

type output = { status : int; out : string; err : string }

let remove_tree path =
  let rec go path =
    match Unix.lstat path with
    | exception Unix.Unix_error _ -> ()
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Array.iter
          (fun entry -> go (Filename.concat path entry))
          (Sys.readdir path);
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  try go path with Unix.Unix_error _ | Sys_error _ -> ()

let temp_dir prefix =
  let base = Filename.get_temp_dir_name () in
  let rec attempt n =
    let dir =
      Filename.concat base (Printf.sprintf "%s.%d.%d" prefix (Unix.getpid ()) n)
    in
    match Unix.mkdir dir 0o700 with
    | () -> dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (n + 1)
  in
  let dir = attempt 0 in
  let owner = Unix.getpid () in
  (* A process [parallel] forks shares the exit handlers; only the one that
     made the directory removes it. *)
  at_exit (fun () -> if Unix.getpid () = owner then remove_tree dir);
  dir

(* Both streams go to files rather than pipes, so a program that writes much
   to one while the other is read cannot block. *)
let run ?cwd ?env ?(merge = false) program arguments =
  let scratch = Filename.get_temp_dir_name () in
  let out_file = Filename.temp_file ~temp_dir:scratch "harness" ".out" in
  let err_file = Filename.temp_file ~temp_dir:scratch "harness" ".err" in
  let argv = Array.of_list (program :: arguments) in
  let status =
    match Unix.fork () with
    | 0 -> (
        try
          Option.iter Sys.chdir cwd;
          let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
          let out = Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
          let err = Unix.openfile err_file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
          Unix.dup2 null Unix.stdin;
          Unix.dup2 out Unix.stdout;
          Unix.dup2 (if merge then out else err) Unix.stderr;
          match env with
          | Some env -> Unix.execvpe program argv env
          | None -> Unix.execvp program argv
        with _ -> Unix._exit 127)
    | pid -> (
        match snd (Unix.waitpid [] pid) with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + abs signal)
  in
  let out = read_file out_file and err = read_file err_file in
  Sys.remove out_file;
  Sys.remove err_file;
  { status; out; err }

let executable ~what program =
  let found =
    if String.contains program '/' then
      if Sys.file_exists program && not (is_directory program) then
        Some
          (Filename.concat
             (directory (Filename.dirname program))
             (Filename.basename program))
      else None
    else
      List.find_map
        (fun dir ->
          let path = Filename.concat dir program in
          if Sys.file_exists path then Some path else None)
        (String.split_on_char ':'
           (Option.value ~default:"" (Sys.getenv_opt "PATH")))
  in
  match found with
  | Some path
    when try
           Unix.access path [ Unix.X_OK ];
           true
         with Unix.Unix_error _ -> false ->
      path
  | _ -> die "%s is not an executable program: %s" what program

let compiler = function
  | None | Some "" ->
      die "no compiler: name the reference jasminc with --jasminc"
  | Some program ->
      let path = executable ~what:"the compiler" program in
      let { status; out; _ } = run path [ "-version" ] in
      let version = String.trim out in
      if status <> 0 then die "%s -version failed (exit %d)" path status;
      if version = "" then die "%s printed no version" path;
      (path, String.concat " " (String.split_on_char '\n' version))

let built relative =
  let rec up dir =
    if
      Filename.basename dir = "default"
      && Filename.basename (Filename.dirname dir) = "_build"
    then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else up parent
  in
  match up (Filename.dirname (absolute Sys.executable_name)) with
  | Some dir when Sys.file_exists (Filename.concat dir relative) ->
      Filename.concat dir relative
  | _ -> die "no %s in this build: build the tree first" relative

let parallel ~jobs f items =
  let items = Array.of_list items in
  let count = Array.length items in
  let results = Array.make count (Error "not run") in
  let scratch = temp_dir "parallel" in
  let file index = Filename.concat scratch (string_of_int index) in
  let running = Hashtbl.create jobs in
  let finish () =
    let pid, status = Unix.wait () in
    match Hashtbl.find_opt running pid with
    | None -> ()
    | Some index ->
        Hashtbl.remove running pid;
        results.(index) <-
          (match status with
          | Unix.WEXITED 0 -> (
              match
                In_channel.with_open_bin (file index) Marshal.from_channel
              with
              | result -> result
              | exception _ -> Error "the process left no result")
          | Unix.WEXITED code ->
              Error (Printf.sprintf "the process exited %d" code)
          | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
              Error
                (Printf.sprintf "the process was killed by signal %d" signal))
  in
  Array.iteri
    (fun index item ->
      while Hashtbl.length running >= jobs do
        finish ()
      done;
      flush_all ();
      match Unix.fork () with
      | 0 ->
          let result =
            match f item with
            | value -> Ok value
            | exception exn -> Error ("raised " ^ Printexc.to_string exn)
          in
          (try
             Out_channel.with_open_bin (file index) (fun channel ->
                 Marshal.to_channel channel (result : (_, string) result) [])
           with _ -> Unix._exit 3);
          flush_all ();
          Unix._exit 0
      | pid -> Hashtbl.replace running pid index)
    items;
  while Hashtbl.length running > 0 do
    finish ()
  done;
  Array.to_list results
