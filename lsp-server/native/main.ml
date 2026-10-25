let log message = prerr_endline ("[jasmin-lsp] " ^ message)
let write channels packets = List.iter (Jide_lsp.Framing.write channels) packets

(* The wait the debounce is built on. The loop waits for input, but never
   longer than the time left on the earliest deadline: the end of that wait is
   what analyses a document whose last change was followed by no packet at
   all. A negative timeout waits indefinitely, which is what the server asks
   for when no document is waiting. *)
let run channels server =
  let publish_due () =
    write channels (Jide_controller.Server.analyse_due server)
  in
  let rec loop () =
    let timeout =
      match Jide_controller.Server.time_to_next server with
      | Some seconds -> seconds
      | None -> -1.
    in
    if not (Jide_lsp.Framing.wait channels ~timeout) then (
      publish_due ();
      loop ())
    else
      match Jide_lsp.Framing.read channels with
      | None ->
          if not (Jide_controller.Server.exited server) then log "input closed";
          Jide_controller.Server.eof_exit_code server
      | Some (Error message) ->
          log ("malformed frame dropped: " ^ message);
          (* A frame the reader cannot frame leaves the stream poisoned, and
             a poisoned stream is ready to read: the turn after this one
             reads end of input and ends the session here. *)
          loop ()
      | Some (Ok packet) ->
          (* A document whose debounce delay has passed is analysed before the
             packet is served, and its diagnostics are written first. *)
          publish_due ();
          write channels (Jide_controller.Server.handle_packet server packet);
          if Jide_controller.Server.exited server then
            Jide_controller.Server.exit_code server
          else loop ()
  in
  loop ()

(** The engine's completion kinds in the protocol layer's own vocabulary. The
    two enumerations are separate on purpose: the engine names what a
    candidate *is* in Jasmin, and the protocol names the icon a client shows,
    so the mapping lives here rather than in either of them. *)
let completion_kind_of (kind : Jide_engine.Complete.kind) :
    Jide_engine.Types.completion_kind =
  match kind with
  | Jide_engine.Complete.Keyword -> Jide_engine.Types.Keyword
  | Jide_engine.Complete.Function -> Jide_engine.Types.Function
  | Jide_engine.Complete.Parameter | Jide_engine.Complete.Variable ->
      Jide_engine.Types.Variable_
  | Jide_engine.Complete.Global -> Jide_engine.Types.Field
  | Jide_engine.Complete.Type_alias -> Jide_engine.Types.Type_parameter
  | Jide_engine.Complete.Annotation_key -> Jide_engine.Types.Text
  | Jide_engine.Complete.Intrinsic -> Jide_engine.Types.Function

(** One candidate, in the shape the protocol layer encodes. [detail] carries
    the declaration's own source text, which is what the engine cut from the
    file; nothing here invents text. *)
let completion_item_of (item : Jide_engine.Complete.item) :
    Jide_engine.Types.completion_item =
  {
    Jide_engine.Types.label = item.Jide_engine.Complete.label;
    kind = completion_kind_of item.Jide_engine.Complete.kind;
    detail = item.Jide_engine.Complete.detail;
    documentation = None;
    insert_text = None;
    sort_text = None;
  }

(** The protocol's answer for one completion: the candidates and nothing
    else, because the protocol replaces the word itself. *)
let completion_of (completion : Jide_engine.Complete.completion) :
    Jide_engine.Types.completion_item list =
  List.map completion_item_of completion.Jide_engine.Complete.items

(** One signature, in the shape the protocol layer encodes. *)
let signature_of (signature : Jide_engine.Complete.signature) :
    Jide_engine.Types.signature =
  {
    Jide_engine.Types.label = signature.Jide_engine.Complete.label;
    documentation = None;
    parameters =
      List.map
        (fun label -> { Jide_engine.Types.label; documentation = None })
        signature.Jide_engine.Complete.parameters;
  }

(** The protocol's answer for one call: the one signature the engine has, and
    the argument the offset is in. *)
let signature_help_of (help : Jide_engine.Complete.signature) :
    Jide_engine.Types.signature_help =
  {
    Jide_engine.Types.signatures = [ signature_of help ];
    active_signature = 0;
    active_parameter = help.Jide_engine.Complete.active;
  }

(** The protocol loop, which is the mode when the command line names no
    other. *)
let serve () =
  let engine = Jide_engine.Engine.create () in
  (* Completion and signature help answer from the snapshot the last check
     left: {!Jide_engine.Engine.snapshot} reads what that check built and does
     no work of its own, so neither request costs a parse, a type or a file
     read, and both answer while the file is being edited. The other five
     requests are still answered with the empty answer the protocol has for
     each, and a rename, which has no empty answer, with a refusal. *)
  let completion ~uri ~byte =
    match Jide_engine.Engine.snapshot engine ~uri with
    | Some snapshot ->
        Some
          (completion_of
             (Jide_engine.Complete.completion ~arch:snapshot.arch
                ~index:snapshot.index ~resolve:snapshot.resolve ~byte ()))
    | None -> None
  in
  let signature_help ~uri ~byte =
    match Jide_engine.Engine.snapshot engine ~uri with
    | Some snapshot ->
        Option.map signature_help_of
          (Jide_engine.Complete.signature_help ~arch:snapshot.arch
             ~index:snapshot.index ~resolve:snapshot.resolve
             ~facts:snapshot.facts ~byte ())
    | None -> None
  in
  let features =
    Jide_controller.Features.make ~completion ~signature_help
      ~prepare_rename:(fun ~uri:_ ~byte:_ -> None)
      ~rename:(fun ~uri:_ ~byte:_ ~new_name:_ -> None)
      ~workspace_symbols:(fun ~query:_ -> None)
      ~highlights:(fun ~uri:_ ~byte:_ -> None)
      ~semantic_tokens:(fun ~uri:_ -> None)
      ()
  in
  if not (Jide_controller.Features.available features) then
    log "no engine for the editing requests: each answer is the empty one";
  let server =
    Jide_controller.Server.create ?engine:(Some engine) ~features
      ?analysis:(Some (Analysis.create engine))
      ~log ()
  in
  let channels = Jide_lsp.Framing.create ~input:stdin ~output:stdout in
  try run channels server with
  | Sys_error message ->
      log ("input or output closed: " ^ message);
      Jide_controller.Server.eof_exit_code server
  | exn ->
      log ("fatal: " ^ Printexc.to_string exn);
      1

let print_line line =
  print_string line;
  print_newline ();
  flush stdout

let () =
  (* The command line is read before the server is created, so the modes that
     are not the server write nothing but their own lines to stdout and never
     touch the protocol (`check` says so too: compiler/cli.mli). An argument
     this program does not take is refused here rather than served: falling
     through to `serve` would wait on standard input for an editor that is
     not coming. `--stdio`, the transport word a language client passes, is
     not such an argument: it names the stream this arm already reads, so it
     is skipped and `serve` runs. *)
  let code =
    match
      Jide_controller.Cli.parse
        (match Array.to_list Sys.argv with
        | _ :: arguments -> arguments
        | [] -> [])
    with
    | Jide_controller.Cli.Check arguments -> Jide_check.Cli.main arguments
    | Jide_controller.Cli.Version ->
        print_line (Jide_controller.Cli.version_line ());
        0
    | Jide_controller.Cli.Help ->
        print_line (Jide_controller.Cli.help ());
        0
    | (Jide_controller.Cli.Unknown _ | Jide_controller.Cli.Unexpected _) as
      refused ->
        List.iter prerr_endline (Jide_controller.Cli.refusal refused);
        2
    | Jide_controller.Cli.Serve -> serve ()
  in
  exit code
