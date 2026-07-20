(** The recovering parse.

    The compiler's parser gives up at the first syntax error: its one [error]
    production is only reachable from the start state, so a broken item
    unwinds the whole parse. This module drives the same parser through
    Menhir's incremental API, keeps the checkpoints it reaches at item and
    statement boundaries, and on an error resumes from the last one of them
    instead of from the beginning of the file.

    The error the compiler raised is not lost: the caller keeps it as the
    first diagnostic, and the driver only decides which items survive.

    Three restarts are tried, in order:

    - the last statement checkpoint, when the failure is inside a body, so a
      broken statement is dropped and its function still reaches the tree;
    - the last item checkpoint, so a broken item is dropped and the items
      after it are parsed from a state that already holds the ones before;
    - the item-level byte restart measured by the recovery spike: the text
      up to the last closed item is kept, the broken item's bytes are
      blanked, and the rest is parsed again from the start. Blanking keeps
      the buffer length, so no offset moves.

    A dropped item leaves a [Dropped] stub carrying its name, so an outline
    can still list it. Every restart is bounded and every call into the
    parser is wrapped: no exception leaves this module. *)

open Jasmin

module I = Parser.MenhirInterpreter

(** A broken item, kept so the outline can show it and so typing knows not
    to report its name as unknown later. *)
type dropped =
  | Dropped of {
      kind : string;  (** The item's keyword, or ["item"]. *)
      name : string option;  (** Its name, when the header was readable. *)
      span : Jide_lang.Span.t;
    }

type counters = {
  mutable recoveries : int;  (** Restarts of any kind. *)
  mutable item_restarts : int;
  mutable statement_restarts : int;
  mutable byte_restarts : int;
}

let no_counters () =
  { recoveries = 0; item_restarts = 0; statement_restarts = 0; byte_restarts = 0 }

type t = {
  items : Syntax.pprogram;
  dropped : dropped list;
  recovered : Jide_lang.Span.t list;
      (** The bytes the driver rewrote or skipped. A typing diagnostic
          inside one of them is a consequence of the broken syntax, not of
          the program, so the typing loop drops it. *)
  diagnostics : Jide_lang.Diagnostic.t list;  (** Beyond the compiler's first. *)
  counters : counters;
}

(** How many restarts one file may have. *)
let max_recoveries = 50

(** How many times the driver may rewrite the buffer before it keeps what
    the prefix alone parses. *)
let max_byte_restarts = 8

let empty counters =
  { items = []; dropped = []; recovered = []; diagnostics = []; counters }

type state = {
  scan : Scan.t;
  initial : Syntax.pprogram I.checkpoint;
  boundaries : (int * Syntax.pprogram I.checkpoint) list ref;
      (** Item boundaries, newest first. *)
  statements : (int * Syntax.pprogram I.checkpoint) list ref;
      (** Statement boundaries, newest first. *)
  tried : (int * int) list ref;
      (** Restart points already used, so a restart that makes no progress
          is not tried twice. *)
  mutable closed_item : int;
      (** The token index the last completed item ends at, which is where
          the item that is now broken begins. *)
  mutable offered : bool;
      (** Whether a token has been read since the last checkpoint, which is
          what tells whether the parser reduced an item with a lookahead. *)
  counters : counters;
}

(** [accepts checkpoint entry] is true when the token at [entry] can be
    shifted from [checkpoint]. Menhir answers by running the reductions the
    token would cause, which is why this is only asked at item boundaries:
    it is what tells a top-level [fn] from a [fn] inside a body. *)
let accepts checkpoint (entry : Scan.token) =
  try I.acceptable checkpoint entry.token entry.start_pos with _ -> false

let record state ~index checkpoint =
  if index < Scan.count state.scan then begin
    let entry = Scan.token state.scan index in
    if Scan.item_start entry.token && accepts checkpoint entry then
      state.boundaries := (index, checkpoint) :: !(state.boundaries);
    if Scan.statement_boundary state.scan index then
      state.statements := (index, checkpoint) :: !(state.statements)
  end

(* The driver's inner loop: offer tokens, run reductions, stop when the
   parser accepts or refuses. *)
type step =
  | Accepted of Syntax.pprogram
  | Failed of int  (** The token index that could not be shifted. *)
  | Raised of int * exn

let rec drive state ~index ~eof_sent checkpoint =
  match checkpoint with
  | I.InputNeeded environment ->
      let checkpoint = I.input_needed environment in
      state.offered <- false;
      record state ~index checkpoint;
      (if index < Scan.count state.scan then
         match I.offer checkpoint (Scan.triple state.scan index) with
         | next ->
             state.offered <- true;
             drive state ~index:(index + 1) ~eof_sent next
         | exception exn -> Raised (index, exn)
       else if eof_sent then Failed (min (index - 1) (Scan.count state.scan))
       else
         match I.offer checkpoint (Scan.eof_triple state.scan) with
         | next ->
             state.offered <- true;
             drive state ~index:(index + 1) ~eof_sent:true next
         | exception exn -> Raised (index, exn))
  | I.AboutToReduce (_, production) ->
      (* A completed item: the next item begins at the token the parser is
         looking at, when it is looking at one, and at the next token to
         read when the reduction was a default one. Only an item at the top
         level counts: an item inside a namespace would leave the prefix
         with an unclosed brace. *)
      (match I.lhs production with
      | I.X (I.N nonterminal) ->
          if I.compare_nonterminals nonterminal I.N_top = 0 then begin
            let at = if state.offered then index - 1 else index in
            if at >= Scan.count state.scan
               || (Scan.token state.scan at).depth = 0
            then state.closed_item <- at
          end
      | I.X (I.T _) -> ());
      (match I.resume checkpoint with
      | next -> drive state ~index ~eof_sent next
      | exception exn -> Raised (index, exn))
  | I.Shifting _ -> (
      match I.resume checkpoint with
      | next -> drive state ~index ~eof_sent next
      | exception exn -> Raised (index, exn))
  | I.HandlingError _ -> Failed (max 0 (index - 1))
  | I.Accepted items -> Accepted items
  | I.Rejected -> Failed (max 0 (index - 1))

let start_checkpoint scan =
  Parser.Incremental.module_
    {
      Lexing.pos_fname = Scan.name scan;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }

let new_state counters scan =
  let initial = start_checkpoint scan in
  {
    scan;
    initial;
    boundaries = ref [ (0, initial) ];
    statements = ref [];
    tried = ref [];
    closed_item = 0;
    offered = false;
    counters;
  }

let boundary_at state index =
  List.find_opt (fun (at, _) -> at = index) !(state.boundaries)

let statement_before state index =
  List.find_opt (fun (at, _) -> at < index && at > state.closed_item) !(state.statements)

let was_tried state (source, target) =
  List.exists (fun pair -> pair = (source, target)) !(state.tried)

let prune state source =
  state.boundaries :=
    List.filter (fun (at, _) -> at <= source) !(state.boundaries);
  state.statements :=
    List.filter (fun (at, _) -> at <= source) !(state.statements)

(** The name a broken item had, read from the tokens of the broken region so
    the stub can carry it. *)
let dropped_of scan ~from ~upto =
  let upto = min upto (Scan.count scan) in
  let kind_of = function
    | Parser.FN -> Some "function"
    | Parser.PARAM -> Some "parameter"
    | Parser.TYPE -> Some "type"
    | Parser.NAMESPACE -> Some "namespace"
    | Parser.GLOBAL -> Some "global"
    | Parser.EXEC -> Some "exec"
    | Parser.REQUIRE | Parser.FROM -> Some "require"
    | _ -> None
  in
  let rec keyword index =
    if index >= upto then None
    else
      match kind_of (Scan.token scan index).token with
      | Some kind -> Some (kind, index)
      | None -> keyword (index + 1)
  in
  let rec name_after index =
    if index >= upto then None
    else
      match (Scan.token scan index).token with
      | Parser.NID name -> Some name
      | _ -> name_after (index + 1)
  in
  let kind, name =
    match keyword from with
    | Some (kind, at) -> (kind, name_after (at + 1))
    | None -> (
        (* No keyword left in the region: the broken item lost the one it
           began with, which is what deleting [fn] does. Its first token is
           then the name, and a name followed by a parenthesis names a
           function. *)
        match if from < upto then Some (Scan.token scan from).token else None with
        | Some (Parser.NID name) ->
            let called =
              from + 1 < upto
              && (Scan.token scan (from + 1)).token = Parser.LPAREN
            in
            ((if called then "function" else "item"), Some name)
        | _ -> ("item", None))
  in
  Dropped
    {
      kind;
      name;
      span =
        Scan.span scan ~start_byte:(Scan.start_byte scan from)
          ~end_byte:(Scan.start_byte scan upto);
    }

type attempt =
  | Restart of {
      source : int;  (** The checkpoint's token index. *)
      target : int;  (** Where to resume reading. *)
      checkpoint : Syntax.pprogram I.checkpoint;
      statement : bool;  (** True for a statement checkpoint. *)
    }

let next_attempt state ~failure =
  let statement () =
    match statement_before state failure with
    | None -> None
    | Some (source, checkpoint) -> (
        let depth = (Scan.token state.scan source).depth in
        match Scan.next_statement state.scan ~from:failure ~depth with
        | Some target
          when target > source && not (was_tried state (source, target)) ->
            Some (Restart { source; target; checkpoint; statement = true })
        | _ -> None)
  in
  let item () =
    match boundary_at state state.closed_item with
    | Some (source, checkpoint) when source = state.closed_item -> (
        match Scan.next_item state.scan ~from:failure with
        | Some target
          when target > source && not (was_tried state (source, target)) ->
            Some (Restart { source; target; checkpoint; statement = false })
        | _ -> None)
    | _ -> None
  in
  match statement () with Some attempt -> Some attempt | None -> item ()

