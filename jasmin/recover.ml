(** The recovering parse.

    The compiler's parser gives up at the first syntax error: its one [error]
    production is only reachable from the start state, so a broken item
    unwinds the whole parse. This module drives the same parser through
    Menhir's incremental API, keeps the checkpoints it reaches at item and
    statement boundaries, and on an error resumes from the last one of them
    instead of from the beginning of the file.

    The error the compiler raised is not lost: the caller keeps it as the
    first diagnostic, and the driver only decides which items survive. Every
    item the driver could not keep is reported too, one diagnostic at the
    stub's own span, so a hole in the tree is a hole in the diagnostics as
    well.

    Four restarts are tried, in order:

    - a virtual [}] when the file ends, or a keyword that can never be an
      identifier appears, inside a block: the block is closed without a byte
      of the file being rewritten and the text read so far stays in the
      tree, which is the only repair that loses nothing;
    - the last statement checkpoint, when the failure is inside a body, so a
      broken statement is dropped and its function still reaches the tree;
    - the latest item checkpoint at or after the last closed item, so a
      broken item is dropped and the items after it are parsed from a state
      that already holds the ones before, including the items of a namespace
      whose neighbour is broken. A checkpoint is kept at the first token
      after each closed top-level block as well as at each item keyword, so
      stray tokens that begin no item are dropped without a rewrite;
    - the item-level byte restart measured by the recovery spike: the text
      up to the last closed item is kept, the broken item's bytes are
      blanked, and the rest is parsed again from the start. Blanking keeps
      the buffer length, so no offset moves.

    When the recovery bound is reached, the prefix that is already complete
    is kept: the bytes of the item that is still broken, and of everything
    after it, are blanked and the prefix is parsed on its own, so a file with
    more faults than the bound still returns every item before the first
    unrepaired one.

    A dropped item leaves a [Dropped] stub carrying its name, so an outline
    can still list it. The stub is not a [Syntax.pitem]: that type belongs to
    the compiler and adding a constructor to it would stop typing from
    consuming the tree unchanged. {!Parse.items_with_stubs} merges the stubs
    into the item stream in source order, which is what an outline walks.

    Every restart is bounded and every call into the parser is wrapped: no
    exception leaves this module. *)

open Jasmin
module I = Parser.MenhirInterpreter

(** The kind of declaration a stub stands for, as the item's keyword names
    it. A consumer that suppresses a later reference to the stub keys on
    this, not on a string, so the two can never disagree about a spelling. *)
type kind =
  | Function
  | Parameter
  | Global
  | Type
  | Namespace
  | Exec
  | Require
  | Item

(** A broken item, kept so the outline can show it and so typing knows not
    to report its name as unknown later. *)
type dropped =
  | Dropped of {
      kind : kind;  (** The item's keyword, or [Item]. *)
      name : string option;  (** Its name, when the header was readable. *)
      span : Jide_lang.Span.t;
    }

(** The kind as the item's keyword spells it, which is what a diagnostic
    about the stub says. *)
let kind_name = function
  | Function -> "function"
  | Parameter -> "parameter"
  | Global -> "global"
  | Type -> "type"
  | Namespace -> "namespace"
  | Exec -> "exec"
  | Require -> "require"
  | Item -> "item"

(** The diagnostic one stub produces. The compiler reports the first fault it
    meets and stops there, so a file with a second broken item would lose it
    from the tree and from the diagnostics at once: a stub is present in both
    or it is present in neither. The message names the item, so two holes in
    one file are two distinguishable markers. *)
let diagnostic_of_dropped = function
  | Dropped { kind; name; span } ->
      let what =
        match name with
        | Some name -> Printf.sprintf "the %s `%s'" (kind_name kind) name
        | None -> Printf.sprintf "an item"
      in
      Jide_lang.Diagnostic.make ~code:"parse" ~source:"jasmin"
        ~severity:Jide_lang.Diagnostic.Error span
        (what ^ " could not be parsed and is not in the tree")

type counters = {
  mutable recoveries : int;  (** Restarts of any kind. *)
  mutable item_restarts : int;
  mutable statement_restarts : int;
  mutable brace_restarts : int;  (** Closings with a brace that was missing. *)
  mutable byte_restarts : int;
  mutable moved_positions : int;
      (** Restarts that moved the position the item after the hole begins
          at, so that item covers the hole alone. *)
  mutable refused_positions : int;
      (** Restarts that could not move it and kept the position the
          checkpoint carried. Expected to stay at zero: it is the defect the
          move removes, and a run that shows it is a run to look at. *)
}

let no_counters () =
  {
    recoveries = 0;
    item_restarts = 0;
    statement_restarts = 0;
    brace_restarts = 0;
    byte_restarts = 0;
    moved_positions = 0;
    refused_positions = 0;
  }

type t = {
  items : Syntax.pprogram;
  dropped : dropped list;
  recovered : Jide_lang.Span.t list;
      (** The bytes the driver rewrote or skipped, the scan's own blankings
          included. A typing diagnostic inside one of them is a consequence
          of the broken syntax, not of the program, so the typing loop drops
          it. *)
  diagnostics : Jide_lang.Diagnostic.t list;
      (** Beyond the compiler's first. *)
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
  mutable stubbed_upto : int;
      (** The token index the last stub ended at. A restart that covers the
          bytes between [closed_item] and this index would be reporting a
          hole that is already stubbed, so only the bytes after it are a new
          hole. *)
  mutable input : (int * Syntax.pprogram I.env) option;
      (** The last state that asked for a token and the index it asked for.
          A failure at that index is a failure to shift that token, and the
          state is where a virtual token can be offered instead. *)
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

(** [item_begins scan ~checkpoint ~target] is true when the parser, resumed
    at [target] from [checkpoint], shifts the token there and reduces a
    top-level item before it meets a token it cannot shift.

    {!accepts} alone does not answer the question that matters here. A global
    declaration begins with its type, and a type is also a statement and an
    expression, so a name or a [u64] that the parser can shift where an item
    belongs may be a global, may be a local declaration whose [reg] was lost,
    or may be the middle of the expression above it. The grammar's FIRST set
    cannot separate them and no lexical rule can. What can is the parser: run
    the item from the candidate and see whether a [top] production comes out
    of it. An item that is not really there fails inside the run, at the
    token where the guess stops making sense, and the candidate is refused.

    Only a [top] reduced after the candidate's own first token has been
    shifted counts. Menhir defers the reduction of an item whose last symbol
    has just been read until it knows the next token, so a checkpoint taken
    at an item boundary still holds the item before it on the stack; the
    first token offered then finishes *that* item and a [top] comes out of it
    without the candidate having begun anything. A shift of the candidate is
    what tells the two apart.

    The run stops at the first [top] reduction, so it costs one item, and it
    never touches [state]: the candidate may be wrong, and a wrong guess must
    leave the parse that continues alone. *)
let item_begins scan ~checkpoint ~target =
  let rec loop ~shifted index checkpoint =
    match checkpoint with
    | I.InputNeeded environment -> (
        let checkpoint = I.input_needed environment in
        if index >= Scan.count scan then false
        else
          match I.offer checkpoint (Scan.triple scan index) with
          | next -> loop ~shifted (index + 1) next
          | exception _ -> false)
    | I.AboutToReduce (_, production) -> (
        match I.lhs production with
        | I.X (I.N nonterminal)
          when shifted && I.compare_nonterminals nonterminal I.N_top = 0 ->
            true
        | _ -> (
            match I.resume checkpoint with
            | next -> loop ~shifted index next
            | exception _ -> false))
    | I.Shifting _ -> (
        match I.resume checkpoint with
        | next -> loop ~shifted:true index next
        | exception _ -> false)
    | I.HandlingError _ | I.Accepted _ | I.Rejected -> false
  in
  loop ~shifted:false target checkpoint

(** An item keyword at item level that follows the closing bracket of an
    annotation, or the [export] or [inline] that begins its item, is not an
    item boundary: the keyword belongs to the item that begins earlier, and a
    checkpoint taken at it would resume inside that item's production. The
    token that begins the item is the boundary. *)
let annotated_keyword state index =
  index > 0
  && (Scan.token state.scan index).depth = 0
  &&
  match (Scan.token state.scan (index - 1)).token with
  | Parser.RBRACKET | Parser.EXPORT | Parser.INLINE -> true
  | _ -> false

let record state ~index checkpoint =
  if index < Scan.count state.scan then (
    let entry = Scan.token state.scan index in
    if
      Scan.item_start entry.token
      && (not (annotated_keyword state index))
      && accepts checkpoint entry
    then state.boundaries := (index, checkpoint) :: !(state.boundaries);
    if Scan.statement_boundary state.scan index then
      state.statements := (index, checkpoint) :: !(state.statements))

(* The driver's inner loop: offer tokens, run reductions, stop when the
   parser accepts or refuses. *)
type step =
  | Accepted of Syntax.pprogram
  | Failed of int  (** The token index that could not be shifted. *)
  | Raised of int * exn

let rec drive state ~index ~eof_sent checkpoint =
  match checkpoint with
  | I.InputNeeded environment -> (
      let checkpoint = I.input_needed environment in
      state.input <- Some (index, environment);
      state.offered <- false;
      record state ~index checkpoint;
      (* A token that follows the closing brace of a top-level block is a
         restart point too: the item before it is complete, and this state
         still owes that item its reduction, which {!reposition} performs. *)
      if
        index > 0
        && index < Scan.count state.scan
        && (let prev = Scan.token state.scan (index - 1) in
            prev.token = Parser.RBRACE && prev.depth = 1)
        && not (List.mem_assoc index !(state.boundaries))
      then state.boundaries := (index, checkpoint) :: !(state.boundaries);
      if index < Scan.count state.scan then
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
  | I.AboutToReduce (_, production) -> (
      (* A completed item: the next item begins at the token the parser is
         looking at, when it is looking at one, and at the next token to
         read when the reduction was a default one. Only an item at the top
         level counts for the prefix: an item inside a namespace would leave
         the prefix with an unclosed brace. *)
      (match I.lhs production with
      | I.X (I.N nonterminal) ->
          if I.compare_nonterminals nonterminal I.N_top = 0 then
            let at = if state.offered then index - 1 else index in
            if
              at >= Scan.count state.scan
              || (Scan.token state.scan at).depth = 0
            then state.closed_item <- at
      | I.X (I.T _) -> ());
      match I.resume checkpoint with
      | next -> drive state ~index ~eof_sent next
      | exception exn -> Raised (index, exn))
  | I.Shifting _ -> (
      match I.resume checkpoint with
      | next -> drive state ~index ~eof_sent next
      | exception exn -> Raised (index, exn))
  | I.HandlingError _ -> Failed (max 0 (index - 1))
  | I.Accepted items -> Accepted items
  | I.Rejected -> Failed (max 0 (index - 1))

let start_checkpoint ~name ~position =
  Parser.Incremental.module_ { position with Lexing.pos_fname = name }

let initial_position =
  { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

(** Where an item begins: the end of the token before its first token.

    The compiler places an item at the position the parser holds when the
    item's production begins, and every item production but [param], [exec],
    [namespace] and one behind a [#[]] annotation begins with a nullable
    symbol ([annotations], or the optional [from?] of a [require]). Menhir
    gives such an empty production the position the parser has reached —
    [parser.mly], and the generated [let _startpos = _menhir_stack.endp] —
    which is the end of the last token that was shifted. So a straight parse
    starts each item where the item before it ends, and a restart has to
    resume with that same position or the item it produces swallows the
    bytes of the hole in front of it. The item's end needs no such care: it
    comes from its own last token, and {!reposition} moves only the cells the
    item that follows begins with, so the item before the hole keeps the end
    its own tokens give it. *)
let position_before scan target =
  if target <= 0 then initial_position
  else (Scan.token scan (target - 1)).end_pos

let new_state counters scan =
  let initial =
    start_checkpoint ~name:(Scan.name scan) ~position:initial_position
  in
  {
    scan;
    initial;
    boundaries = ref [ (0, initial) ];
    statements = ref [];
    tried = ref [];
    closed_item = 0;
    stubbed_upto = 0;
    input = None;
    offered = false;
    counters;
  }

(** A copy of [state] with its own checkpoint lists and its own counters, so
    an attempt that fails leaves nothing of an imaginary parse behind. *)
let fork state =
  {
    scan = state.scan;
    initial = state.initial;
    boundaries = ref !(state.boundaries);
    statements = ref !(state.statements);
    tried = ref !(state.tried);
    closed_item = state.closed_item;
    stubbed_upto = state.stubbed_upto;
    input = state.input;
    offered = state.offered;
    counters = no_counters ();
  }

let commit state attempt =
  state.boundaries := !(attempt.boundaries);
  state.statements := !(attempt.statements);
  state.tried := !(attempt.tried);
  state.closed_item <- attempt.closed_item;
  state.stubbed_upto <- attempt.stubbed_upto

(** Where the parser can be handed a token. *)
let rec wait_for_input checkpoint =
  match checkpoint with
  | I.InputNeeded environment -> Some (I.input_needed environment)
  | I.AboutToReduce _ | I.Shifting _ -> (
      match I.resume checkpoint with
      | next -> wait_for_input next
      | exception _ -> None)
  | I.HandlingError _ | I.Accepted _ | I.Rejected -> None

(** [reposition ~counters checkpoint ~position ~lead] is [checkpoint] with
    the parser left ready to read [lead], the token the restart resumes at,
    and with [position] as the position the item that begins there starts at.

    A checkpoint taken at an item boundary still owes the item before it its
    reduction: menhir decides that reduction from the next token, so the item
    has not been built yet and its end is still to be read off the top stack
    cell. Writing [position] into that cell, which is what this function used
    to do, therefore moved the end of the item before the hole to the end of
    the hole. Instead the parser is offered [lead], the token the restart
    reads anyway, which performs every reduction the item before the hole is
    owed with the positions the item's own tokens carry; the shift is then
    taken back off, so the token is read again by the driver. What is left to
    move is the cells the item that follows begins with: a production that
    starts with [annotations] or with the optional [from?] of a [require]
    takes the item's start from the end of such an empty cell, and moving
    those cells, and only those, is what puts the item at [position].

    The stack that comes out holds the same cells in the same states as one
    that never stopped, so the parse that continues is the one the checkpoint
    describes; only where the empty productions say they are is different.
    Nothing here can refuse: [lead] is an item boundary's token, which the
    checkpoint accepted when it was recorded, and each cell that is fed back
    was taken off the very state that put it there. A refusal is counted all
    the same, because it would leave the position the checkpoint carries —
    the defect this function exists to remove — in place. *)
let reposition ~counters checkpoint ~position ~lead =
  let refuse () =
    counters.refused_positions <- counters.refused_positions + 1;
    checkpoint
  in
  match checkpoint with
  | I.InputNeeded environment -> (
      match I.top environment with
      | None -> refuse ()
      | Some (I.Element (_, _, _, carried)) -> (
          if carried = position then checkpoint
          else
            match
              wait_for_input
                (I.offer checkpoint
                   (lead.Scan.token, lead.Scan.start_pos, lead.Scan.end_pos))
            with
            | None -> refuse ()
            | Some (I.InputNeeded shifted) -> (
                match I.pop shifted with
                | None -> refuse ()
                | Some below ->
                    let rec move env =
                      match I.top env with
                      | Some (I.Element (state, value, start, stop))
                        when start = carried && stop = carried -> (
                          match I.pop env with
                          | None -> env
                          | Some under ->
                              I.feed (I.incoming_symbol state) position value
                                position (move under))
                      | _ -> env
                    in
                    counters.moved_positions <- counters.moved_positions + 1;
                    I.input_needed (move below))
            | Some _ -> refuse ()))
  | _ -> refuse ()

(** [close_blocks checkpoint ~position ~count] offers [count] closing braces
    at [position]. [None] means the parser did not take them, which is how a
    failure that is not a missing brace is told apart from one that is. *)
let close_blocks checkpoint ~position ~count =
  let rec loop checkpoint remaining =
    if remaining <= 0 then Some checkpoint
    else
      match wait_for_input checkpoint with
      | None -> None
      | Some checkpoint -> (
          match I.offer checkpoint (Parser.RBRACE, position, position) with
          | next -> loop next (remaining - 1)
          | exception _ -> None)
  in
  loop checkpoint count

(** The name a broken item had, read from the tokens of the broken region so
    the stub can carry it. *)
let dropped_of scan ~from ~upto =
  let upto = min upto (Scan.count scan) in
  let kind_of = function
    | Parser.FN -> Some Function
    | Parser.PARAM -> Some Parameter
    | Parser.TYPE -> Some Type
    | Parser.NAMESPACE -> Some Namespace
    | Parser.EXEC -> Some Exec
    | Parser.REQUIRE | Parser.FROM -> Some Require
    | _ -> None
  in
  let rec keyword index =
    if index >= upto then None
    else
      match kind_of (Scan.token scan index).token with
      | Some kind -> Some (kind, index)
      | None -> keyword (index + 1)
  in
  (* The first name at or after [from], and the last one before [upto]. A
     parameter and a global are named after their type, so their name is the
     one next to the [=], not the first one in the header. *)
  let rec first_name index =
    if index >= upto then None
    else
      match (Scan.token scan index).token with
      | Parser.NID name -> Some name
      | _ -> first_name (index + 1)
  in
  let last_name ~from ~upto =
    let found = ref None in
    for index = from to upto - 1 do
      match (Scan.token scan index).token with
      | Parser.NID name -> found := Some name
      | _ -> ()
    done;
    !found
  in
  let rec first_eq index =
    if index >= upto then None
    else if (Scan.token scan index).token = Parser.EQ then Some index
    else first_eq (index + 1)
  in
  let declared kind at =
    match kind with
    | Parameter | Global -> (
        match first_eq (at + 1) with
        | Some eq -> last_name ~from:(at + 1) ~upto:eq
        | None -> first_name (at + 1))
    | _ -> first_name (at + 1)
  in
  let kind, name =
    match keyword from with
    | Some (kind, at) -> (kind, declared kind at)
    | None -> (
        (* No keyword left in the region: the broken item lost the one it
           began with, which is what deleting [fn] does. Its first token is
           then the name, and a name followed by a parenthesis names a
           function; a name before an [=] names a global. *)
        match first_eq from with
        | Some eq -> (
            match last_name ~from ~upto:eq with
            | Some name -> (Global, Some name)
            | None -> (Item, None))
        | None -> (
            match
              if from < upto then Some (Scan.token scan from).token else None
            with
            | Some (Parser.NID name) ->
                let called =
                  from + 1 < upto
                  && (Scan.token scan (from + 1)).token = Parser.LPAREN
                in
                ((if called then Function else Item), Some name)
            | _ -> (Item, None)))
  in
  Dropped
    {
      kind;
      name;
      span =
        Scan.span scan
          ~start_byte:(Scan.start_byte scan from)
          ~end_byte:(Scan.start_byte scan upto);
    }

let contains (outer : Jide_lang.Span.t) (inner : Jide_lang.Span.t) =
  outer.Jide_lang.Span.start_byte <= inner.Jide_lang.Span.start_byte
  && outer.Jide_lang.Span.end_byte >= inner.Jide_lang.Span.end_byte

(** [crosses_item scan ~source ~target] is true when a token strictly between
    [source] and [target] begins an item of a shallower block: the body the
    statement belongs to has closed and another item starts before the
    target. A statement restart that resumes there would parse the next
    item's body with the state of this one, so it is not a restart point. *)
let crosses_item scan ~source ~target =
  let depth = (Scan.token scan source).depth in
  let rec loop index =
    index < target
    && ((Scan.token scan index).depth < depth
        && Scan.item_only (Scan.token scan index).token
       || loop (index + 1))
  in
  loop (source + 1)

let statement_before state index =
  List.find_opt
    (fun (at, _) -> at < index && at > state.closed_item)
    !(state.statements)

let was_tried state (source, target) =
  List.exists (fun pair -> pair = (source, target)) !(state.tried)

let prune state source =
  state.boundaries :=
    List.filter (fun (at, _) -> at <= source) !(state.boundaries);
  state.statements :=
    List.filter (fun (at, _) -> at <= source) !(state.statements)

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
          when target > source
               && (not (was_tried state (source, target)))
               && not (crosses_item state.scan ~source ~target) ->
            Some (Restart { source; target; checkpoint; statement = true })
        | _ -> None)
  in
  let item () =
    let depth_of index = (Scan.token state.scan index).depth in
    let fit (source, _) target =
      source <= failure
      && source >= state.closed_item
      && target > source
      && depth_of target >= depth_of source
      && not (was_tried state (source, target))
    in
    let restart (source, checkpoint) target =
      Some (Restart { source; target; checkpoint; statement = false })
    in
    let choose (source, checkpoint) target =
      if
        fit (source, checkpoint) target
        && accepts checkpoint (Scan.token state.scan target)
      then restart (source, checkpoint) target
      else None
    in
    (* A token that begins a global is a restart target only when an item
       really begins at it. {!accepts} cannot tell a global's type from a
       name that continues the broken item, so the candidate is handed to the
       parser first: the one that comes back with a [top] production is the
       next item, and one that does not is simply the next candidate. The
       keyword targets below are asked first and are unchanged; this search
       reaches the items the item set cannot name. *)
    let choose_verified (source, checkpoint) targets =
      let rec loop = function
        | [] -> None
        | target :: rest -> (
            match choose (source, checkpoint) target with
            | Some _ as attempt ->
                if item_begins state.scan ~checkpoint ~target then attempt
                else loop rest
            | None -> loop rest)
      in
      loop targets
    in
    (* Newest checkpoint first. From each, the nearest item at its own depth
       keeps the block the checkpoint is in (the items of a namespace whose
       neighbour is broken), and the smallest depth left in the buffer is the
       fallback for a block that was never closed. *)
    let rec pick = function
      | [] -> None
      | ((source, _) as boundary) :: rest -> (
          let inside =
            match
              Scan.next_item_at_depth state.scan ~from:failure
                ~depth:(depth_of source)
            with
            | Some target -> choose boundary target
            | None -> None
          in
          match inside with
          | Some _ as attempt -> attempt
          | None -> (
              match Scan.next_item state.scan ~from:failure with
              | Some target -> (
                  match choose boundary target with
                  | Some _ as attempt -> attempt
                  | None -> pick rest)
              | None -> None))
    in
    match pick !(state.boundaries) with
    | Some _ as attempt -> attempt
    | None ->
        (* No token that begins an item with a keyword of its own is left
           ahead of the hole, so the item after it can only be a global. *)
        let rec any = function
          | [] -> None
          | ((source, _) as boundary) :: rest -> (
              match
                choose_verified boundary
                  (Scan.type_candidates state.scan ~from:failure
                     ~depth:(Some (depth_of source)))
              with
              | Some _ as attempt -> attempt
              | None -> (
                  match
                    choose_verified boundary
                      (Scan.type_candidates state.scan ~from:failure ~depth:None)
                  with
                  | Some _ as attempt -> attempt
                  | None -> any rest))
        in
        any !(state.boundaries)
  in
  match statement () with Some attempt -> Some attempt | None -> item ()

let recover ~name ~text : t =
  let counters = no_counters () in
  let dropped = ref [] in
  let recovered = ref [] in
  let diagnostics = ref [] in
  let raised = ref 0 in
  let note_raise exn =
    if !raised < 3 then (
      incr raised;
      diagnostics := Error.to_diagnostic (Error.of_exn exn) :: !diagnostics)
  in
  (* Where an item restart would go, or the end of the buffer when the
     broken item is the last one. *)
  let target_of state ~failure =
    match Scan.next_item state.scan ~from:failure with
    | Some target -> target
    | None -> Scan.count state.scan
  in
  (* Where the bytes of a hole end: where the item that follows it begins,
     which is the end of the last token in the hole. The two abut, as they do
     in a parse that was never interrupted, so a stub never overlaps the item
     after it — a typing diagnostic in that item is the program's own error
     and must not be dropped with the hole's. A hole that runs off the end of
     the buffer ends with the buffer. *)
  let hole_end state upto =
    if upto <= 0 then 0
    else if upto >= Scan.count state.scan then Scan.start_byte state.scan upto
    else (position_before state.scan upto).Lexing.pos_cnum
  in
  (* Where the hole a byte restart covers begins, as a token index. Until the
     restart has reported anything, that is the whole open item
     ([closed_item]). Once it has, the bytes before [stubbed_upto] are a stub
     already and the question is whether the bytes after it are the rest of
     the same item or an item of their own: a token that can begin a top-level
     item begins a new one, and anything else — a statement of the body the
     item opens — is the tail of the item that is already reported. The tail
     is reported from the item's own start, which replaces its stub with one
     covering the whole item, instead of as a second item the file does not
     have. *)
  let hole_start state target =
    let stubbed = state.stubbed_upto in
    if stubbed <= state.closed_item || stubbed >= target then state.closed_item
    else if Scan.item_only (Scan.token state.scan stubbed).token then stubbed
    else state.closed_item
  in
  (* One stub and one recovered region for the bytes of one hole. Two holes
     are two stubs even when one of them is first seen as part of a wider
     restart, which is why the restart that follows one starts at the end of
     the stub already reported ([stubbed_upto]) rather than at the last
     completed item. A restart that has nothing new to report adds nothing,
     and one that sees the same hole again from further out replaces it — the
     same item, reported once, over all of its bytes. *)
  let note_dropped state ~from ~upto =
    let span =
      Scan.span state.scan
        ~start_byte:(Scan.start_byte state.scan from)
        ~end_byte:(hole_end state upto)
    in
    (* The bytes the stubs already reported, which are disjoint and in source
       order: a hole that lies entirely inside them has nothing new to report,
       however many of them it takes to cover it. That is what a rewrite of
       the prefix sees: the holes it blanked are the stubs it was reported
       as, and the restart that follows one of them reads the buffer from the
       beginning again. Bytes between two reported holes are trivia — a line
       break between two broken items — and they are reported as missing with
       the holes they sit between. *)
    let covered (span : Jide_lang.Span.t) =
      let text = Scan.text state.scan in
      let length = String.length text in
      let blank start_byte end_byte =
        let rec loop index =
          index >= end_byte
          ||
          match text.[index] with
          | ' ' | '\t' | '\r' | '\n' -> loop (index + 1)
          | _ -> false
        in
        loop (max 0 start_byte)
      in
      let spans =
        List.filter_map (function Dropped { span; _ } -> Some span) !dropped
        |> List.sort (fun (a : Jide_lang.Span.t) (b : Jide_lang.Span.t) ->
               compare a.Jide_lang.Span.start_byte b.Jide_lang.Span.start_byte)
      in
      let rec walk reach = function
        | _ when reach >= span.Jide_lang.Span.end_byte -> true
        | [] -> blank reach span.Jide_lang.Span.end_byte
        | (earlier : Jide_lang.Span.t) :: rest ->
            if earlier.Jide_lang.Span.start_byte > reach then
              if blank reach earlier.Jide_lang.Span.start_byte then
                walk (max reach earlier.Jide_lang.Span.end_byte) rest
              else false
            else walk (max reach earlier.Jide_lang.Span.end_byte) rest
      in
      walk (min length span.Jide_lang.Span.start_byte) spans
    in
    (* A hole whose first bytes belong to a stub already reported cannot be
       stated as a span of its own without overlapping it. The bytes after it
       are the rest of an item whose stub is there, and {!hole_start} keeps
       that case out; anything left here would be a report of half a hole, so
       it is not made. *)
    let front_reported =
      List.exists
        (function
          | Dropped { span = earlier; _ } ->
              earlier.Jide_lang.Span.start_byte < span.Jide_lang.Span.start_byte
              && earlier.Jide_lang.Span.end_byte
                 > span.Jide_lang.Span.start_byte)
        !dropped
    in
    if
      span.Jide_lang.Span.start_byte < span.Jide_lang.Span.end_byte
      && not (covered span || front_reported)
    then (
      (* The stub's span is the hole's, so the stub and the region the typing
         loop stays quiet about are the same bytes; {!dropped_of} reads the
         name off the tokens and has no opinion about the extent. A hole that
         grows over a stub already reported is the same item seen again, so
         it keeps the name that stub was given: the wider range is read for
         its span, not for a second guess at what the item was. *)
      let stub =
        match
          List.find_opt
            (function Dropped { span = earlier; _ } -> contains span earlier)
            !dropped
        with
        | Some (Dropped fields) -> Dropped { fields with span }
        | None -> (
            match dropped_of state.scan ~from ~upto with
            | Dropped fields -> Dropped { fields with span })
      in
      dropped :=
        stub
        :: List.filter
             (function
               | Dropped { span = earlier; _ } -> not (contains span earlier))
             !dropped;
      state.stubbed_upto <- max state.stubbed_upto upto;
      recovered :=
        span :: List.filter (fun older -> not (contains span older)) !recovered)
  in
  (* One pass over one buffer: checkpoint restarts until the parser accepts,
     a rewrite when no checkpoint is left, the prefix alone when even that is
     gone. *)
  let rec pass ~depth text : Syntax.pprogram =
    match Scan.make ~name text with
    | Error diagnostic ->
        diagnostics := diagnostic :: !diagnostics;
        []
    | Ok scan ->
        recovered := List.rev_append (Scan.blanked scan) !recovered;
        run (new_state counters scan) ~depth
  and run state ~depth : Syntax.pprogram =
    match drive state ~index:0 ~eof_sent:false state.initial with
    | Accepted items -> items
    | Raised (_, exn) ->
        note_raise exn;
        finish state ~depth
    | Failed failure -> step state ~depth ~failure
  and step state ~depth ~failure : Syntax.pprogram =
    if counters.recoveries >= max_recoveries then finish state ~depth
    else
      match close_blocks_here state ~failure with
      | Some items -> items
      | None -> (
          match next_attempt state ~failure with
          | Some (Restart { source; target; checkpoint; statement }) ->
              counters.recoveries <- counters.recoveries + 1;
              if statement then
                counters.statement_restarts <- counters.statement_restarts + 1
              else counters.item_restarts <- counters.item_restarts + 1;
              state.tried := (source, target) :: !(state.tried);
              prune state source;
              if statement then (
                recovered :=
                  Scan.span state.scan
                    ~start_byte:(Scan.start_byte state.scan source)
                    ~end_byte:(Scan.start_byte state.scan target)
                  :: !recovered;
                run_from state ~depth ~start:target checkpoint)
              else (
                note_dropped state ~from:source ~upto:target;
                state.closed_item <- source;
                (* The item this restart produces must begin where the item
                   before it ends, which is the end of the token before
                   [target] and the position a parse that was never
                   interrupted gives it. A checkpoint carries the position of
                   the token before the *dropped* item instead, so it is
                   moved; the start state, which holds no symbol and so has
                   no position to move, is rebuilt at that position — it is
                   only reachable while nothing has been reduced, so nothing
                   is lost by starting it again. *)
                let position = position_before state.scan target in
                let checkpoint =
                  if source = 0 then
                    start_checkpoint ~name:(Scan.name state.scan) ~position
                  else
                    reposition ~counters:state.counters checkpoint ~position
                      ~lead:(Scan.token state.scan target)
                in
                run_from state ~depth ~start:target checkpoint)
          | None ->
              let target = target_of state ~failure in
              let start = hole_start state target in
              let from = Scan.start_byte state.scan start in
              let upto = Scan.start_byte state.scan target in
              if upto <= from || depth >= max_byte_restarts then
                finish state ~depth
              else (
                counters.recoveries <- counters.recoveries + 1;
                counters.byte_restarts <- counters.byte_restarts + 1;
                (* The blanking covers every byte the stub stands for, from
                   the beginning of the hole, so the text that is rewritten is
                   exactly the text that is reported as missing. *)
                note_dropped state ~from:start ~upto:target;
                pass ~depth:(depth + 1)
                  (Scan.blank (Scan.text state.scan) ~from ~upto)))
  (* A block that was never closed is closed without a byte being rewritten.
     The attempt runs on a copy: a virtual brace that the rest of the file
     cannot follow must leave the ordinary restarts as they were.

     Two things place the brace. The token decides whether one is tried at
     all: only a token that can begin a top-level item and can never begin a
     statement means the block was left open, because anything else could be
     the next statement or the middle of this one ({!Scan.item_only_tokens}).
     The position is the end of the last token the parser shifted, which is
     where the missing brace would have been written: menhir starts the next
     item where the last symbol ends, so a brace offered at the *following*
     token instead would push that item's start past the trivia between them
     and an outline would place it a few bytes late. A brace has no bytes of
     its own, so its start and its end are that one position. *)
  and close_blocks_here state ~failure : Syntax.pprogram option =
    match state.input with
    | None -> None
    | Some (at, environment) -> (
        let missing =
          if at <> failure then 0
          else if failure >= Scan.count state.scan then
            Scan.open_depth state.scan
          else
            let entry = Scan.token state.scan failure in
            if Scan.item_only entry.token then entry.depth else 0
        in
        if missing <= 0 then None
        else
          let position = position_before state.scan failure in
          let attempt = fork state in
          let checkpoint = I.input_needed environment in
          match close_blocks checkpoint ~position ~count:missing with
          | None -> None
          | Some checkpoint -> (
              match drive attempt ~index:failure ~eof_sent:false checkpoint with
              | Accepted items ->
                  commit state attempt;
                  counters.recoveries <- counters.recoveries + 1;
                  counters.brace_restarts <- counters.brace_restarts + 1;
                  Some items
              | Failed _ | Raised _ -> None))
  and run_from state ~depth ~start checkpoint =
    match drive state ~index:start ~eof_sent:false checkpoint with
    | Accepted items -> items
    | Raised (_, exn) ->
        note_raise exn;
        finish state ~depth
    | Failed failure -> step state ~depth ~failure
  (* Nothing left to try: keep the items that are already whole. The item
     that is still broken and everything after it are blanked, so a file with
     more faults than the bound returns the prefix instead of nothing. *)
  and finish state ~depth =
    ignore depth;
    let scan = state.scan in
    let base =
      List.fold_left
        (fun text (span : Jide_lang.Span.t) ->
          Scan.blank text ~from:span.Jide_lang.Span.start_byte
            ~upto:span.Jide_lang.Span.end_byte)
        (Scan.text scan) !recovered
    in
    let cut_index = Scan.item_root scan state.closed_item in
    let cut = Scan.start_byte scan cut_index in
    let text = Scan.blank base ~from:cut ~upto:(String.length base) in
    if String.equal text (Scan.text scan) then []
    else (
      if cut < String.length base then
        note_dropped state
          ~from:(max cut_index state.stubbed_upto)
          ~upto:(Scan.count scan);
      match Parseio.parse_program ~name (Utils.IO.input_string text) with
      | items -> items
      | exception _ -> [])
  in
  let items = pass ~depth:0 text in
  (* In source order, which is the order an outline shows them in and the
     order their diagnostics belong in. The restarts meet the bytes in the
     order they are resolved, which is not always left to right. *)
  let dropped =
    List.sort
      (fun (Dropped a) (Dropped b) ->
        compare
          (a.span.Jide_lang.Span.start_byte, a.span.Jide_lang.Span.end_byte)
          (b.span.Jide_lang.Span.start_byte, b.span.Jide_lang.Span.end_byte))
      !dropped
  in
  {
    items;
    dropped;
    recovered = List.sort_uniq compare !recovered;
    diagnostics = List.rev !diagnostics @ List.map diagnostic_of_dropped dropped;
    counters;
  }
