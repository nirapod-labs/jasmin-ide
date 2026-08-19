(** The recovering parse.

    The compiler's parser gives up at the first syntax error: its one [error]
    production is only reachable from the start state, so a broken item
    unwinds the whole parse. This module drives the same parser through
    Menhir's incremental API, item by item, and on an error decides two
    things apart: where the broken item ends, and how much of it to keep.

    {b Where the broken item ends.} A broken item begins at the last item
    boundary the parse reached, a state where the parser waits for the first
    token of an item (it accepts [param], which nothing but an item can
    begin with). It ends at the first token after that boundary from which a
    whole item really parses: the parser is started in the boundary's own
    state at that token, and the candidate counts only if a complete item
    comes out of it, with its braces balanced, before any error. Inside a
    namespace, the [}] that closes the namespace ends the broken item too.

    That end is a fence. Every item the fault did not touch is a whole item in
    the text, so it parses from the boundary's state wherever it stands, and
    the fence is at or before it. Nothing done to the broken item may cross
    the fence, so no repair of the broken item can swallow a later item, and
    no later item depends on what the fault did to the brace depth of the
    text in between. The test is the parser's, not a token class or a depth:
    a [u64] that a statement left behind does not begin a global unless a
    global really parses from it.

    Two lexical facts keep the search exact where the parser alone cannot
    decide. A type right after a storage class ([reg], [stack], [inline],
    [global], [ptr], [mut], [const]) or after [::] is the middle of a
    declaration or a name, never the start of an item, because no item ends
    with those tokens; without this, [reg u64 x = 0;] inside a broken body
    would offer the global [u64 x = 0;]. And a [}] closes the namespace only
    if the braces after it balance to the namespace's own depth: the text
    after the fault is the text as it was, so counting from the end of the
    buffer gives the true depth there, whatever the fault did to the braces
    before it.

    {b How much of the broken item to keep.} Inside the fence the driver
    tries, in order: virtual [}] tokens where the parser has reached the
    fence, or a keyword that can never begin a statement, inside an open
    body, so the header and the body read so far stay in the tree; a
    statement restart from the last statement checkpoint, so a broken
    statement is dropped and its function is kept; deleting the token that
    failed, or writing [;] for [}] and [}] for [;], when the three tokens
    after it then parse. When none of these brings the parser to a boundary
    at the fence, the item is dropped: a [Dropped] stub stands for it, the
    parser resumes at the fence in the boundary's state, and the item there
    begins where the token before it ends, as in an uninterrupted parse.

    A dropped item leaves a [Dropped] stub carrying its name, so an outline
    can still list it. The stub is not a [Syntax.pitem]: that type belongs to
    the compiler and adding a constructor to it would stop typing from
    consuming the tree unchanged. {!Parse.items_with_stubs} merges the stubs
    into the item stream in source order, which is what an outline walks.

    Every restart is bounded, at most {!max_recoveries} per file; past the
    bound the rest of the file is one stub and the items before it are kept.
    Every call into the parser is wrapped: no exception leaves this module. *)

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
      namespace : string option;
          (** The namespace the item was in, qualified ([A::B]), or [None] at
              the top level. A reference to the item from outside the
              namespace is written with this in front of its name. *)
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
  mutable recoveries : int;  (** Restarts and repairs of any kind. *)
  mutable item_restarts : int;  (** Broken items dropped for a stub. *)
  mutable statement_restarts : int;
  mutable brace_restarts : int;  (** Closings with a brace that was missing. *)
  mutable token_repairs : int;
      (** A token deleted, or a [;] and a [}] written for each other. *)
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
    token_repairs = 0;
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

(** How many restarts and repairs one file may have. *)
let max_recoveries = 50

(** How many repairs one broken item may have before it is dropped. A repair
    that only moves the failure along is not worth more than a few tries:
    the stub is one diagnostic, and every repair is a guess. *)
let max_repairs_per_item = 8

let empty counters =
  { items = []; dropped = []; recovered = []; diagnostics = []; counters }

type checkpoint = Syntax.pprogram I.checkpoint

let start_checkpoint ~name ~position =
  Parser.Incremental.module_ { position with Lexing.pos_fname = name }

let initial_position =
  { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

(** Where an item begins: the end of the token before its first token.

    The compiler places an item at the position the parser holds when the
    item's production begins, and every item production but [param], [exec],
    [namespace] and one behind a [#[]] annotation begins with a nullable
    symbol ([annotations], or the optional [from?] of a [require]). Menhir
    gives such an empty production the position the parser has reached,
    which is the end of the last token that was shifted. So a straight parse
    starts each item where the token before it ends, and a restart has to
    resume with that same position or the item it produces swallows the
    bytes of the hole in front of it. *)
let position_before scan target =
  if target <= 0 then initial_position
  else (Scan.token scan (target - 1)).end_pos

(** The parser run to its next request for a token, or to its end. A
    semantic action that raises is a failure like any other. *)
let rec settle (checkpoint : checkpoint) =
  match checkpoint with
  | I.InputNeeded _ -> `Need checkpoint
  | I.Shifting _ | I.AboutToReduce _ -> (
      match I.resume checkpoint with
      | next -> settle next
      | exception _ -> `Error)
  | I.Accepted items -> `Accepted items
  | I.HandlingError _ | I.Rejected -> `Error

let offer (checkpoint : checkpoint) triple =
  match I.offer checkpoint triple with
  | next -> settle next
  | exception _ -> `Error

(** [at_boundary checkpoint] is true when the parser waits for the first
    token of an item: [param] begins no annotation, no statement and no
    expression, so a state that accepts it is between two items, whatever
    the item before it still owes in reductions. *)
let at_boundary (checkpoint : checkpoint) =
  try I.acceptable checkpoint Parser.PARAM Lexing.dummy_pos with _ -> false

(** The state of a boundary with the item before it reduced: [param] is
    offered, which performs every reduction the item before it is owed with
    the positions its own tokens carry, and taken back off. The state that is
    left is the one every item at this level begins from. *)
let normalise (checkpoint : checkpoint) =
  match offer checkpoint (Parser.PARAM, Lexing.dummy_pos, Lexing.dummy_pos) with
  | `Need (I.InputNeeded environment) -> I.pop environment
  | _ -> None

(** Whether a token can be the last token of an item: each item ends with a
    [}], a [;], the [)] of an [exec], or a string of a [require]. *)
let ends_item = function
  | Parser.RBRACE | Parser.SEMICOLON | Parser.RPAREN | Parser.STRING _ -> true
  | _ -> false

(** Whether a boundary can follow a token: after the last token of an item,
    or after the [{] that opens a namespace, where its first item begins. *)
let may_precede_item = function
  | Parser.LBRACE -> true
  | token -> ends_item token

let brace_delta = function Parser.LBRACE -> 1 | Parser.RBRACE -> -1 | _ -> 0

(** Tokens after which a type is the middle of a declaration or of a name:
    a storage class, a pointer qualifier, or [::]. No item ends with one of
    them, so a type that follows one never begins an item. *)
let continues_a_type = function
  | Parser.REG | Parser.STACK | Parser.INLINE | Parser.GLOBAL | Parser.POINTER
  | Parser.CONSTANT | Parser.MUTABLE | Parser.COLONCOLON ->
      true
  | _ -> false

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

    A boundary state may still owe the item before it its reduction: menhir
    decides that reduction from the next token, so the item has not been
    built yet and its end is still to be read off the top stack cell, which
    must therefore not be written. Instead the parser is offered [lead],
    which performs every reduction the item before the hole is owed with the
    positions the item's own tokens carry; the shift is then taken back off,
    so the token is read again by the driver. What is left to move is the
    cells the item that follows begins with: a production that starts with
    [annotations] or with the optional [from?] of a [require] takes the
    item's start from the end of such an empty cell, and moving those cells,
    and only those, is what puts the item at [position].

    The stack that comes out holds the same cells in the same states as one
    that never stopped, so the parse that continues is the one the checkpoint
    describes; only where the empty productions say they are is different.
    It is one step into the item at [lead], so it is not itself a boundary
    state. Nothing here should refuse: [lead] is the first token of a whole
    item that parsed from this state. A refusal is counted all the same,
    because it would leave the position the checkpoint carries in place. *)
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
