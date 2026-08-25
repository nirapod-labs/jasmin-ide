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
      span : Jide_lang.Span.t;  (** The bytes the hole replaced. *)
      at : Jide_lang.Span.t;
          (** The token the parse stopped at inside those bytes, which is
              where the hole is reported. The first byte of [span] would name
              a byte the user did write, the item's own keyword, while the
              fault is the token the parser could not shift. *)
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
    one file are two distinguishable markers.

    The marker is placed on the token the parse stopped at, not on the first
    byte of the hole: a hole that begins at byte zero would otherwise be
    reported at line 1, column 1, before the compiler's own error, which is
    the position a reader (and every tool that reads the first diagnostic)
    would take as the first error in the file. *)
let diagnostic_of_dropped = function
  | Dropped { kind; name; at; _ } ->
      let what =
        match name with
        | Some name -> Printf.sprintf "the %s `%s'" (kind_name kind) name
        | None -> Printf.sprintf "an item"
      in
      Jide_lang.Diagnostic.make ~code:"parse" ~source:"jasmin"
        ~severity:Jide_lang.Diagnostic.Error at
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

(** The bytes of the token at [failure], or the empty span at the end of the
    buffer when the failure is there. *)
let stop_of_scan scan ~(failure : int) : Jide_lang.Span.t =
  if failure >= Scan.count scan then
    let at = String.length (Scan.text scan) in
    Scan.span scan ~start_byte:at ~end_byte:at
  else
    let entry = Scan.token scan (max 0 failure) in
    Scan.span scan ~start_byte:entry.start_byte ~end_byte:entry.end_byte

(** The name a broken item had, read from the tokens of the broken region so
    the stub can carry it. *)
let dropped_of scan ~from ~upto ~at =
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
           began with, which is what deleting [fn] does. A region that begins
           with [inline] or [export] is a function: at item level those two
           begin nothing else. Otherwise its first token is the name, and a
           name followed by a parenthesis names a function; a name before an
           [=] names a global. *)
        match
          if from < upto then Some (Scan.token scan from).token else None
        with
        | Some (Parser.INLINE | Parser.EXPORT) -> (Function, first_name from)
        | _ -> (
            match first_eq from with
            | Some eq -> (
                match last_name ~from ~upto:eq with
                | Some name -> (Global, Some name)
                | None -> (Item, None))
            | None -> (
                match
                  if from < upto then Some (Scan.token scan from).token
                  else None
                with
                | Some (Parser.NID name) ->
                    let called =
                      from + 1 < upto
                      && (Scan.token scan (from + 1)).token = Parser.LPAREN
                    in
                    ((if called then Function else Item), Some name)
                | _ -> (Item, None))))
  in
  Dropped
    {
      kind;
      name;
      namespace = None;
      at;
      span =
        Scan.span scan
          ~start_byte:(Scan.start_byte scan from)
          ~end_byte:(Scan.start_byte scan upto);
    }

let contains (outer : Jide_lang.Span.t) (inner : Jide_lang.Span.t) =
  outer.Jide_lang.Span.start_byte <= inner.Jide_lang.Span.start_byte
  && outer.Jide_lang.Span.end_byte >= inner.Jide_lang.Span.end_byte

(** The terminals that carry no value, which are the ones a repair can
    write without inventing a name or a number. *)
let valueless =
  Parser.
    [
      LBRACE;
      RBRACE;
      LPAREN;
      RPAREN;
      LBRACKET;
      RBRACKET;
      SEMICOLON;
      COMMA;
      COLON;
      COLONCOLON;
      DOT;
      EQ;
      RARROW;
      NAMESPACE;
      FN;
      PARAM;
      TYPE;
      EXEC;
      REQUIRE;
      FROM;
      EXPORT;
      INLINE;
      REG;
      STACK;
      GLOBAL;
      POINTER;
      CONSTANT;
      MUTABLE;
      RETURN;
      IF;
      ELSE;
      FOR;
      TO;
      DOWNTO;
      WHILE;
      T_BOOL;
      T_INT;
      SHARPLBRACKET;
      SHARP;
      UNDERSCORE;
      QUESTIONMARK;
      ARRAYINIT;
      ASSERT;
      TRUE;
      FALSE;
      ALIGNED;
      UNALIGNED;
      PLUS;
      MINUS;
      STAR;
      AMP;
      AMPAMP;
      PIPE;
      PIPEPIPE;
      HAT;
      BANG;
      BANGEQ;
      EQEQ;
      LTLT;
      ROR;
      ROL;
    ]

type boundary = { at : int; checkpoint : checkpoint; level : int }
(** An item boundary the parse reached: the token index, the checkpoint
    that waits for that token, and the namespace depth there. *)

type frame = {
  mutable start : boundary;
  mutable fence : int;
  mutable repairs : int;
  mutable stopped : int option;
      (** The token the parse first stopped at in this item. *)
}
(** A broken item: the boundary it begins at and the fence it ends at. *)

type run = {
  scan : Scan.t;
  count : int;
  after : int array;
      (** [after.(i)] is the number of [}] minus the number of [{] in the
          tokens from [i] to the end: the brace depth before token [i] as
          the end of the buffer sees it. *)
  counters : counters;
  mutable dropped : dropped list;
  mutable recovered : Jide_lang.Span.t list;
  mutable tried : (int * int) list;  (** Statement restarts already made. *)
}
(** The bytes and tokens of one parse, and what it has reported. *)

type cursor = {
  mutable checkpoint : checkpoint;
  mutable index : int;
  mutable braces : int;  (** Braces shifted and not closed, virtual included. *)
  mutable boundary : boundary;  (** The last boundary reached. *)
  mutable look : bool;  (** Whether the last token read can end an item. *)
  mutable statements : (int * checkpoint * int) list;
      (** Statement checkpoints since [boundary], newest first. *)
  mutable history : (int * checkpoint * int) list;
      (** Every state since [boundary], newest first, so a fence found
          behind the failure can be returned to. *)
  mutable frames : frame list;
      (** Broken items being repaired, innermost first. *)
}
(** Where the parse is. [checkpoint] waits for the token at [index]. *)

let after_depths scan =
  let count = Scan.count scan in
  let after = Array.make (count + 1) 0 in
  for index = count - 1 downto 0 do
    after.(index) <-
      after.(index + 1) - brace_delta (Scan.token scan index).Scan.token
  done;
  after

(** The namespaces a boundary state is inside, qualified, read off the
    parser's own stack: each open namespace left its keyword there, with its
    name in the cell above it. *)
let namespace_of (environment : Syntax.pprogram I.env) =
  let rec walk environment above found =
    match I.top environment with
    | None -> found
    | Some (I.Element (state, value, _, _)) -> (
        let found, above =
          match I.incoming_symbol state with
          | I.T I.T_NAMESPACE -> (
              match above with
              | Some names -> (String.concat "::" names :: found, None)
              | None -> (found, None))
          | I.N I.N_separated_nonempty_list_COLONCOLON_NID_ ->
              (found, Some (value : string list))
          | _ -> (found, None)
        in
        match I.pop environment with
        | Some below -> walk below above found
        | None -> found)
  in
  match walk environment None [] with
  | [] -> None
  | names -> Some (String.concat "::" names)

(** One stub and one recovered region for the tokens [from] to [upto]. The
    stub's bytes run from its first token to the end of its last one, which
    is where the item after it begins, so the two abut as they do in a parse
    that was never interrupted. A stub that covers earlier ones, an item that
    was repaired and then dropped as a whole, replaces them. *)
let note_dropped ?namespace run ~failure ~from ~upto =
  let scan = run.scan in
  let start_byte = Scan.start_byte scan from in
  let end_byte =
    if upto >= run.count then String.length (Scan.text scan)
    else (position_before scan upto).Lexing.pos_cnum
  in
  if start_byte < end_byte then (
    let span = Scan.span scan ~start_byte ~end_byte in
    let stub =
      (* The marker goes on the token the parse first stopped at in this
         item, which may be the first token after the stub: an item that ran
         into the fence stopped there. *)
      let failure =
        match failure with
        | Some failure when failure >= from -> failure
        | Some _ | None -> upto
      in
      match dropped_of scan ~from ~upto ~at:(stop_of_scan scan ~failure) with
      | Dropped fields -> Dropped { fields with span; namespace }
    in
    run.dropped <-
      stub
      :: List.filter
           (function Dropped { span = inner; _ } -> not (contains span inner))
           run.dropped;
    run.recovered <-
      span :: List.filter (fun inner -> not (contains span inner)) run.recovered)

let note_recovered run ~from ~upto =
  let start_byte = Scan.start_byte run.scan from in
  let end_byte = Scan.start_byte run.scan upto in
  if start_byte < end_byte then
    run.recovered <- Scan.span run.scan ~start_byte ~end_byte :: run.recovered

(** [candidate run index] is true when the token there may begin an item:
    the parser still has to say whether one does. *)
let candidate run index =
  let token = (Scan.token run.scan index).Scan.token in
  if Scan.item_start token then true
  else if Scan.type_start token then
    index = 0 || not (continues_a_type (Scan.token run.scan (index - 1)).token)
  else false

(** [item_parses run environment ~start ~limit] is true when a whole item
    parses from [start] in the boundary state [environment], ending at or
    before [limit]: the parser comes back to a boundary with the braces it
    read balanced, before any error. *)
let item_parses run environment ~start ~limit =
  let rec loop checkpoint index net =
    if
      index > start && net = 0
      && ends_item (Scan.token run.scan (index - 1)).Scan.token
      && at_boundary checkpoint
    then true
    else if index >= limit || index >= run.count || net < 0 then false
    else
      match offer checkpoint (Scan.triple run.scan index) with
      | `Need next ->
          loop next (index + 1)
            (net + brace_delta (Scan.token run.scan index).Scan.token)
      | `Accepted _ | `Error -> false
  in
  loop (I.input_needed environment) start 0

(** [closes run environment index] is true when the [}] at [index] closes
    the namespace the boundary state [environment] is in. *)
let closes run environment index =
  match offer (I.input_needed environment) (Scan.triple run.scan index) with
  | `Need next -> at_boundary next
  | `Accepted _ | `Error -> false

(** The fence of the item broken at [boundary]: the first token after the
    boundary where a whole item parses, or the [}] that closes the boundary's
    namespace, or [limit] when there is neither before it.

    With [~outside:true] an item is a candidate only where the braces after
    it balance to the boundary's depth or less. An item deeper than that sits
    in a block the text after it closes, which is what the items of a
    namespace whose header is broken look like: they parse on their own, but
    they belong to that namespace. *)
let find_fence ?(outside = false) run (boundary : boundary) ~limit =
  match normalise boundary.checkpoint with
  | None -> limit
  | Some environment ->
      let rec search index =
        if index >= limit then limit
        else
          let token = (Scan.token run.scan index).Scan.token in
          if token = Parser.RBRACE then
            if
              boundary.level > 0
              && run.after.(index + 1) = boundary.level - 1
              && closes run environment index
            then index
            else search (index + 1)
          else if
            candidate run index
            && ((not outside) || run.after.(index) <= boundary.level)
            && item_parses run environment ~start:index ~limit
          then index
          else search (index + 1)
      in
      search (boundary.at + 1)

(** The boundary state [environment], ready to read the token at [target]
    with the position the item there begins at: the end of the token before
    it, as in a parse that was never interrupted. *)
let resume_at run environment ~target =
  if target >= run.count then I.input_needed environment
  else
    let lead = Scan.token run.scan target in
    let position = position_before run.scan target in
    if lead.Scan.token = Parser.RBRACE then I.input_needed environment
    else
      match I.top environment with
      | None ->
          (* The start state holds no symbol and so no position to move; it
             is only reachable before any item was read, so nothing is lost
             by starting it again at that position. *)
          start_checkpoint ~name:(Scan.name run.scan) ~position
      | Some _ ->
          reposition ~counters:run.counters
            (I.input_needed environment)
            ~position ~lead

let virtual_braces checkpoint ~position ~count =
  let rec loop checkpoint remaining =
    if remaining <= 0 then Some checkpoint
    else
      match offer checkpoint (Parser.RBRACE, position, position) with
      | `Need next -> loop next (remaining - 1)
      | `Accepted _ | `Error -> None
  in
  loop checkpoint count

(** [survives run checkpoint ~from ~braces ~level ~fence] is true when the
    next three tokens from [from] parse. A repair that reaches the fence
    sooner has to leave a whole item there: the parser is at a boundary of
    depth [level] or less, or inside a body that virtual braces close to one.
    [braces] is the depth the parser is at before [from]. *)
let survives run checkpoint ~from ~braces ~level ~fence =
  let rec loop checkpoint index braces remaining =
    if remaining = 0 then true
    else if index >= fence || index >= run.count then
      (braces <= level && at_boundary checkpoint)
      || braces > level
         &&
         match
           virtual_braces checkpoint
             ~position:(position_before run.scan index)
             ~count:(braces - level)
         with
         | Some closed -> at_boundary closed
         | None -> false
    else
      let token = (Scan.token run.scan index).Scan.token in
      match offer checkpoint (Scan.triple run.scan index) with
      | `Need next ->
          loop next (index + 1) (braces + brace_delta token) (remaining - 1)
      | `Accepted _ -> true
      | `Error -> false
  in
  loop checkpoint from braces 3

(** [item_keyword run index] is true when the token there begins an item and
    can begin nothing else. [export] is also an annotation label, and inside
    a [#[...]] group it begins nothing. *)
let item_keyword run index =
  let token = (Scan.token run.scan index).Scan.token in
  Scan.item_only token
  && not
       (token = Parser.EXPORT && index > 0
       &&
       match (Scan.token run.scan (index - 1)).Scan.token with
       | Parser.SHARPLBRACKET | Parser.COMMA | Parser.EQ -> true
       | _ -> false)

(** The first token of the item whose keyword is at [index]: back over
    [export] and [inline], and over [#[...]] groups, but not past [from]. *)
let item_head run ~from index =
  let token at = (Scan.token run.scan at).Scan.token in
  let rec group at depth =
    (* [at] is inside a group, scanning back; [depth] brackets are open. *)
    if at < from then None
    else
      match token at with
      | Parser.RBRACKET -> group (at - 1) (depth + 1)
      | Parser.LBRACKET -> group (at - 1) (depth - 1)
      | Parser.SHARPLBRACKET ->
          if depth = 1 then Some at else group (at - 1) (depth - 1)
      | _ -> group (at - 1) depth
  in
  let rec back at =
    if at - 1 < from then at
    else
      match token (at - 1) with
      | Parser.INLINE | Parser.EXPORT -> back (at - 1)
      | Parser.RBRACKET -> (
          match group (at - 2) 1 with
          | Some opening -> back opening
          | None -> at)
      | _ -> at
  in
  back index

(** The next item keyword after [from] and before [fence] that is not part
    of the item that begins at [from], or [fence]. *)
let next_keyword run ~from ~fence =
  let rec search index =
    if index >= fence then fence
    else if item_keyword run index then
      let head = item_head run ~from index in
      if head > from then head else search (index + 1)
    else search (index + 1)
  in
  search (from + 1)

(** [set_boundary cursor ~at ~checkpoint ~level] records a boundary. The
    parse goes on from [working] when it is given: a state that already
    carries the position of the item at [at] (see {!resume_at}), and so is
    one step into that item. The boundary keeps the state before that step,
    which is the one a later restart of the same item has to begin from. *)
let set_boundary ?working cursor ~at ~checkpoint ~level =
  let working = Option.value working ~default:checkpoint in
  cursor.boundary <- { at; checkpoint; level };
  cursor.checkpoint <- working;
  cursor.statements <- [];
  cursor.history <- [ (at, working, level) ]

let rec drive run cursor : Syntax.pprogram =
  let index = cursor.index in
  if cursor.look && at_boundary cursor.checkpoint then
    set_boundary cursor ~at:index ~checkpoint:cursor.checkpoint
      ~level:cursor.braces;
  cursor.look <- false;
  if cursor.boundary.at <> index then
    cursor.history <-
      (index, cursor.checkpoint, cursor.braces) :: cursor.history;
  if
    index < run.count
    && cursor.braces > cursor.boundary.level
    && Scan.statement_boundary run.scan index
  then
    cursor.statements <-
      (index, cursor.checkpoint, cursor.braces) :: cursor.statements;
  match cursor.frames with
  | frame :: _ when index = frame.fence -> at_fence run cursor frame
  | _ -> (
      if index >= run.count then at_end run cursor
      else
        let token = (Scan.token run.scan index).Scan.token in
        match offer cursor.checkpoint (Scan.triple run.scan index) with
        | `Need next ->
            cursor.checkpoint <- next;
            cursor.index <- index + 1;
            cursor.braces <- cursor.braces + brace_delta token;
            cursor.look <- may_precede_item token;
            drive run cursor
        | `Accepted items -> items
        | `Error -> failure run cursor)

(* The end of the buffer. At a boundary the namespaces still open are closed
   and the end is read; anywhere else the item that is open is broken. *)
and at_end run cursor =
  if cursor.boundary.at = cursor.index then
    let position = position_before run.scan run.count in
    let closed =
      if cursor.braces <= 0 then Some cursor.checkpoint
      else (
        run.counters.recoveries <- run.counters.recoveries + 1;
        run.counters.brace_restarts <- run.counters.brace_restarts + 1;
        virtual_braces cursor.checkpoint ~position ~count:cursor.braces)
    in
    match closed with
    | None -> []
    | Some checkpoint -> (
        match offer checkpoint (Scan.eof_triple run.scan) with
        | `Accepted items -> items
        | `Need _ | `Error -> [])
  else failure run cursor

(* The parse reached the fence of the item it is repairing. If it is at a
   boundary of the item's own depth the item is whole; if it is inside a body
   of that item, virtual braces close it; otherwise the item is dropped. *)
and at_fence run cursor frame =
  let level = frame.start.level in
  (* A repair may have closed the namespace the item was in, a [}] written
     for a [;]: a boundary at a shallower depth ends the item as well. *)
  if cursor.boundary.at = cursor.index && cursor.braces <= level then (
    cursor.frames <- List.tl cursor.frames;
    drive run cursor)
  else
    let missing = cursor.braces - level in
    let closed =
      if missing <= 0 then None
      else
        match
          virtual_braces cursor.checkpoint
            ~position:(position_before run.scan cursor.index)
            ~count:missing
        with
        | Some checkpoint when at_boundary checkpoint -> Some checkpoint
        | _ -> None
    in
    match closed with
    | Some checkpoint ->
        run.counters.recoveries <- run.counters.recoveries + 1;
        run.counters.brace_restarts <- run.counters.brace_restarts + 1;
        cursor.checkpoint <- checkpoint;
        cursor.braces <- level;
        set_boundary cursor ~at:cursor.index ~checkpoint ~level;
        cursor.frames <- List.tl cursor.frames;
        drive run cursor
    | None -> drop run cursor frame

and failure run cursor =
  let at = cursor.index in
  let frame =
    match cursor.frames with
    | frame :: _ when frame.start.at = cursor.boundary.at -> frame
    | _ ->
        let limit =
          match cursor.frames with frame :: _ -> frame.fence | [] -> run.count
        in
        let frame =
          {
            start = cursor.boundary;
            fence = find_fence run cursor.boundary ~limit;
            repairs = 0;
            stopped = None;
          }
        in
        cursor.frames <- frame :: cursor.frames;
        frame
  in
  if frame.stopped = None then frame.stopped <- Some at;
  if
    run.counters.recoveries < max_recoveries
    && frame.repairs < max_repairs_per_item
    && repair_namespace run cursor frame
  then (
    frame.repairs <- frame.repairs + 1;
    run.counters.recoveries <- run.counters.recoveries + 1;
    drive run cursor)
  else if frame.fence <= at then (
    (* The item that follows began before the token that failed: a body
       left open read the first tokens of the next item ([inline] before
       [fn]) and failed later. The parse goes back to the fence, where the
       broken item has to end. *)
    (match
       List.find_opt (fun (index, _, _) -> index = frame.fence) cursor.history
     with
    | Some (index, checkpoint, braces) ->
        cursor.checkpoint <- checkpoint;
        cursor.index <- index;
        cursor.braces <- braces;
        cursor.look <- false;
        cursor.statements <-
          List.filter
            (fun (index, _, _) -> index < frame.fence)
            cursor.statements;
        cursor.history <-
          List.filter (fun (index, _, _) -> index < frame.fence) cursor.history
    | None -> ());
    if cursor.index = frame.fence then at_fence run cursor frame
    else drop run cursor frame)
  else if run.counters.recoveries >= max_recoveries then give_up run cursor
  else if frame.repairs < max_repairs_per_item && repair run cursor frame then (
    frame.repairs <- frame.repairs + 1;
    run.counters.recoveries <- run.counters.recoveries + 1;
    drive run cursor)
  else drop run cursor frame

(* The repairs that keep the broken item, in the order of what they lose:
   nothing, one statement, one token. Each stays inside the fence. *)
and repair run cursor frame =
  let at = cursor.index in
  let level = frame.start.level in
  let entry = Scan.token run.scan at in
  let braces () =
    if Scan.item_only entry.Scan.token && cursor.braces > level then
      match
        virtual_braces cursor.checkpoint
          ~position:(position_before run.scan at)
          ~count:(cursor.braces - level)
      with
      | Some checkpoint when at_boundary checkpoint ->
          run.counters.brace_restarts <- run.counters.brace_restarts + 1;
          cursor.checkpoint <- checkpoint;
          cursor.braces <- level;
          set_boundary cursor ~at ~checkpoint ~level;
          true
      | _ -> false
    else false
  in
  let statement () =
    match
      List.find_opt (fun (index, _, _) -> index <= at) cursor.statements
    with
    | None -> false
    | Some (source, checkpoint, braces) -> (
        let depth = (Scan.token run.scan source).Scan.depth in
        match Scan.next_statement run.scan ~from:at ~depth with
        | Some target
          when target > source && target <= frame.fence
               && not (List.mem (source, target) run.tried) ->
            run.tried <- (source, target) :: run.tried;
            run.counters.statement_restarts <-
              run.counters.statement_restarts + 1;
            note_recovered run ~from:source ~upto:target;
            cursor.checkpoint <- checkpoint;
            cursor.index <- target;
            cursor.braces <- braces;
            cursor.look <- false;
            cursor.statements <-
              List.filter
                (fun (index, _, _) -> index <= source)
                cursor.statements;
            cursor.history <-
              List.filter (fun (index, _, _) -> index < source) cursor.history;
            true
        | _ -> false)
  in
  (* A token deleted, or a [;] and a [}] written for each other, kept when
     the three tokens after it parse. Where both do, the one that leaves the
     parser at the depth the rest of the buffer closes is taken: the text
     after the fault is the text as it was, so that depth is the true one. *)
  let token_repair () =
    (* Inside a body, or a stray token between two items. A header with one
       token wrong is not mended: the item is dropped for a stub, as every
       other broken header is. *)
    cursor.braces > level
    || at = cursor.boundary.at
       &&
       let deleted =
         if
           survives run cursor.checkpoint ~from:(at + 1) ~braces:cursor.braces
             ~level ~fence:frame.fence
         then [ (None, cursor.checkpoint, cursor.braces) ]
         else []
       in
       let replaced =
         match entry.Scan.token with
         | (Parser.RBRACE | Parser.SEMICOLON) as written -> (
             let token =
               if written = Parser.RBRACE then Parser.SEMICOLON
               else Parser.RBRACE
             in
             match
               offer cursor.checkpoint
                 (token, entry.Scan.start_pos, entry.Scan.end_pos)
             with
             | `Need checkpoint
               when survives run checkpoint ~from:(at + 1)
                      ~braces:(cursor.braces + brace_delta token)
                      ~level ~fence:frame.fence ->
                 [ (Some token, checkpoint, cursor.braces + brace_delta token) ]
             | _ -> [])
         | _ -> []
       in
       let options = deleted @ replaced in
       let consistent (_, _, braces) = braces = run.after.(at + 1) in
       let chosen =
         match List.find_opt consistent options with
         | Some option -> Some option
         | None -> (
             match options with option :: _ -> Some option | [] -> None)
       in
       match chosen with
       | None -> false
       | Some (written, checkpoint, braces) ->
           run.counters.token_repairs <- run.counters.token_repairs + 1;
           (* The token's own bytes, not the trivia after it, which belongs to
              the item that may begin there. *)
           run.recovered <-
             Scan.span run.scan ~start_byte:entry.Scan.start_byte
               ~end_byte:entry.Scan.end_byte
             :: run.recovered;
           cursor.index <- at + 1;
           cursor.braces <- braces;
           (match written with
           | Some token ->
               cursor.checkpoint <- checkpoint;
               cursor.look <- may_precede_item token
           | None -> (
               (* The token after a deleted one is read with the state before
                  it. If that state is a boundary, the item that begins there
                  starts where the deleted token ended, the end of the token
                  before it in the buffer. *)
               cursor.look <- false;
               match
                 if at + 1 < run.count && at_boundary checkpoint then
                   normalise checkpoint
                 else None
               with
               | Some environment ->
                   set_boundary cursor ~at:(at + 1)
                     ~checkpoint:(I.input_needed environment)
                     ~working:(resume_at run environment ~target:(at + 1))
                     ~level:braces
               | None -> cursor.look <- true));
           true
  in
  braces () || statement () || token_repair ()

(* A namespace is the one item that holds items, so a namespace whose
   header is broken would take every item inside it with it. One token
   written, deleted or replaced in the header is tried, and kept only when
   the whole namespace then parses, up to the fence. *)
and repair_namespace run cursor frame =
  let at = cursor.index in
  let level = frame.start.level in

  let start = frame.start in
  let first = Scan.token run.scan start.at in
  let fence =
    lazy
      (find_fence ~outside:true run start
         ~limit:
           (match cursor.frames with
           | _ :: outer :: _ -> outer.fence
           | _ -> run.count))
  in
  let whole checkpoint ~from =
    let fence = Lazy.force fence in
    let rec loop checkpoint index braces =
      if index >= fence || index >= run.count then
        index = fence && braces = level && at_boundary checkpoint
      else
        let token = (Scan.token run.scan index).Scan.token in
        match offer checkpoint (Scan.triple run.scan index) with
        | `Need next -> loop next (index + 1) (braces + brace_delta token)
        | `Accepted _ | `Error -> false
    in
    loop checkpoint from
  in
  let take checkpoint ~index ~braces =
    frame.fence <- Lazy.force fence;
    run.counters.token_repairs <- run.counters.token_repairs + 1;
    (let first = min index at and last = max index at in
     if last < run.count then
       run.recovered <-
         Scan.span run.scan
           ~start_byte:(Scan.token run.scan first).Scan.start_byte
           ~end_byte:(Scan.token run.scan last).Scan.end_byte
         :: run.recovered);
    cursor.checkpoint <- checkpoint;
    cursor.index <- index;
    cursor.braces <- braces;
    cursor.look <- false;
    cursor.statements <- [];
    cursor.history <- [ (index, checkpoint, braces) ];
    true
  in
  let virtual_token checkpoint token position =
    match offer checkpoint (token, position, position) with
    | `Need next -> Some next
    | `Accepted _ | `Error -> None
  in
  if first.Scan.token = Parser.NAMESPACE then
    (* The keyword is there: one token at the failure is missing, extra or
       wrong. *)
    let position = position_before run.scan at in
    let written =
      List.filter_map
        (fun token ->
          if try I.acceptable cursor.checkpoint token position with _ -> false
          then
            match virtual_token cursor.checkpoint token position with
            | Some next
              when whole next ~from:at (cursor.braces + brace_delta token) ->
                Some (next, at, cursor.braces + brace_delta token)
            | _ -> None
          else None)
        valueless
    in
    let deleted =
      if whole cursor.checkpoint ~from:(at + 1) cursor.braces then
        [ (cursor.checkpoint, at + 1, cursor.braces) ]
      else []
    in
    match deleted @ written with
    | (checkpoint, index, braces) :: _ -> take checkpoint ~index ~braces
    | [] -> false
  else if cursor.braces = level then
    (* The keyword is missing, or its token is not the keyword: the name and
       the brace of a header are there, at the item's first token or after
       it. *)
    let header index =
      index + 1 < run.count
      && (match (Scan.token run.scan index).Scan.token with
         | Parser.NID _ -> true
         | _ -> false)
      && (Scan.token run.scan (index + 1)).Scan.token = Parser.LBRACE
    in
    let position = first.Scan.start_pos in
    let attempt ~from =
      match virtual_token start.checkpoint Parser.NAMESPACE position with
      | Some next when whole next ~from level -> Some next
      | _ -> None
    in
    match if header start.at then attempt ~from:start.at else None with
    | Some next -> take next ~index:start.at ~braces:level
    | None -> (
        match
          if header (start.at + 1) then attempt ~from:(start.at + 1) else None
        with
        | Some next -> take next ~index:(start.at + 1) ~braces:level
        | None -> false)
  else false

(* The broken item is dropped: one stub for its tokens, and the parse goes on
   at the fence from the state of the boundary it began at. *)
and drop run cursor frame =
  (* A keyword that begins an item and nothing else, between the broken item
     and its fence, is a second broken item: it is stubbed on its own, so two
     holes are two markers. The fence stays where it is, since no whole item
     begins before it. *)
  let upto = next_keyword run ~from:frame.start.at ~fence:frame.fence in
  let environment = normalise frame.start.checkpoint in
  note_dropped
    ?namespace:(Option.bind environment namespace_of)
    run ~failure:frame.stopped ~from:frame.start.at ~upto;
  run.counters.recoveries <- run.counters.recoveries + 1;
  run.counters.item_restarts <- run.counters.item_restarts + 1;
  let level = frame.start.level in
  match environment with
  | None -> []
  | Some environment ->
      let working = resume_at run environment ~target:upto in
      cursor.index <- upto;
      cursor.braces <- level;
      cursor.look <- false;
      set_boundary cursor ~at:upto
        ~checkpoint:(I.input_needed environment)
        ~working ~level;
      (if upto < frame.fence then (
         frame.start <- cursor.boundary;
         frame.repairs <- 0;
         frame.stopped <- None)
       else
         let rec pop = function
           | [] -> []
           | inner :: rest -> if inner == frame then rest else pop rest
         in
         cursor.frames <- pop cursor.frames);
      drive run cursor

(* The bound is reached: the rest of the buffer is one stub, and the items
   already read are kept. *)
and give_up run cursor =
  let boundary = cursor.boundary in
  let environment = normalise boundary.checkpoint in
  let stopped =
    match cursor.frames with
    | frame :: _ when frame.start.at = boundary.at -> frame.stopped
    | _ -> Some cursor.index
  in
  note_dropped
    ?namespace:(Option.bind environment namespace_of)
    run ~failure:stopped ~from:boundary.at ~upto:run.count;
  cursor.frames <- [];
  match environment with
  | None -> []
  | Some environment ->
      let checkpoint = I.input_needed environment in
      cursor.checkpoint <- checkpoint;
      cursor.index <- run.count;
      cursor.braces <- boundary.level;
      cursor.look <- false;
      set_boundary cursor ~at:run.count ~checkpoint ~level:boundary.level;
      at_end run cursor

let recover ~name ~text : t =
  let counters = no_counters () in
  match Scan.make ~name text with
  | Error diagnostic -> { (empty counters) with diagnostics = [ diagnostic ] }
  | Ok scan ->
      let run =
        {
          scan;
          count = Scan.count scan;
          after = after_depths scan;
          counters;
          dropped = [];
          recovered = Scan.blanked scan;
          tried = [];
        }
      in
      let initial =
        start_checkpoint ~name:(Scan.name scan) ~position:initial_position
      in
      let cursor =
        {
          checkpoint = initial;
          index = 0;
          braces = 0;
          boundary = { at = 0; checkpoint = initial; level = 0 };
          look = false;
          statements = [];
          history = [ (0, initial, 0) ];
          frames = [];
        }
      in
      let items = try drive run cursor with _ -> [] in
      (* In source order, which is the order an outline shows them in and
         the order their diagnostics belong in. *)
      let dropped =
        List.sort
          (fun (Dropped a) (Dropped b) ->
            compare
              (a.span.Jide_lang.Span.start_byte, a.span.Jide_lang.Span.end_byte)
              (b.span.Jide_lang.Span.start_byte, b.span.Jide_lang.Span.end_byte))
          run.dropped
      in
      {
        items;
        dropped;
        recovered = List.sort_uniq compare run.recovered;
        diagnostics = List.map diagnostic_of_dropped dropped;
        counters;
      }
