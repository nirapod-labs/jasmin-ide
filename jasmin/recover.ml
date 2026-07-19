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

