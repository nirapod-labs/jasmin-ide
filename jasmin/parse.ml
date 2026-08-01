(** The fast path and the recovering one.

    The compiler's own parse comes first and its result is used unchanged
    when it succeeds, so a file that is already correct costs one parse and
    nothing else. On a syntax error the compiler's error is kept as the
    first diagnostic, byte for byte, and [Recover] is asked for the items
    that do parse. Every item [Recover] could not keep is reported after it,
    at the item's own span, so a hole in the tree is a marker in the editor.

    Nothing is normalised before the fast path, not even a byte order mark:
    the compiler rejects a mark at byte zero, and a recovering parse that
    blanked it first would report the file clean while [jasminc] reports an
    error. The mark is blanked by the recovery, which keeps the compiler's
    error and still returns the items after the mark. *)

open Jasmin

type t = Recover.t = {
  items : Syntax.pprogram;
  dropped : Recover.dropped list;
  recovered : Jide_lang.Span.t list;
  diagnostics : Jide_lang.Diagnostic.t list;
  counters : Recover.counters;
}
(** The parse result is {!Recover.t} under this module's own name. The record
    is written out again, field for field, because the tree's index reads it
    through this module's path ([Jide_jasmin.Parse.items]): an abbreviation
    alone leaves the fields unbound there. The equation keeps the two types
    equal, so every value the driver builds is a [Parse.t] unchanged. *)

let parse ~(name : string) (text : string) : t =
  if String.length text > Scan.max_bytes then
    {
      Recover.items = [];
      dropped = [];
      recovered = [];
      diagnostics = [ Scan.too_large Scan.Too_many_bytes ];
      counters = Recover.no_counters ();
    }
  else
    match Parseio.parse_program ~name (Utils.IO.input_string text) with
    | items ->
        {
          Recover.items;
          dropped = [];
          recovered = [];
          diagnostics = [];
          counters = Recover.no_counters ();
        }
    | exception exn ->
        let first = Error.of_exn exn in
        let recovered = Recover.recover ~name ~text in
        {
          recovered with
          Recover.diagnostics =
            Error.to_diagnostic first :: recovered.Recover.diagnostics;
        }

let items (parsed : t) = parsed.Recover.items
let dropped (parsed : t) = parsed.Recover.dropped
let recovered (parsed : t) = parsed.Recover.recovered
let diagnostics (parsed : t) = parsed.Recover.diagnostics
let counters (parsed : t) = parsed.Recover.counters
let failed (parsed : t) = parsed.Recover.diagnostics <> []

(** One entry of the item stream an outline or a symbol list walks: an item
    the parse kept, or the stub of one it dropped, in source order. *)
type item = Kept of Syntax.pitem Location.located | Stub of Recover.dropped

(** [items_with_stubs parsed] is [items] with every [Dropped] stub in the
    place its bytes belong, so a consumer that lists items still shows a
    broken one. The stubs are a separate list only because [Syntax.pitem]
    belongs to the compiler and adding a constructor to it would stop typing
    from consuming the tree unchanged. *)
let items_with_stubs (parsed : t) : item list =
  let start_of_item (item : Syntax.pitem Location.located) =
    item.Location.pl_loc.Location.loc_bchar
  in
  let kept =
    List.map (fun item -> (start_of_item item, Kept item)) parsed.Recover.items
  in
  let stubs =
    List.map
      (fun (Recover.Dropped { span; _ } as stub) ->
        (span.Jide_lang.Span.start_byte, Stub stub))
      parsed.Recover.dropped
  in
  List.map snd
    (List.stable_sort (fun (a, _) (b, _) -> compare a b) (kept @ stubs))
