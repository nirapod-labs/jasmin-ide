(* The stub, and the shape of what replaces it.

   Each function answers "unavailable": the empty answer, which the
   controller encodes as [null] or [], never as an error. That is a real
   answer — a position the engine cannot describe has no definition, no
   references, no hover and no symbols — so the routing and its tests do not
   change when the engine can answer.

   What the adapter will do, in terms of what already exists:

   - bind the position to a byte of the open document
     ([Document.byte_of_position]: it clamps a line past the last one and a
     character past the end of a line, so a position outside the text is an
     answer about the end of it, never an error);
   - ask [Resolve.answer_at] what the occurrence holding that byte names, and
     take [Index.declaration] out of a [Resolved] answer;
   - turn a declaration's [name_span], or an occurrence's [span], into a
     {!range} with the line index of the text the span belongs to
     ([Index.text]), which is why this conversion is here and not in the
     controller;
   - take the outline from [Index.outline], whose items already nest.

   None of that is written yet: the file is the seam between the controller
   and the engine, and it is not the only one that moves. [Engine.analysis]
   carries the text, its version, its diagnostics and its context, and no
   [Index.t] and no [Resolve.t], so these four have nothing to answer from
   today even when the document is open. The index an adapter needs is not
   built at all yet, and the seam that has to widen is [Engine]: an analysis
   that keeps what was built from it (the resolver's per-file index, and the
   text that index describes), so that an answer can be read back without
   re-resolving the file. The four functions below keep the signature they
   have; what changes is what the engine holds for them to read. The
   controller and the routing tests do not change.
*)

type range = { start_ : Jide_lang.Position.t; end_ : Jide_lang.Position.t }
type location = { uri : string; range : range }
type hover = { range : range option; contents : Types.markdown }

type symbol = {
  name : string;
  detail : string option;
  kind : Types.symbol_kind;
  range : range;
  selection_range : range;
  children : symbol list;
}

let definition _engine ~uri:_ ~position:_ = []
let references _engine ~uri:_ ~position:_ ~include_declaration:_ = []
let hover _engine ~uri:_ ~position:_ = None
let symbols _engine ~uri:_ = []
