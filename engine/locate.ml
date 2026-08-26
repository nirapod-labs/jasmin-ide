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

   None of that is written yet: the file is the seam, so the lane that fills
   it in touches nothing outside [engine/locate.ml]. *)

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
