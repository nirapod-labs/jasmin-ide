type t = { text : string; starts : int array }

let line_starts text =
  let length = String.length text in
  let count = ref 1 in
  for i = 0 to length - 1 do
    if String.get text i = '\n' then incr count
  done;
  let starts = Array.make !count 0 in
  let next = ref 1 in
  for i = 0 to length - 1 do
    if String.get text i = '\n' then begin
      starts.(!next) <- i + 1;
      incr next
    end
  done;
  starts

let create text = { text; starts = line_starts text }

let line_count index = Array.length index.starts

let clamp_line index line =
  let last = Array.length index.starts - 1 in
  if line < 0 then 0 else if line > last then last else line

let line_start index line = index.starts.(clamp_line index line)

let line_end index line =
  let line = clamp_line index line in
  let length = String.length index.text in
  if line + 1 >= Array.length index.starts then length
  else
    let newline = index.starts.(line + 1) - 1 in
    if newline > 0 && String.get index.text (newline - 1) = '\r' then
      newline - 1
    else newline

(* Width, in bytes and in UTF-16 code units, of the character at [pos]. A
   byte that does not begin a well-formed sequence counts as one byte and one
   code unit, so the walk always moves forward and never fails. *)
let char_width text pos =
  let length = String.length text in
  let is_continuation code = (code land 0xC0) = 0x80 in
  if pos >= length then (1, 1)
  else
    let b0 = Char.code (String.get text pos) in
    if b0 < 0x80 then (1, 1)
    else if b0 < 0xC2 then (1, 1)
    else if b0 < 0xE0 then
      if pos + 1 >= length then (1, 1)
      else
        let b1 = Char.code (String.get text (pos + 1)) in
        if is_continuation b1 then (2, 1) else (1, 1)
    else if b0 < 0xF0 then
      if pos + 2 >= length then (1, 1)
      else
        let b1 = Char.code (String.get text (pos + 1)) in
        let b2 = Char.code (String.get text (pos + 2)) in
        if not (is_continuation b1) || not (is_continuation b2) then (1, 1)
        else
          let code_point =
            ((b0 land 0x0F) lsl 12)
            lor ((b1 land 0x3F) lsl 6)
            lor (b2 land 0x3F)
          in
          if code_point < 0x800 then (1, 1)
          else if code_point >= 0xD800 && code_point <= 0xDFFF then (1, 1)
          else (3, 1)
    else if b0 < 0xF5 then
      if pos + 3 >= length then (1, 1)
      else
        let b1 = Char.code (String.get text (pos + 1)) in
        let b2 = Char.code (String.get text (pos + 2)) in
        let b3 = Char.code (String.get text (pos + 3)) in
        if
          not (is_continuation b1)
          || not (is_continuation b2)
          || not (is_continuation b3)
        then (1, 1)
        else
          let code_point =
            ((b0 land 0x07) lsl 18)
            lor ((b1 land 0x3F) lsl 12)
            lor ((b2 land 0x3F) lsl 6)
            lor (b3 land 0x3F)
          in
          if code_point < 0x10000 then (1, 1)
          else if code_point > 0x10FFFF then (1, 1)
          else (4, 2)
    else (1, 1)

(* UTF-16 code units of the characters that lie wholly inside [start, stop). *)
let units_between text start stop =
  let rec go pos total =
    if pos >= stop then total
    else
      let bytes, units = char_width text pos in
      if pos + bytes > stop then total else go (pos + bytes) (total + units)
  in
  go start 0

let line_of_byte index byte =
  let low = ref 0 in
  let high = ref (Array.length index.starts - 1) in
  while !low < !high do
    let middle = (!low + !high + 1) / 2 in
    if index.starts.(middle) <= byte then low := middle
    else high := middle - 1
  done;
  !low

let position_of_byte index byte =
  let length = String.length index.text in
  let byte = max 0 (min byte length) in
  let line = line_of_byte index byte in
  let start = index.starts.(line) in
  let stop = min byte (line_end index line) in
  Position.make ~line ~character:(units_between index.text start stop)

let byte_of_position index (position : Position.t) =
  let line = clamp_line index position.line in
  let start = index.starts.(line) in
  let stop = line_end index line in
  let rec walk pos total =
    if pos >= stop then stop
    else
      let bytes, units = char_width index.text pos in
      let next = total + units in
      if next > position.character then pos
      else if next = position.character then min (pos + bytes) stop
      else walk (pos + bytes) next
  in
  if position.character <= 0 then start else walk start 0

let utf16_length index (span : Span.t) =
  let length = String.length index.text in
  let start = max 0 (min span.start_byte length) in
  let stop = max 0 (min span.end_byte length) in
  if stop <= start then 0 else units_between index.text start stop
