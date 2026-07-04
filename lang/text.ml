let strip_bom text =
  let length = String.length text in
  if
    length >= 3
    && Char.code (String.get text 0) = 0xEF
    && Char.code (String.get text 1) = 0xBB
    && Char.code (String.get text 2) = 0xBF
  then String.sub text 3 (length - 3)
  else text

let clamp length value =
  if value < 0 then 0 else if value > length then length else value

let utf8_byte_length text start stop =
  let length = String.length text in
  let start = clamp length start in
  let stop = clamp length stop in
  if stop <= start then 0 else stop - start

(* Number of bytes of the well-formed sequence that starts at [pos], or zero
   when the bytes there are not one. *)
let sequence_width text pos =
  let length = String.length text in
  let is_continuation code = (code land 0xC0) = 0x80 in
  let byte index = Char.code (String.get text index) in
  if pos >= length then 0
  else
    let b0 = byte pos in
    if b0 < 0x80 then 1
    else if b0 < 0xC2 then 0
    else if b0 < 0xE0 then
      if pos + 1 < length && is_continuation (byte (pos + 1)) then 2 else 0
    else if b0 < 0xF0 then
      if pos + 2 >= length then 0
      else
        let b1 = byte (pos + 1) in
        let b2 = byte (pos + 2) in
        if not (is_continuation b1) || not (is_continuation b2) then 0
        else
          let code_point =
            ((b0 land 0x0F) lsl 12)
            lor ((b1 land 0x3F) lsl 6)
            lor (b2 land 0x3F)
          in
          if code_point < 0x800 then 0
          else if code_point >= 0xD800 && code_point <= 0xDFFF then 0
          else 3
    else if b0 < 0xF5 then
      if pos + 3 >= length then 0
      else
        let b1 = byte (pos + 1) in
        let b2 = byte (pos + 2) in
        let b3 = byte (pos + 3) in
        if
          not (is_continuation b1)
          || not (is_continuation b2)
          || not (is_continuation b3)
        then 0
        else
          let code_point =
            ((b0 land 0x07) lsl 18)
            lor ((b1 land 0x3F) lsl 12)
            lor ((b2 land 0x3F) lsl 6)
            lor (b3 land 0x3F)
          in
          if code_point < 0x10000 then 0
          else if code_point > 0x10FFFF then 0
          else 4
    else 0

let is_utf8 text =
  let length = String.length text in
  let rec go pos =
    if pos >= length then true
    else
      let width = sequence_width text pos in
      if width = 0 then false else go (pos + width)
  in
  go 0
