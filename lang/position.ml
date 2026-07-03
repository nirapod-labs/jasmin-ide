type t = { line : int; character : int }

let make ~line ~character = { line; character }

let compare a b =
  let by_line = Int.compare a.line b.line in
  if by_line <> 0 then by_line else Int.compare a.character b.character
