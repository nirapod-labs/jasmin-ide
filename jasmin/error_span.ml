open Jasmin
let of_location (location : Location.t) : Jide_lang.Span.t =
  let start_byte = max 0 location.loc_bchar in
  let end_byte = max start_byte location.loc_echar in
  Jide_lang.Span.make ~start_byte ~end_byte

let of_error_location (location : Utils.error_loc) : Jide_lang.Span.t =
  match location with
  | Utils.Lnone -> Jide_lang.Span.make ~start_byte:0 ~end_byte:0
  | Utils.Lone location -> of_location location
  | Utils.Lmore location -> of_location location.Location.base_loc

let of_i_location (location : Location.i_loc) : Jide_lang.Span.t =
  of_location location.Location.base_loc
