(** Seeded generators for the two fuzzing surfaces.

    A case derives its own stream from the run's seed, the surface and the
    case's ordinal, so case 37 of seed 1 is the same input on every run, on
    any host, however long the run before it lasted. That is what makes a
    failing case replayable: ask for at least thirty-eight cases with the
    same seed. *)

type surface = Text | Edit

let tag = function Text -> 1 | Edit -> 2

type state = Random.State.t

let make ~(seed : int) ~(surface : surface) ~(case : int) : state =
  Random.State.make [| seed; tag surface; case |]

let int (st : state) bound = if bound <= 0 then 0 else Random.State.int st bound
let bool (st : state) = Random.State.bool st

let pick (st : state) (choices : 'a array) =
  choices.(int st (Array.length choices))

let between (st : state) low high =
  if high <= low then low else low + int st (high - low + 1)

let list (st : state) ~count (f : state -> 'a) : 'a list =
  List.init count (fun _ -> f st)

let chars = [| "a"; "b"; "c"; "p"; "q"; "s"; "t"; "x"; "y"; "z" |]

let identifier st =
  pick st chars ^ if bool st then string_of_int (int st 10) else ""

let int_types =
  [| "u8"; "u16"; "u32"; "u64"; "bool"; "int"; "word"; "sptr"; "ptr" |]

