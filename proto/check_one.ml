(* throwaway: can the library parse a file for us at all *)
open Jasmin

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  let path = Sys.argv.(1) in
  let text = read_file path in
  match Parseio.parse_program ~name:path (Utils.IO.input_string text) with
  | items -> Printf.printf "%s: %d items\n" path (List.length items)
  | exception exn ->
      Printf.printf "%s: %s\n" path (Printexc.to_string exn);
      exit 1
