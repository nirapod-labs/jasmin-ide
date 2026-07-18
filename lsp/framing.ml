type channels = { input : in_channel; output : out_channel }
type packet = Jsonrpc.Packet.t
type read_result = (packet, string) result option

module Wire =
  Lsp.Io.Make
    (struct
      type 'a t = 'a

      let return x = x
      let raise = raise

      module O = struct
        let ( let+ ) x f = f x
        let ( let* ) x f = f x
      end
    end)
    (struct
      type input = in_channel
      type output = out_channel

      let read_line ic = try Some (input_line ic) with End_of_file -> None

      let read_exactly ic n =
        try Some (really_input_string ic n) with End_of_file -> None

      let write oc chunks =
        List.iter (output_string oc) chunks;
        flush oc
    end)

let read { input; _ } =
  match Wire.read input with
  | None -> None
  | Some packet -> Some (Ok packet)
  | exception Lsp.Io.Error msg -> Some (Error msg)
  | exception Jsonrpc.Json.Of_json (what, _) ->
      Some (Error ("invalid packet: " ^ what))
  | exception Yojson.Json_error msg -> Some (Error ("invalid json: " ^ msg))
  | exception End_of_file -> None
  | exception Sys_error msg -> Some (Error ("read error: " ^ msg))

let write { output; _ } packet = Wire.write output packet
