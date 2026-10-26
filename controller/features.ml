type file = { uri : string; lines : Jide_lang.Line_index.t }
type edit = { file : file; edits : Jide_engine.Types.text_edit list }
type located_symbol = { file : file; symbol : Jide_engine.Types.symbol }

type t = {
  available : bool;
  completion :
    uri:string -> byte:int -> Jide_engine.Types.completion_item list option;
  signature_help :
    uri:string -> byte:int -> Jide_engine.Types.signature_help option;
  prepare_rename :
    uri:string -> byte:int -> (Jide_lang.Span.t, string) result option;
  rename :
    uri:string ->
    byte:int ->
    new_name:string ->
    (edit list, string) result option;
  workspace_symbols : query:string -> located_symbol list option;
  highlights :
    uri:string -> byte:int -> Jide_engine.Types.highlight list option;
  semantic_tokens : uri:string -> Jide_engine.Types.semantic_token list option;
}

let unavailable =
  {
    available = false;
    completion = (fun ~uri:_ ~byte:_ -> None);
    signature_help = (fun ~uri:_ ~byte:_ -> None);
    prepare_rename = (fun ~uri:_ ~byte:_ -> None);
    rename = (fun ~uri:_ ~byte:_ ~new_name:_ -> None);
    workspace_symbols = (fun ~query:_ -> None);
    highlights = (fun ~uri:_ ~byte:_ -> None);
    semantic_tokens = (fun ~uri:_ -> None);
  }

let make ~completion ~signature_help ~prepare_rename ~rename ~workspace_symbols
    ~highlights ~semantic_tokens () =
  {
    available = true;
    completion;
    signature_help;
    prepare_rename;
    rename;
    workspace_symbols;
    highlights;
    semantic_tokens;
  }

let available t = t.available
let completion t ~uri ~byte = t.completion ~uri ~byte
let signature_help t ~uri ~byte = t.signature_help ~uri ~byte
let prepare_rename t ~uri ~byte = t.prepare_rename ~uri ~byte
let rename t ~uri ~byte ~new_name = t.rename ~uri ~byte ~new_name
let workspace_symbols t ~query = t.workspace_symbols ~query
let highlights t ~uri ~byte = t.highlights ~uri ~byte
let semantic_tokens t ~uri = t.semantic_tokens ~uri
