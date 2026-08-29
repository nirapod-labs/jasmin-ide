let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

(* The tests say where a byte is by quoting the source around it, never by a
   number written by hand: [byte_of_in text ~within ~name] is where [name]
   starts inside the first place [within] sits in the text. *)
let byte_of text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then None
    else if String.equal (String.sub text index fragment_length) fragment then
      Some index
    else from (index + 1)
  in
  if fragment_length = 0 then Some 0 else from 0

let byte_of_in text ~within ~name =
  match byte_of text within with
  | None -> None
  | Some start -> (
      let window = String.sub text start (String.length within) in
      match byte_of window name with
      | None -> None
      | Some offset -> Some (start + offset))

let byte_after text ~within ~name =
  match byte_of_in text ~within ~name with
  | None -> None
  | Some start -> Some (start + String.length name)

let resolve ?(idirs = []) ?(files = []) file text =
  let read path = List.assoc_opt (Filename.basename path) files in
  Jide_engine.Resolve.of_text ~idirs ~read ~file text

let index_of (resolved : Jide_engine.Resolve.t) path =
  match Jide_engine.Resolve.file resolved path with
  | Some index -> index
  | None -> Jide_engine.Index.of_text ~file:path "" |> fst

let span_of text ~within ~name =
  match byte_of_in text ~within ~name with
  | None -> Jide_lang.Span.make ~start_byte:0 ~end_byte:0
  | Some start ->
      Jide_lang.Span.make ~start_byte:start
        ~end_byte:(start + String.length name)

(* A function fact as the checker leaves it: the name span is the identity,
   the file and the byte where the name starts, and the signature is what the
   compiler typed. *)
let function_fact text ~within ~name ~fs_in ~fs_out =
  let name_span = span_of text ~within ~name in
  {
    Jide_jasmin.Facts.i_kind = Jide_jasmin.Facts.Function;
    i_span = name_span;
    i_name = name;
    i_name_span = name_span;
    i_annot = [];
    i_sig = Some { Jide_jasmin.Facts.fs_in; fs_out };
    i_vars = [];
  }

let facts_of items = { Jide_jasmin.Facts.items }

let complete_at ?trigger (resolved, index) ~byte =
  Jide_engine.Complete.completion ?trigger ~index ~resolve:resolved ~byte ()

let complete_in (resolved, index) ~text ~within ~name =
  match byte_of_in text ~within ~name with
  | None -> None
  | Some byte -> Some (complete_at (resolved, index) ~byte)

let labels (completion : Jide_engine.Complete.completion) =
  List.map
    (fun (item : Jide_engine.Complete.item) -> item.label)
    completion.items

let find item_label (completion : Jide_engine.Complete.completion) =
  List.find_opt
    (fun (item : Jide_engine.Complete.item) ->
      String.equal item.label item_label)
    completion.items

let has_label completion label = find label completion <> None
let no_label completion label = not (has_label completion label)

let kind_of completion label =
  Option.map
    (fun (item : Jide_engine.Complete.item) -> item.kind)
    (find label completion)

let detail_of completion label =
  Option.bind (find label completion) (fun (item : Jide_engine.Complete.item) ->
      item.detail)

let signature_at ?facts (resolved, index) ~byte =
  Jide_engine.Complete.signature_help ?facts ~index ~resolve:resolved ~byte ()

(* ------------------------------------------------------------------ *)
(* One small program: a global, an alias, three functions and a block *)

let basic =
  {|u64 counter = 0;
type word = u64;

#[stacksize=64]
fn add(reg u64 a, reg u64 b) -> reg u64 {
  reg u64 c;
  c = a + b;
  return c;
}

fn branch(reg u64 n) -> reg u64 {
  reg u64 out;
  out = 0;
  if n {
    reg u64 inner;
    inner = 1;
    out = inner;
  }
  return out;
}

fn main() -> reg u64 {
  reg u64 x;
  x = add(counter, 1);
  return x;
}
|}

let basic_facts =
  facts_of
    [
      function_fact basic ~within:"fn add(reg u64 a, reg u64 b)" ~name:"add"
        ~fs_in:[ "reg u64"; "reg u64" ] ~fs_out:[ "reg u64" ];
      function_fact basic ~within:"fn main() -> reg u64" ~name:"main" ~fs_in:[]
        ~fs_out:[ "reg u64" ];
    ]

let test_completion_in_a_function_body () =
  let file = "/work/basic.jazz" in
  let resolved = resolve file basic in
  let index = index_of resolved file in
  match
    complete_in (resolved, index) ~text:basic ~within:"  x = add(counter, 1);"
      ~name:"x"
  with
  | None -> check "basic: the body has a position" false
  | Some completion ->
      check "basic: a local is in scope" (has_label completion "x");
      check "basic: a local is a variable"
        (kind_of completion "x" = Some Jide_engine.Complete.Variable);
      check "basic: a function is in scope" (has_label completion "add");
      check "basic: a function is a function"
        (kind_of completion "add" = Some Jide_engine.Complete.Function);
      check "basic: a function carries its written signature"
        (match detail_of completion "add" with
        | Some detail ->
            String.equal detail "fn add(reg u64 a, reg u64 b) -> reg u64"
        | None -> false);
      check "basic: a global is in scope" (has_label completion "counter");
      check "basic: a type alias is in scope" (has_label completion "word");
      check "basic: a keyword is in scope" (has_label completion "fn");
      check "basic: a keyword has the keyword kind"
        (kind_of completion "fn" = Some Jide_engine.Complete.Keyword);
      check "basic: another function's local is not in scope"
        (no_label completion "c");
      check "basic: another function's parameter is not in scope"
        (no_label completion "a");
      check "basic: a function is not in scope before it is written"
        (no_label completion "main")

let test_block_scope () =
  let file = "/work/basic.jazz" in
  let resolved = resolve file basic in
  let index = index_of resolved file in
  match
    complete_in (resolved, index) ~text:basic ~within:"  return out;"
      ~name:"out"
  with
  | None -> check "block: the position exists" false
  | Some completion ->
      check "block: the variable before the block is in scope"
        (has_label completion "out");
      check "block: the parameter is in scope" (has_label completion "n");
      check "block: a declaration inside the block ends with it"
        (no_label completion "inner")

let test_completion_at_the_top_level () =
  let file = "/work/basic.jazz" in
  let resolved = resolve file basic in
  let index = index_of resolved file in
  let completion = complete_at (resolved, index) ~byte:(String.length basic) in
  check "top level: the last function is in scope" (has_label completion "main");
  check "top level: the first function is in scope" (has_label completion "add");
  check "top level: the global is in scope" (has_label completion "counter");
  check "top level: the alias is in scope" (has_label completion "word");
  check "top level: a local of a body is not in scope" (no_label completion "x");
  check "top level: an empty prefix offers every keyword"
    (List.length completion.items > 40);
  check "top level: the prefix is empty" (String.equal completion.prefix "")

let test_the_prefix_filters_the_list () =
  let file = "/work/basic.jazz" in
  let resolved = resolve file basic in
  let index = index_of resolved file in
  match byte_after basic ~within:"  x = add(counter, 1);" ~name:"add" with
  | None -> check "prefix: the position exists" false
  | Some byte ->
      let completion = complete_at (resolved, index) ~byte in
      check "prefix: the word being typed is read"
        (String.equal completion.prefix "add");
      check "prefix: the span is that word"
        (Jide_lang.Span.compare completion.span
           (Jide_lang.Span.make ~start_byte:(byte - 3) ~end_byte:byte)
        = 0);
      check "prefix: every item matches it"
        (List.for_all
           (fun (item : Jide_engine.Complete.item) ->
             String.starts_with ~prefix:"add" item.label)
           completion.items);
      check "prefix: the function is offered" (has_label completion "add");
      check "prefix: a name that does not match is dropped"
        (no_label completion "counter");
      check "prefix: a keyword that does not match is dropped"
        (no_label completion "fn")

let test_annotation_keys () =
  let file = "/work/basic.jazz" in
  let resolved = resolve file basic in
  let index = index_of resolved file in
  match byte_after basic ~within:"#[stacksize=64]" ~name:"#[stacksi" with
  | None -> check "annotation: the position exists" false
  | Some byte ->
      let completion = complete_at (resolved, index) ~byte in
      check "annotation: only the matching key is offered"
        (labels completion = [ "stacksize" ]);
      check "annotation: a key has the annotation kind"
        (kind_of completion "stacksize"
        = Some Jide_engine.Complete.Annotation_key);
      check "annotation: no keyword is offered" (no_label completion "fn");
      check "colon: the design's second trigger asks for the keys"
        (labels
           (complete_at ~trigger:Jide_engine.Complete.Colon (resolved, index)
              ~byte:(String.length basic))
        = Jide_engine.Complete.annotation_keys);
      check "colon: no keyword comes back"
        (no_label
           (complete_at ~trigger:Jide_engine.Complete.Colon (resolved, index)
              ~byte:(String.length basic))
           "fn")

let test_the_fixed_tables () =
  let keywords = Jide_engine.Complete.keywords in
  check "keywords: the lexer's 49 words less the wildcard"
    (List.length keywords = 48);
  check "keywords: fn is one" (List.mem "fn" keywords);
  check "keywords: namespace is one" (List.mem "namespace" keywords);
  check "keywords: require is one" (List.mem "require" keywords);
  check "keywords: u64 is one" (List.mem "u64" keywords);
  check "keywords: the wildcard is not a candidate"
    (not (List.mem "_" keywords));
  let keys = Jide_engine.Complete.annotation_keys in
  check "annotation keys: 19 of them" (List.length keys = 19);
  check "annotation keys: stacksize is one" (List.mem "stacksize" keys);
  check "annotation keys: calldepth is one" (List.mem "calldepth" keys);
  check "annotation keys: ct is one" (List.mem "ct" keys);
  check "annotation keys: sct is one" (List.mem "sct" keys);
  check "annotation keys: they are sorted" (keys = List.sort String.compare keys)

