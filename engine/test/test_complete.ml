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

let facts_of items = { Jide_jasmin.Facts.items; uses = []; calls = [] }

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

let test_signature_help () =
  let file = "/work/basic.jazz" in
  let resolved = resolve file basic in
  let index = index_of resolved file in
  (match byte_after basic ~within:"  x = add(counter, 1);" ~name:"add(" with
  | None -> check "signature: the call exists" false
  | Some byte -> (
      match signature_at ~facts:basic_facts (resolved, index) ~byte with
      | None -> check "signature: the call has a signature" false
      | Some signature ->
          check "signature: the callee is the name as written"
            (String.equal signature.callee "add");
          check "signature: the parameters are the checked types"
            (signature.parameters = [ "reg u64"; "reg u64" ]);
          check "signature: the label is the signature"
            (String.equal signature.label "add(reg u64, reg u64) -> reg u64");
          check "signature: the first argument is active" (signature.active = 0)
      ));
  (match
     byte_after basic ~within:"  x = add(counter, 1);" ~name:"add(counter,"
   with
  | None -> check "signature: the second argument exists" false
  | Some byte -> (
      match signature_at ~facts:basic_facts (resolved, index) ~byte with
      | None -> check "signature: the second argument has a signature" false
      | Some signature ->
          check "signature: the argument after the comma is active"
            (signature.active = 1)));
  (match
     byte_after basic ~within:"  x = add(counter, 1);" ~name:"add(counter, 1)"
   with
  | None -> check "signature: the closed call exists" false
  | Some byte ->
      check "signature: a closed call asks for nothing"
        (signature_at ~facts:basic_facts (resolved, index) ~byte = None));
  (* A fact whose name span is not the declaration's is not that
     declaration's signature: the identity is the file and the byte of the
     name, so the answer falls back to the source the index renders. *)
  let shifted_facts =
    facts_of
      [
        {
          (function_fact basic ~within:"fn add(reg u64 a, reg u64 b)"
             ~name:"add" ~fs_in:[ "reg u64"; "reg u64" ] ~fs_out:[ "reg u64" ])
          with
          Jide_jasmin.Facts.i_name_span =
            Jide_lang.Span.make ~start_byte:0 ~end_byte:3;
        };
      ]
  in
  match byte_after basic ~within:"  x = add(counter, 1);" ~name:"add(" with
  | None -> check "signature: the fallback position exists" false
  | Some byte -> (
      match signature_at ~facts:shifted_facts (resolved, index) ~byte with
      | None -> check "signature: the fallback answers" false
      | Some signature ->
          check "signature: a shifted fact is not the declaration's"
            (signature.parameters = [ "reg u64 a"; "reg u64 b" ]);
          check "signature: the fallback label is the written signature"
            (String.equal signature.label "add(reg u64 a, reg u64 b) -> reg u64")
      )

(* ------------------------------------------------------------------ *)
(* A call inside a namespace, and the qualified call after it *)

let namespaced =
  {|namespace A {
  fn mul(reg u64 a, reg u64 b) -> reg u64 {
    reg u64 p;
    p = a * b;
    return p;
  }

  fn twice(reg u64 v) -> reg u64 {
    reg u64 r;
    r = mul(v, 2);
    return r;
  }
}

fn use_it(reg u64 x) -> reg u64 {
  reg u64 y;
  y = A::mul(x, 3);
  return y;
}
|}

let namespaced_facts =
  facts_of
    [
      function_fact namespaced ~within:"fn mul(reg u64 a, reg u64 b)"
        ~name:"mul" ~fs_in:[ "reg u64"; "reg u64" ] ~fs_out:[ "reg u64" ];
      function_fact namespaced ~within:"fn twice(reg u64 v)" ~name:"twice"
        ~fs_in:[ "reg u64" ] ~fs_out:[ "reg u64" ];
    ]

let test_a_namespace () =
  let file = "/work/namespaced.jazz" in
  let resolved = resolve file namespaced in
  let index = index_of resolved file in
  (match
     complete_in (resolved, index) ~text:namespaced ~within:"    return r;"
       ~name:"return"
   with
  | None -> check "namespace: the inner position exists" false
  | Some completion ->
      check "namespace: the namespace's own function is in scope, unqualified"
        (has_label completion "mul");
      check "namespace: its parameter is in scope" (has_label completion "v");
      check "namespace: the other function's local is not in scope"
        (no_label completion "p");
      check "namespace: a function is not in scope before it is written"
        (no_label completion "twice");
      check "namespace: an inner name is not offered qualified"
        (no_label completion "A::mul"));
  let completion =
    complete_at (resolved, index) ~byte:(String.length namespaced)
  in
  check "namespace: the namespace's function is qualified outside it"
    (has_label completion "A::mul");
  check "namespace: the second function is qualified too"
    (has_label completion "A::twice");
  check "namespace: the outer function is in scope"
    (has_label completion "use_it");
  check "namespace: the inner name is not in scope unqualified"
    (no_label completion "mul");
  (* A call inside the namespace, and the qualified call after it. *)
  (match byte_after namespaced ~within:"    r = mul(v, 2);" ~name:"mul(" with
  | None -> check "namespace: the inner call exists" false
  | Some byte -> (
      match signature_at ~facts:namespaced_facts (resolved, index) ~byte with
      | None -> check "namespace: the inner call has a signature" false
      | Some signature ->
          check "namespace: the inner callee is the name written"
            (String.equal signature.callee "mul");
          check "namespace: the inner call takes the checked types"
            (signature.parameters = [ "reg u64"; "reg u64" ])));
  match byte_after namespaced ~within:"  y = A::mul(x, 3);" ~name:"A::mul(" with
  | None -> check "namespace: the qualified call exists" false
  | Some byte -> (
      match signature_at ~facts:namespaced_facts (resolved, index) ~byte with
      | None -> check "namespace: the qualified call has a signature" false
      | Some signature ->
          check "namespace: the qualified callee is the whole name"
            (String.equal signature.callee "A::mul");
          check "namespace: the qualified call takes the checked types"
            (signature.parameters = [ "reg u64"; "reg u64" ]);
          check "namespace: its first argument is active" (signature.active = 0)
      )

(* ------------------------------------------------------------------ *)
(* A required file, and a call the parser dropped at the end of the file *)

let library = {|fn other(reg u64 a) -> reg u64 {
  return a;
}
|}

let entry =
  {|require "lib.jazz"

fn helper(reg u64 a, reg u64 b) -> reg u64 {
  reg u64 c;
  c = other(a);
  return c;
}

fn run(reg u64 seed) -> reg u64 {
  reg u64 r;
  r = helper(seed, 1);
  return r;
}
|}

(* The text a client holds while the last argument is still being typed. The
   parser drops the whole file at an unfinished call, so the names come from
   the snapshot of the text before it, which [entry] is. *)
let partial_entry =
  match
    byte_after entry ~within:"  r = helper(seed, 1);" ~name:"helper(seed, "
  with
  | Some byte -> String.sub entry 0 byte
  | None -> entry

let entry_facts =
  facts_of
    [
      function_fact entry ~within:"fn helper(reg u64 a, reg u64 b)"
        ~name:"helper" ~fs_in:[ "reg u64"; "reg u64" ] ~fs_out:[ "reg u64" ];
    ]

let test_a_required_file_and_a_partial_call () =
  let file = "/work/entry.jazz" in
  let resolved = resolve ~files:[ ("lib.jazz", library) ] file entry in
  let index = index_of resolved file in
  (match
     complete_in (resolved, index) ~text:entry ~within:"  c = other(a);"
       ~name:"other"
   with
  | None -> check "require: the position exists" false
  | Some completion ->
      check "require: the required function is in scope"
        (has_label completion "other");
      check "require: it is a function"
        (kind_of completion "other" = Some Jide_engine.Complete.Function);
      check "require: its detail is its own source"
        (match detail_of completion "other" with
        | Some detail -> String.equal detail "fn other(reg u64 a) -> reg u64"
        | None -> false);
      check "require: the file's own function is not in scope in its own body"
        (no_label completion "helper");
      check "require: the parameters are in scope" (has_label completion "a");
      check "require: the later function is not in scope"
        (no_label completion "run"));
  (match
     complete_in (resolved, index) ~text:entry ~within:"  r = helper(seed, 1);"
       ~name:"r"
   with
  | None -> check "require: the second function's body has a position" false
  | Some completion ->
      check "require: the file's own function is in scope once declared"
        (has_label completion "helper"));
  let partial_index = Jide_engine.Index.of_text ~file partial_entry |> fst in
  (match
     byte_of_in partial_entry ~within:"r = helper(seed, " ~name:"helper"
   with
  | None -> check "partial: the call exists" false
  | Some byte ->
      check "partial: the parser dropped the unfinished call"
        (Jide_engine.Index.find_occurrence partial_index ~byte = None));
  (match byte_after entry ~within:"  c = other(a);" ~name:"other(" with
  | None -> check "require: the required call exists" false
  | Some byte -> (
      match signature_at ~facts:entry_facts (resolved, index) ~byte with
      | None -> check "require: the required call has a signature" false
      | Some signature ->
          check "require: the parameters come from the required file's index"
            (signature.parameters = [ "reg u64 a" ]);
          check "require: the label is that file's signature"
            (String.equal signature.label "other(reg u64 a) -> reg u64")));
  match
    Jide_engine.Complete.signature_help ~facts:entry_facts ~index:partial_index
      ~resolve:resolved
      ~byte:(String.length partial_entry)
      ()
  with
  | None ->
      check "partial: the call at the end of the file has a signature" false
  | Some signature ->
      check "partial: the callee is read from the text"
        (String.equal signature.callee "helper");
      check "partial: the parameters are the checked types"
        (signature.parameters = [ "reg u64"; "reg u64" ]);
      check "partial: the label is the signature"
        (String.equal signature.label "helper(reg u64, reg u64) -> reg u64");
      check "partial: the argument after the comma is active"
        (signature.active = 1)

let () =
  test_completion_in_a_function_body ();
  test_block_scope ();
  test_completion_at_the_top_level ();
  test_the_prefix_filters_the_list ();
  test_annotation_keys ();
  test_the_fixed_tables ();
  test_signature_help ();
  test_a_namespace ();
  test_a_required_file_and_a_partial_call ();
  if !failures = 0 then print_endline "all completion tests passed"
  else (
    Printf.printf "%d completion test(s) failed\n" !failures;
    exit 1)
