let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n" name)

let create () = Jide_engine.Engine.create ()

let uri_of name = "file:///tmp/" ^ name ^ ".jazz"

(* The store's own contract, without the controller in the way: only a
   document the client opened may be changed, and a closed document must
   never come back from a change. *)

let test_change_without_open_is_ignored () =
  let engine = create () in
  let uri = uri_of "never-opened" in
  Jide_engine.Engine.change_document engine ~uri ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  check "change without open: no document created"
    (Jide_engine.Engine.document engine ~uri = None);
  check "change without open: the store stays empty"
    (Jide_engine.Engine.documents engine = [])

let test_change_after_close_is_ignored () =
  let engine = create () in
  let uri = uri_of "closed" in
  Jide_engine.Engine.open_document engine ~uri ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  Jide_engine.Engine.close_document engine ~uri;
  Jide_engine.Engine.change_document engine ~uri ~version:2
    ~text:"fn g() -> reg u32 { return 1; }";
  check "change after close: no document created"
    (Jide_engine.Engine.document engine ~uri = None);
  check "change after close: the store stays empty"
    (Jide_engine.Engine.documents engine = [])

let test_open_then_change_updates_the_text () =
  let engine = create () in
  let uri = uri_of "open" in
  Jide_engine.Engine.open_document engine ~uri ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  Jide_engine.Engine.change_document engine ~uri ~version:3
    ~text:"fn g() -> reg u32 { return 1; }";
  match Jide_engine.Engine.document engine ~uri with
  | Some document ->
    check "open then change: text" (Jide_engine.Document.text document = "fn g() -> reg u32 { return 1; }");
    check "open then change: version" (Jide_engine.Document.version document = 3)
  | None -> check "open then change: document kept" false

let test_close_removes_the_document () =
  let engine = create () in
  let uri = uri_of "removed" in
  Jide_engine.Engine.open_document engine ~uri ~version:1 ~text:"fn f()";
  Jide_engine.Engine.close_document engine ~uri;
  check "close: document removed" (Jide_engine.Engine.document engine ~uri = None);
  check "close: the store is empty" (Jide_engine.Engine.documents engine = [])

let () =
  test_change_without_open_is_ignored ();
  test_change_after_close_is_ignored ();
  test_open_then_change_updates_the_text ();
  test_close_removes_the_document ();
  if !failures = 0 then print_endline "all engine tests passed"
  else (
    Printf.printf "%d engine test(s) failed\n" !failures;
    exit 1)
