(* The debounce rules on their own: nothing here reads a clock, so every case
   is exact rather than timed. *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let open_ = "file:///a.jazz"
let other = "file:///b.jazz"
let third = "file:///c.jazz"

let test_new () =
  let t = Jide_controller.Debounce.create () in
  check "a new debounce holds nothing"
    (not (Jide_controller.Debounce.stale t ~uri:open_));
  check "a new debounce has nothing due"
    (Jide_controller.Debounce.due t ~now:1_000_000.0 = [])

let test_mark_and_due () =
  let t = Jide_controller.Debounce.create () in
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:10.0;
  check "marked: stale" (Jide_controller.Debounce.stale t ~uri:open_);
  check "marked: not due before the deadline"
    (Jide_controller.Debounce.due t ~now:9.999 = []);
  check "marked: due at the deadline"
    (Jide_controller.Debounce.due t ~now:10.0 = [ open_ ]);
  check "due: the document stops waiting"
    (not (Jide_controller.Debounce.stale t ~uri:open_));
  check "due: not twice" (Jide_controller.Debounce.due t ~now:11.0 = [])

let test_order () =
  let t = Jide_controller.Debounce.create () in
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:3.0;
  Jide_controller.Debounce.mark t ~uri:other ~deadline:1.0;
  Jide_controller.Debounce.mark t ~uri:third ~deadline:2.0;
  check "due: the earliest deadline first, and only those that passed"
    (Jide_controller.Debounce.due t ~now:2.5 = [ other; third ]);
  check "due: the last one afterwards"
    (Jide_controller.Debounce.due t ~now:3.0 = [ open_ ])

let test_ties () =
  let t = Jide_controller.Debounce.create () in
  Jide_controller.Debounce.mark t ~uri:other ~deadline:5.0;
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:5.0;
  check "due: the same deadline is ordered by uri"
    (Jide_controller.Debounce.due t ~now:5.0 = [ open_; other ])

let test_restart () =
  let t = Jide_controller.Debounce.create () in
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:1.0;
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:5.0;
  check "a second mark restarts the wait"
    (Jide_controller.Debounce.due t ~now:2.0 = []);
  check "the restarted deadline is the one that counts"
    (Jide_controller.Debounce.due t ~now:5.0 = [ open_ ])

let test_clear () =
  let t = Jide_controller.Debounce.create () in
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:1.0;
  Jide_controller.Debounce.clear t ~uri:open_;
  check "cleared: not stale" (not (Jide_controller.Debounce.stale t ~uri:open_));
  check "cleared: nothing due" (Jide_controller.Debounce.due t ~now:2.0 = []);
  Jide_controller.Debounce.clear t ~uri:open_;
  check "clearing twice is harmless"
    (not (Jide_controller.Debounce.stale t ~uri:open_))

let test_other_documents_are_kept () =
  let t = Jide_controller.Debounce.create () in
  Jide_controller.Debounce.mark t ~uri:open_ ~deadline:1.0;
  Jide_controller.Debounce.mark t ~uri:other ~deadline:9.0;
  ignore (Jide_controller.Debounce.due t ~now:2.0);
  check "only the due document was taken"
    (Jide_controller.Debounce.stale t ~uri:other
    && not (Jide_controller.Debounce.stale t ~uri:open_))

let () =
  test_new ();
  test_mark_and_due ();
  test_order ();
  test_ties ();
  test_restart ();
  test_clear ();
  test_other_documents_are_kept ();
  if !failures = 0 then print_endline "all debounce tests passed"
  else (
    Printf.printf "%d debounce test(s) failed\n" !failures;
    exit 1)
