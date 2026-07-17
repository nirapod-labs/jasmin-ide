let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then false
    else if String.equal (String.sub text index fragment_length) fragment then
      true
    else from (index + 1)
  in
  fragment_length = 0 || from 0

let temp_dir () =
  let path = Filename.temp_file "jide-mutation" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

let rec remove_tree path =
  if not (Sys.file_exists path) then ()
  else if Sys.is_directory path then (
    Array.iter
      (fun entry -> remove_tree (Filename.concat path entry))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let write path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

let program =
  String.concat "\n"
    [
      "param int N = 3;";
      "type word = u64;";
      "export";
      "fn f(reg u64 x) -> reg u64 {";
      "  reg u64 y;";
      "  global u64 g = 42;";
      "  g = 42;";
      "  y = x;";
      "  y += g;";
      "  return y;";
      "}";
      "fn h(reg u64 x) -> reg u64 {";
      "  return x;";
      "}";
      "";
    ]

let tokens text =
  match Jide_mutation.lex text with Some tokens -> tokens | None -> []

let names items =
  List.map
    (fun (item : Jide_mutation.item) -> item.kind ^ " " ^ item.name)
    items

let () =
  (* The lexer wrapper: token boundaries are the compiler's own, and the
     tokens stop at the end of the text rather than at the EOF token. *)
  let text = "fn f() { }" in
  let lexed = tokens text in
  check "lexing: six tokens" (List.length lexed = 6);
  (match lexed with
  | first :: _ ->
      check "lexing: the first token starts at 0" (first.Jide_mutation.start = 0);
      check "lexing: fn ends at 2" (first.Jide_mutation.stop = 2)
  | [] -> check "lexing: a token" false);
  check "lexing: no tokens in an empty text" (tokens "" = []);
  check "lexing: the lexer refuses an unterminated comment"
    (Jide_mutation.lex "fn f() { /* " = None);

  (* The outline reads the headers the grammar puts in the token sequence. *)
  let outlined = Jide_mutation.outline_of_tokens (tokens program) in
  check "outline: five items" (List.length outlined = 5);
  check "outline: the items and their names"
    (names outlined = [ "param N"; "type word"; "fn f"; "global g"; "fn h" ]);
  check "outline: the first item starts at 0"
    (match outlined with
    | first :: _ -> first.Jide_mutation.start = 0
    | [] -> false);
  check "outline: the header offsets grow"
    (let rec increasing = function
       | (a : Jide_mutation.item) :: (b :: _ as rest) ->
           a.start < b.start && increasing rest
       | _ -> true
     in
     increasing outlined);

  (* Two runs with one seed are one run. *)
  let generate seed =
    Jide_mutation.mutations ~seed ~text:program ~tokens:(tokens program)
  in
  let first = generate 7 and second = generate 7 in
  check "seed: the same seed writes the same mutations"
    (List.map (fun (m : Jide_mutation.mutation) -> m.text) first
    = List.map (fun (m : Jide_mutation.mutation) -> m.text) second);
  check "seed: different seeds write different mutations"
    (List.map (fun (m : Jide_mutation.mutation) -> m.text) (generate 8)
    <> List.map (fun (m : Jide_mutation.mutation) -> m.text) first);
  check "mutations: the five kinds are present"
    (List.sort_uniq compare
       (List.map (fun (m : Jide_mutation.mutation) -> m.label) first)
    = [ "delete"; "duplicate"; "insert"; "swap"; "truncate" ]);
  check "mutations: a deletion removes exactly one token"
    (List.exists
       (fun (m : Jide_mutation.mutation) ->
         m.label = "delete"
         && String.length m.text = String.length program - (m.stop - m.start))
       first);
  check "mutations: a duplication repeats a token of the file"
    (List.exists
       (fun (m : Jide_mutation.mutation) ->
         m.label = "duplicate"
         &&
         let added = String.length m.text - String.length program - 1 in
         added > 0
         && Jide_mutation.contains
              ~fragment:(String.sub m.text (m.start + 1) added)
              program)
       first);
  check "mutations: no insertion brings an item keyword"
    (not
       (List.exists
          (fun keyword ->
            Array.exists (fun entry -> entry = keyword) Jide_mutation.insertions)
          [ "fn"; "proc"; "param"; "global"; "type"; "require"; "export" ]));

  (* The rule itself: the entry the edit sits in may change or go, and every
     entry away from it must survive, in order, with its kind and its name. *)
  let item kind name start = { Jide_mutation.kind; name; start } in
  let entries =
    [
      item "param" "N" 0;
      item "type" "word" 10;
      item "fn" "f" 20;
      item "global" "g" 40;
      item "fn" "h" 60;
    ]
  in
  let last = Some (List.nth entries 4) in
  let without index =
    List.filteri (fun position _ -> position <> index) entries
  in
  check "outline away: the edited entry may go"
    (Jide_mutation.outline_away ~items:entries
       ~mutated:(Some (without 4))
       ~touched:last ~start:60 ~cut:false
    = `Same);
  check "outline away: the edited entry may stay"
    (Jide_mutation.outline_away ~items:entries ~mutated:(Some entries)
       ~touched:(Some (List.nth entries 2))
       ~start:20 ~cut:false
    = `Same);
  check "outline away: a lost entry is a difference"
    (Jide_mutation.outline_away ~items:entries
       ~mutated:(Some (without 1))
       ~touched:last ~start:60 ~cut:false
    = `Different);
  check "outline away: a renamed entry is a difference"
    (Jide_mutation.outline_away ~items:entries
       ~mutated:
         (Some
            [
              item "param" "N" 0;
              item "type" "other" 10;
              item "fn" "f" 20;
              item "global" "g" 40;
            ])
       ~touched:last ~start:60 ~cut:false
    = `Different);
  check "outline away: a reordered entry is a difference"
    (Jide_mutation.outline_away ~items:entries
       ~mutated:
         (Some
            [
              item "type" "word" 10;
              item "param" "N" 0;
              item "fn" "f" 20;
              item "global" "g" 40;
            ])
       ~touched:last ~start:60 ~cut:false
    = `Different);
  check "outline away: two entries with one name are two entries"
    (let twin = [ item "fn" "f" 0; item "fn" "f" 20 ] in
     Jide_mutation.outline_away ~items:twin
       ~mutated:(Some [ item "fn" "f" 20 ])
       ~touched:(Some (List.nth twin 1))
       ~start:20 ~cut:false
     = `Same);
  check "outline away: an unreadable outline is not a difference"
    (Jide_mutation.outline_away ~items:entries ~mutated:None ~touched:last
       ~start:60 ~cut:false
    = `Unavailable);
  check "outline away: a cut keeps the entries the prefix holds"
    (Jide_mutation.outline_away ~items:entries
       ~mutated:(Some [ item "param" "N" 0; item "type" "word" 10 ])
       ~touched:None ~start:20 ~cut:true
    = `Same);
  check "outline away: a cut that renames an entry is a difference"
    (Jide_mutation.outline_away ~items:entries
       ~mutated:(Some [ item "param" "N" 0; item "fn" "word" 10 ])
       ~touched:None ~start:20 ~cut:true
    = `Different);

  (* A deletion inside the body leaves the outline of every other item
     alone, which is what the second criterion asks. *)
  (match
     List.find_opt
       (fun (m : Jide_mutation.mutation) -> m.label = "delete")
       first
   with
  | None -> check "outline away: a deletion exists" false
  | Some mutation -> (
      let touched =
        Jide_mutation.touched_item outlined ~start:mutation.Jide_mutation.start
      in
      let mutated = Jide_mutation.outline mutation.Jide_mutation.text in
      let away =
        Jide_mutation.outline_away ~items:outlined ~mutated ~touched
          ~start:mutation.Jide_mutation.start ~cut:false
      in
      check "outline away: a body deletion keeps it" (away = `Same);
      match List.nth_opt outlined 4 with
      | None -> check "outline away: a cut has an item to drop" false
      | Some last ->
          let cut = last.Jide_mutation.start in
          let prefix = String.sub program 0 cut in
          check "outline away: a cut drops the item it runs into"
            (match Jide_mutation.outline prefix with
            | None -> false
            | Some mutated ->
                Jide_mutation.outline_away ~items:outlined
                  ~mutated:(Some mutated) ~touched:None ~start:cut ~cut:true
                = `Same)));

  (* The harness end to end, on a corpus of its own. *)
  let corpus = temp_dir () in
  write (Filename.concat corpus "one.jazz") program;
  Unix.mkdir (Filename.concat corpus "sub") 0o700;
  write (Filename.concat corpus "sub/two.jazz") program;
  Fun.protect
    ~finally:(fun () -> remove_tree corpus)
    (fun () ->
      let report = Jide_mutation.run ~seed:3 ~corpus () in
      check "run: two files" (report.Jide_mutation.files = 2);
      check "run: the report names the seed" (report.Jide_mutation.seed = 3);
      check "run: the cases ran" (report.Jide_mutation.cases > 0);
      check "run: the single-token cases ran"
        (report.Jide_mutation.single_cases > 0);
      check "run: no exception"
        (report.Jide_mutation.raised = [] && report.Jide_mutation.hung = []);
      check "run: every readable outline is unchanged"
        (report.Jide_mutation.outline_different = []);
      check "run: the criteria hold" (Jide_mutation.passed report);
      let limited = Jide_mutation.run ~seed:3 ~limit:1 ~corpus () in
      check "run: the limit bounds the files" (limited.Jide_mutation.files = 1);

      (* A corpus with no file is not a pass. *)
      let empty = temp_dir () in
      Fun.protect
        ~finally:(fun () -> remove_tree empty)
        (fun () ->
          let report = Jide_mutation.run ~corpus:empty () in
          check "empty corpus: no case" (report.Jide_mutation.cases = 0);
          check "empty corpus: not a pass" (not (Jide_mutation.passed report))));

  (* A corpus that is not there has no cases rather than an exception. *)
  let missing = Filename.concat (temp_dir ()) "absent" in
  let report = Jide_mutation.run ~corpus:missing () in
  check "missing corpus: no case" (report.Jide_mutation.cases = 0);
  check "missing corpus: not a pass" (not (Jide_mutation.passed report));

  if !failures = 0 then Printf.printf "all mutation tests passed\n";
  exit (if !failures = 0 then 0 else 1)
