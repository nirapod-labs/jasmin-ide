(* The arity and signature of every intrinsic call.

   The requirement is that signature help for every intrinsic call in a corpus
   has the arity of the compiler's instruction description. The check goes
   further: for every intrinsic of every architecture the linked Jasmin accepts,
   the signature the server answers equals the compiler's own table.

   The expected side is the native compiler, [--jasminc]. Its messages give the
   number of arguments and of results of each instruction (see oracle.ml), so
   the arity the server answers is compared with the compiler's own count and
   not with a copy of the library's table. The library is a second opinion,
   used for the parameter types and the whole label, which the compiler's
   messages do not carry; it is checked against the compiler's counts too.

   Two parts:

   - The table part takes every spelling the table accepts: each name with
     each of its suffixes, and the bare name where the table takes it, plus
     four names the compiler types by a rule of its own and two names the table
     does not hold. Each spelling is written in a document under a
     [jasmin.project] that names the architecture; the engine analyses the
     document, and [Complete.signature_help] is asked at the call with the
     snapshot's own architecture, which is the value the server passes.

   - The corpus part takes every intrinsic call in the [.jazz] and [.jinc]
     files under the directory named on the command line. Each call is asked
     under the architecture its path names, which is the corpus's own rule
     (see [archs_of_path]). A corpus has no project file, so this part calls
     [Complete.signature_help] directly.

   A comparison agrees when the server's answer has the compiler's number of
   arguments and results, or when both refuse. The table part also compares the
   parameters and the whole label with the library's description, and the
   library's counts with the compiler's. Anything else is a difference, and a
   difference fails the run.

   usage: arity [--jasminc <path>] [--control ignore-arch | --control drop-parameter]
                [<corpus-root>]

   Without [--jasminc] the table part compares the server with the library's
   description only, and a corpus cannot be run: its expected side is the
   compiler's. [--control] answers wrongly on purpose, so the comparison is seen
   to fail. [ignore-arch] answers every call as x86-64, which is what a server
   answers when it does not pass the project's architecture; [drop-parameter]
   answers without the last parameter of each call that has one. It applies to
   the table part and to the corpus part.

   Exit status: 0 when every comparison agrees, 1 when one differs, 2 when the
   run cannot be made. *)

open Jide_engine
module Intrinsics = Jide_jasmin.Intrinsics
module Arch = Jide_jasmin.Arch

let usage =
  "usage: arity [--jasminc <path>] [--control ignore-arch | --control \
   drop-parameter] [<corpus-root>]"

(* A run that cannot be made. It is raised, not exited, so the temporary tree
   is removed on the way out, and [main] prints it. *)
exception Run_failed of string

let die fmt = Printf.ksprintf (fun message -> raise (Run_failed message)) fmt

(* ------------------------------------------------------------------ *)
(* Controls *)

type control = No_control | Ignore_arch | Drop_parameter

let control_of_string = function
  | "ignore-arch" -> Ignore_arch
  | "drop-parameter" -> Drop_parameter
  | other -> die "unknown control %s (ignore-arch, drop-parameter)" other

let control_name = function
  | No_control -> "none"
  | Ignore_arch -> "ignore-arch"
  | Drop_parameter -> "drop-parameter"

(* ------------------------------------------------------------------ *)
(* The library's description, rendered the way the server labels it *)

let render_ty = function
  | Intrinsics.Bool -> "bool"
  | Intrinsics.Int -> "int"
  | Intrinsics.Word { signed; bits } ->
      (if signed then "s" else "u") ^ string_of_int bits
  | Intrinsics.Array { bits; length } -> Printf.sprintf "u%d[%s]" bits length

(* The parameters and the whole label one description gives a call written as
   [written]. This is a second rendering, written here, so the label check
   compares two renderings of one description and not one with itself. *)
let expected_of ~written (description : Intrinsics.t_signature) =
  let parameters = List.map render_ty description.Intrinsics.arguments in
  let results = List.map render_ty description.Intrinsics.results in
  let label =
    Printf.sprintf "%s(%s)%s" written
      (String.concat ", " parameters)
      (match results with [] -> "" | _ -> " -> " ^ String.concat ", " results)
  in
  (parameters, label)

(* The number of results in a server label: the items after the arrow, or none.
   The label is written by [Complete.label], and no type it prints has a comma. *)
let results_of_label label =
  let arrow = " -> " in
  let width = String.length arrow in
  let rec find index =
    if index + width > String.length label then None
    else if String.equal (String.sub label index width) arrow then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> 0
  | Some index ->
      let start = index + width in
      let after = String.sub label start (String.length label - start) in
      List.length (String.split_on_char ',' after)

(* ------------------------------------------------------------------ *)
(* Spellings of the table *)

(* The four names [Pretyping.tt_prim] types by a rule before it reaches the
   table. The compiler accepts them without an instruction description, so they
   have no count, and the server must refuse them. *)
let pseudo_names = [ "spill"; "unspill"; "randombytes"; "declassify" ]

(* Names the table does not hold, under any architecture. *)
let unknown_names = [ "NOSUCH_64"; "ADD_999" ]

(* One spelling the table accepts, with the library's own description of it. *)
type spelling = {
  text : string;  (** the name after the [\#], suffix included *)
  library : Intrinsics.t_signature option;
}

let spellings_of ~arch =
  let named =
    List.concat_map
      (fun (entry : Intrinsics.t) ->
        (* The bare name is a spelling of its own whenever the table takes
           it, which for an x86 name with suffixes is the default size. *)
        let suffixes =
          List.sort_uniq String.compare ("" :: entry.Intrinsics.suffixes)
        in
        List.map
          (fun suffix ->
            let text =
              match
                Intrinsics.written_of ~arch ~name:entry.Intrinsics.name ~suffix
              with
              | Some text -> text
              | None ->
                  die
                    "the table lists %s under %s but writes no spelling for it"
                    entry.Intrinsics.name arch
            in
            {
              text;
              library =
                Intrinsics.signature_of ~arch ~name:entry.Intrinsics.name
                  ~suffix;
            })
          suffixes)
      (Intrinsics.all ~arch)
  in
  let unlisted text = { text; library = None } in
  named @ List.map unlisted pseudo_names @ List.map unlisted unknown_names

(* ------------------------------------------------------------------ *)
(* Comparisons *)

(* The server's answer for one call, after a control has been applied. *)
let shown_under ~control (shown : Complete.signature option) =
  match (control, shown) with
  | Drop_parameter, Some signature ->
      let rec drop_last = function
        | [] -> []
        | [ _ ] -> []
        | item :: rest -> item :: drop_last rest
      in
      Some
        {
          signature with
          Complete.parameters = drop_last signature.Complete.parameters;
        }
  | _ -> shown

(* The server against the compiler's answer: the number of arguments and of
   results, or that both refuse. *)
let judge_against (answer : Oracle.answer) (shown : Complete.signature option) =
  match (answer, shown) with
  | Oracle.Described { args; results }, None ->
      [
        Printf.sprintf
          "refuses it, but the compiler describes %d arguments and %d results"
          args results;
      ]
  | Oracle.Described { args; results }, Some signature ->
      let shown_args = List.length signature.Complete.parameters in
      let shown_results = results_of_label signature.Complete.label in
      if shown_args = args && shown_results = results then []
      else
        [
          Printf.sprintf
            "answers %d arguments and %d results, the compiler %d and %d"
            shown_args shown_results args results;
        ]
  | (Oracle.Refused _ | Oracle.Rule), None -> []
  | (Oracle.Refused _ | Oracle.Rule), Some signature ->
      [
        Printf.sprintf "answers %d arguments where the compiler refuses it"
          (List.length signature.Complete.parameters);
      ]

(* The server against the library's own description: callee, parameters and
   label. *)
let judge_library ~written (library : Intrinsics.t_signature option)
    (shown : Complete.signature option) =
  match (library, shown) with
  | None, None -> []
  | None, Some signature ->
      [
        Printf.sprintf
          "answers %d parameters where the table holds no such spelling"
          (List.length signature.Complete.parameters);
      ]
  | Some description, None ->
      [
        Printf.sprintf "refuses it where the table describes %d parameters"
          (List.length description.Intrinsics.arguments);
      ]
  | Some description, Some signature ->
      let parameters, label = expected_of ~written description in
      if not (String.equal signature.Complete.callee written) then
        [ Printf.sprintf "callee %S" signature.Complete.callee ]
      else if
        List.length signature.Complete.parameters <> List.length parameters
      then
        [
          Printf.sprintf "arity %d, the table's %d"
            (List.length signature.Complete.parameters)
            (List.length parameters);
        ]
      else if signature.Complete.parameters <> parameters then
        [
          Printf.sprintf "parameters [%s], the table's [%s]"
            (String.concat "; " signature.Complete.parameters)
            (String.concat "; " parameters);
        ]
      else if not (String.equal signature.Complete.label label) then
        [
          Printf.sprintf "label %S, the table's %S" signature.Complete.label
            label;
        ]
      else []

(* The library against the compiler's counts. *)
let judge_library_against_compiler (library : Intrinsics.t_signature option)
    (answer : Oracle.answer) =
  match (library, answer) with
  | None, (Oracle.Refused _ | Oracle.Rule) -> []
  | Some _, (Oracle.Refused _ | Oracle.Rule) ->
      [ "the library describes it, the compiler refuses it" ]
  | None, Oracle.Described { args; results } ->
      [
        Printf.sprintf
          "the library refuses it, the compiler describes %d arguments and %d \
           results"
          args results;
      ]
  | Some description, Oracle.Described { args; results } ->
      let library_args = List.length description.Intrinsics.arguments in
      let library_results = List.length description.Intrinsics.results in
      if library_args = args && library_results = results then []
      else
        [
          Printf.sprintf
            "the library describes %d arguments and %d results, the compiler \
             %d and %d"
            library_args library_results args results;
        ]

(* ------------------------------------------------------------------ *)
(* The table part *)

