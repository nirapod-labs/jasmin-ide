(** One reference per typed call.

    The requirement is one sentence:

    {v
      Each typed call in the corpus has exactly one reference to its callee
      inside that instruction's range.
    v}

    This program measures that requirement and fails when it does not hold.
    For every file of a corpus it builds what the server builds for an open
    document: the compiler's check ({!Jide_jasmin.Adapter.check}), the typed
    facts it leaves, and the index and the resolver over the same bytes
    ({!Jide_engine.Locate.of_text}). It then asks the engine's own references
    question ({!Jide_engine.Locate.references_at}) at the byte each typed call
    writes its callee at, and counts the answers whose range falls inside the
    instruction that makes the call.

    The question is asked in the file the call's bytes belong to, which is the
    checked file for everything the file writes itself and the required file
    for a call that a [.jinc] writes: the parser splices a required file's
    items into the requiring one and keeps their ranges in the required file's
    own bytes, so the facts carry the file with the ranges
    ({!Jide_jasmin.Facts.call}). Asking in the checked file would read the
    wrong text for such a call.

    - [one]: exactly one reference falls inside the instruction, and it is the
      callee's own name there. This is the requirement.
    - [none]: the instruction holds no reference to the callee. The resolver
      did not resolve the callee, or the index recorded no occurrence where the
      syntax writes it, or both.
    - [more than one]: the instruction holds two references or more, which is
      one instruction naming its callee twice.
    - [not the callee]: one reference falls inside the instruction but the
      declaration it names is not the callee (another name, or not a function),
      or the one reference is a different span. The requirement asks for a
      reference to the callee, so this is a difference too. The names are
      compared unqualified, [A::id] against [id], because that is how the
      index keeps a declaration's name.
    - [not located]: the facts hold the call but no range for its callee's
      name, so there is no reference to ask about. This is a difference too.
      A call that the harness dropped would be neither judged nor counted, so
      the facts keep every call the compiler typed and this verdict reports
      the ones the syntax cannot place.

    The calls come from the typed facts ({!Jide_jasmin.Facts.call}): the
    compiler typed each of them, and the facts carry the instruction's range
    and the range the callee's name was written at. The facts hold every call
    the compiler typed in the items it reached, including the items typed
    before a diagnostic, so a call the facts do not hold is not a typed call.
    The run counts the files with no typed call apart, so the gap is visible.

    Usage:

    {v
      calls --corpus <dir> [--limit N]   measure the requirement over a corpus
      calls --selftest                   check this program against small sources
      calls --help                       print this text
    v}

    <dir> is the `compiler/tests` directory of a Jasmin release, the `compiler`
    directory that holds it, or the release root that holds that. Every
    `.jazz` file under it is read, so no file is skipped by a folder rule. The
    architecture a file is checked for is the deepest directory of its path
    that names one, `common` meaning the compiler's default (x86-64), as
    `test/parity/parity.sh` reads the same corpus. Nothing here needs a
    network, a compiler binary or a second process.

    Stdout carries one line per count and one line per difference; the exit
    status is 0 when every typed call has exactly one reference inside its
    instruction, 1 when one does not, and 2 when the command line is wrong or
    the corpus names no file. *)

module Facts = Jide_jasmin.Facts
module Index = Jide_engine.Index
module Locate = Jide_engine.Locate
module Span = Jide_lang.Span
module Types = Jide_engine.Types

let usage =
  String.concat "\n"
    [
      "usage: calls --corpus <dir> [--limit N]";
      "       calls --selftest";
      "       calls --help";
      "";
      "  --corpus <dir>  a Jasmin release's compiler/tests, compiler or root";
      "  --limit N       read at most N files (for a quick run)";
      "  --selftest      check the counting against small sources";
      "";
      "See the comment at the top of test/parity/calls.ml for the requirement";
      "it measures and what each count means.";
    ]

(* ------------------------------------------------------------------ *)
(* Reading files and choosing an architecture *)

let read (path : string) : string option =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception _ -> None

let default_arch = "x86-64"

(* The compiler's default architecture, and the one a `common` folder means:
   the release checks a `common` file on every architecture it has, and x86-64
   is the one the compiler itself defaults to. *)
let arch_of_path (path : string) : Jide_jasmin.Adapter.arch =
  let rec deepest = function
    | [] -> None
    | part :: rest -> (
        match Jide_jasmin.Adapter.arch_of_string part with
        | Some arch -> Some arch
        | None -> deepest rest)
  in
  match deepest (String.split_on_char '/' path) with
  | Some arch -> arch
  | None -> Option.get (Jide_jasmin.Adapter.arch_of_string default_arch)

(* ------------------------------------------------------------------ *)
(* Judging one call *)

(* The verdict of one call, and what it was read from. The two go together
   because a difference is only useful with the references that were found. *)
type verdict = One | None_found | Many | Not_the_callee | Unlocated

type judgement = {
  call : Facts.call;
  declaration : Index.declaration option;
  inside : Types.location list;
  verdict : verdict;
}

