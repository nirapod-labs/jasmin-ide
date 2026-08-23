(** One single-token mutant per corpus file per family, written to disk.

    The first-error parity run puts a mutated file in front of the checker and
    in front of a real compiler and compares the two first diagnostics, so it
    needs the mutants as files, not as the in-memory texts {!Mutant} builds
    for the recovery harness. {!Mutant} defines what each single-token family
    does and {!Token} splits a text the way the compiler's lexer does; this
    program only chooses one site per file per family, writes the mutated
    text, and records the set in a manifest:

    - {!Mutant.Delete}: one token is removed;
    - {!Mutant.Duplicate}: one token is copied in place;
    - {!Mutant.Swap}: a [;] becomes [}] or a [}] becomes [;].

    {!Mutant.Truncate} is not a single-token mutation and is not emitted.

    The site is a function of the seed, the file's path below the corpus and
    the family's name, so the same seed over the same corpus emits the same
    set, in the same order, on every run and every host. Nothing about this
    host is read or written: the corpus and the output directory are
    arguments, and the program writes nothing outside the output directory. *)

let usage =
  String.concat "\n"
    [
      "usage: mutants --output DIR [options] <corpus directory>";
      "";
      "Write one single-token mutant per corpus file per family, with a";
      "manifest that names each one.";
      "";
      "options:";
      "  --output DIR   where to write the mutants and their manifest.tsv";
      "                 (required; the directory is made if it is absent)";
      "  --seed TEXT    the text that fixes which site each mutant touches";
      "                 (default 0)";
      "  --files N      mutate at most N files, spread evenly over the sorted";
      "                 file list (default 0: every file)";
      "  -h, --help     print this text";
      "";
      "The manifest holds one line per mutant, tab separated:";
      "";
      "  <index>  <family>  <path below the corpus>  <label>";
      "";
      "and the mutant itself is <output>/<index>.jazz.";
      "";
      "exit status: 0 on success, 2 when the command line or the corpus is";
      "unusable.";
    ]

type options = { output : string; seed : string; files : int; corpus : string }

let default_options = { output = ""; seed = "0"; files = 0; corpus = "" }

