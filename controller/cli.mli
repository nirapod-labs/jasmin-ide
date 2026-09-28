(** The command line of the server binary.

    These forms, and nothing else:

    - no argument serves the protocol, which is the mode an editor starts;
    - [--stdio] names the transport, standard input and output, and is
      accepted for compatibility: a language client passes it to every server
      it starts. Those two streams are the only transport this server has, so
      the option asks for what would happen anyway. It is dropped before the
      mode is read, so it may precede any other form and changes none of
      them: [--stdio] alone serves and `--stdio --version` prints the version;
    - [check ...] is the batch checker's own command line, read by
      {!Jide_check.Request.parse} and not here;
    - [--version] ([-v]) and [--help] ([-h]) print one thing and return
      status 0;
    - any other option, and any argument that is not an option, is refused.
      Accepting [--stdio] widens nothing else: an option this program does not
      take is still refused after it, with the same message and status.

    The refusal is the point of this module. Before it existed an argument
    the binary did not know fell through to the protocol loop, which waited
    for a client that was never going to come and, at the end of input,
    printed [input closed] and exited 1: `jasmin-lsp --version` was that
    fall-through, and so was a mistyped option. An argument this program does
    not take is a command line it refuses, and it says so on standard error
    and exits 2 without touching standard output. The protocol stream belongs
    to the session: a byte written into it by a bad command line would be
    read as a frame header by the next reader of that stream.

    Only the first argument is read, because the only mode with arguments of
    its own is [check]. Reading a [check] argument here would mean two
    readers for one command line, and the one that ran first would be the one
    that was wrong: `check --help` is the checker's help, not the server's. *)

type t =
  | Serve  (** no argument, or [--stdio] and nothing else: the protocol loop *)
  | Version  (** [--version] or [-v] *)
  | Help  (** [--help] or [-h] *)
  | Check of string list  (** [check] and its arguments, unread here *)
  | Unknown of string  (** an option this program does not take *)
  | Unexpected of string  (** an argument that is not an option at all *)

val parse : string list -> t
(** [parse arguments] is the form the arguments name, with the program name
    already removed. [--stdio] is dropped first, so it names no form of its
    own; whatever follows it is read as if it were the first word. *)

val refusal : t -> string list
(** [refusal t] is the lines to write on standard error for {!Unknown} and
    {!Unexpected}, the detail first and the usage line last, and the empty
    list for every other form. *)

val usage_line : string
(** The one line {!refusal} ends with and the first line of {!help}: the forms
    of the whole command line, [--stdio] included. *)

val version_line : unit -> string
(** [version_line ()] is the whole `--version` output, without its line
    break: the server's own version, the Jasmin version the server links, and
    which source that Jasmin came from, in the one form `design/INFRA.md`
    section 2 asks for:

    {[
      jasmin-lsp <server version> (<jasmin version>, built from <route>)
    ]}

    The Jasmin version is the library's own string, as its compiler prints it
    (`Jasmin Compiler 2026.09.0`), so the line cannot drift from what the
    linked library says about itself. [<route>] is one of two fixed phrases,
    never a guess: the build records which one applies ({!Build_route}),
    because nothing at run time can tell a vendored library from an installed
    one — they carry the same module name, so `%{lib-available:jasmin}`
    answers yes either way. *)

val help : unit -> string
(** [help ()] is the `--help` block: {!usage_line} first, the modes that
    exist, the three options above with the transport the [--stdio] one names,
    and the exit codes, one line each, without the last line break. *)
