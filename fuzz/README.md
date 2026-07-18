# Fuzz

`fuzz/` holds two fuzzing surfaces over the parts of the server that read a
client's bytes: source text into the checker, and protocol packets into the
state machine. It is not part of the build's library code; the `make fuzz`
step runs it.

```
make fuzz                                   # two minutes, seed 1
make fuzz FUZZ_SECONDS=600 FUZZ_SEED=7      # ten minutes, another seed
make fuzz CORPUS=/path/to/jasmin/compiler/tests
```

## The two surfaces

**Text.** The generator writes a source text and the harness checks it with
`Jide_jasmin.Adapter.check`, then again through `Jide_engine.Engine.check` so
the store, the analysis key and the spans the engine derives are covered too.
The texts are a program of the shape the checker accepts, junk, the two
joined, a byte-level alteration of any of those, and, when a corpus is given,
an altered unit of it. A case holds when:

- the call returns — an exception that reaches the harness is a finding, since
  the library is supposed to catch its own;
- the same text checked twice gives the same diagnostics, so no compiler state
  carries from one document to the next;
- no span starts before the text or ends before it starts;
- the engine's spans lie inside the document it was given, which covers the
  byte-order mark shift and the warnings the compiler reports by line and
  column rather than by offset;
- at three names of the text, prepareRename and rename are asked as the
  server asks them, to a name the text does not hold: a rename that is made
  has its edits inside the text, in order and apart, and renaming the
  declaration back to its old name restores the text byte for byte.

**Edit.** The generator drives `Jide_controller.Server.handle_packet` with a
whole session: a request before `initialize`, the `initialize`, random opens,
full-sync changes, closes, requests the server does not serve, prepareRename and
rename at random positions with random names (answered by the engine, as the
native server answers them), notifications it does not know and one it cannot
parse, then `shutdown` and `exit`. The texts
carry astral characters, NUL bytes, carriage returns and long lines. A case
holds when:

- a full-sync change leaves exactly the text sent, at exactly the version
  sent, and the versions of a document only grow;
- a request gets exactly one reply, with the id it was given and the code the
  protocol names for that state;
- a notification gets no reply, and one that cannot be parsed or arrives
  outside `Running` leaves the document store alone;
- every diagnostic the analysis produces lies inside the document;
- a prepareRename is a range inside the document, `null` or a refusal, and a
  rename is a workspace edit whose ranges lie inside the documents it names,
  or a refusal;
- the exit status after a `shutdown` is 0.

