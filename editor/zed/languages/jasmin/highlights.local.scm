; Highlights for two nodes the pinned grammar does not have yet. MIT, see LICENSE.
;
; This file is not one of the five query files Zed reads (`highlights.scm`,
; `brackets.scm`, `outline.scm`, `indents.scm`, `textobjects.scm`), and it must
; not become one: the extension pins the official upstream grammar, which has
; neither node, and a query file naming a node the grammar does not have does
; not just lose that pattern — Zed drops the whole file, and highlighting
; stops working for every user. The patterns wait here until the grammar that
; has the nodes is the one the pin names, which needs the project to go
; public.
;
; `editor/zed/test/queries.sh` runs this file against a grammar whose
; `src/node-types.json` has both nodes — `JASMIN_GRAMMAR_DIR` pointed at the
; lab clone — and skips it against the pin, so the patterns are exercised
; without ever being loaded by Zed.

; Intrinsic instructions such as `#spill(x);`, a statement of their own since
; the grammar parses them as one rather than as an annotation.
(intrinsic_instr
  intrinsic: (intrinsic) @function)

; `assert("message", condition);`, the compiler's ASSERT statement.
(assert_statement
  "assert" @keyword
  message: (string_literal) @string)
