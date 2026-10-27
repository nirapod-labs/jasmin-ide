; Highlights. Every capture name here is one Zed gives a style to, and
; every node name exists in the pinned grammar.

; Keywords
[
  "fn"
  "param"
  "type"
  "exec"
  "global"
  "namespace"
  "require"
  "from"
  "return"
  "if"
  "else"
  "while"
  "for"
  "to"
  "downto"
  "const"
  "mut"
  "ptr"
] @keyword

(call_conv) @keyword
(storage) @keyword
(keyword) @keyword
(alignment) @keyword

; Module names: @module is the preferred capture, @type the fallback.
(namespace
  name: (identifier) @type @module)

(from
  id: (identifier) @type @module)

; Types
(type_definition
  alias_name: (identifier) @type)

(type_alias) @type

(array_type
  type: (identifier) @type)

[
  (bool_type)
  (int_type)
  (utype)
  (swsize)
  (svsize)
] @type.builtin

; Intrinsics
(intrinsic) @function

(intrinsic
  "#" @punctuation.special)

; An intrinsic instruction such as `#spill(x);` and the compiler's
; `assert("message", condition);` are nodes the pinned upstream grammar does
; not have yet, so a pattern for them here would
; stop this whole file compiling and Zed would drop every highlight. They are
; kept in `highlights.local.scm` beside this file, which Zed does not read and
; `test/queries.sh` checks against the clone.

; Annotations and their contents
(annotations) @attribute

(annotations
  "#" @punctuation.special)

(annotations
  (keyword) @keyword)

(annotations
  (string_literal) @string)

; An annotation value is an expression, so a number in one is an
; `int_literal` and the literal rule below colours it with the rest.

; Functions
(function_definition
  name: (identifier) @function)

(call_expr
  function: (identifier) @function)

(call_instr
  function: (identifier) @function)

"ArrayInit" @function

; Variables and parameters
(variable) @variable
(parameter) @variable.parameter
(ignore) @variable.special

; Literals
(int_literal) @number

(string_literal) @string
(escape_sequence) @string.escape

[
  (true)
  (false)
] @boolean

; Operators
(operator) @operator

(unary_expr
  [
    "!"
    "-"
  ] @operator)

(ternary_expr
  "?" @operator)

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ";"
  ","
  ":"
] @punctuation.delimiter

; Comments
(comment) @comment
