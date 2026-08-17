; Outline. Each top-level name in a Jasmin file becomes one item.

(function_definition
  name: (identifier) @name) @item

(type_definition
  alias_name: (identifier) @name) @item

(namespace
  name: (identifier) @name) @item

(param
  name: (variable) @name) @item

(global
  name: (variable) @name) @item

(exec
  pex_name: (variable) @name) @item
