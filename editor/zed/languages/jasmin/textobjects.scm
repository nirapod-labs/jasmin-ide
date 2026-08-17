; Text objects. A function is the whole definition, its body the braces and
; what is inside them; a namespace is the section around them.

(function_definition) @function.around
(function_body) @function.inside

(namespace) @class.around

(comment)+ @comment.around
