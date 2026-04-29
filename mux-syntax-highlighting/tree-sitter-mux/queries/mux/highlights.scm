;; Comments
(line_comment) @comment @comment.line
(block_comment) @comment @comment.block

;; Keywords
(keyword_control) @keyword @keyword.control
(keyword_declaration) @keyword @keyword.declaration
(keyword_operator) @keyword @keyword.declaration
(keyword_constant) @constant @constant.language

;; Numbers
(int_literal) @number @constant.numeric.integer
(float_literal) @number @constant.numeric.float

;; Booleans
(boolean) @constant @constant.language

;; Literals
(char_literal) @string @string.quoted.single
(string_literal) @string @string.quoted.double
(triple_string_literal) @string @string.quoted.triple.double
(underscore) @variable @variable.language

;; Delimiters
(delimiter) @punctuation.bracket

;; Identifiers
(identifier) @variable @variable.other

;; Structured names
(function_declaration name: (identifier) @entity.name.function)
(call_expression function: (identifier) @entity.name.function.call)
(call_expression function: (field_access field: (identifier) @entity.name.function.call))

((field_access field: (identifier) @entity.name.function.call))

((identifier) @type
 (#match? @type "^(string|bool|void|int|float|char|optional|result|list|map|tuple|set|range|Stringable|Hashable|Thread|Outer|Point)$"))

((identifier) @variable.builtin
 (#eq? @variable.builtin "self"))

;; Structured names
(function_declaration name: (identifier) @entity.name.function)
(call_expression function: (identifier) @entity.name.function.call)
(call_expression function: (field_access field: (identifier) @entity.name.function.call))

((field_access field: (identifier) @entity.name.function.call))

((identifier) @type
 (#match? @type "^(string|bool|void|int|float|char|optional|result|list|map|tuple|set|range|Stringable|Hashable|Thread|Outer|Point)$"))

((identifier) @variable.builtin
 (#eq? @variable.builtin "self"))
