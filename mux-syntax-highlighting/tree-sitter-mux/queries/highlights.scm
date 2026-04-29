;; Comments
(line_comment) @comment @comment.line
(block_comment) @comment @comment.block

;; Keywords
(keyword_control) @keyword @keyword.control
((keyword_declaration) @keyword @keyword.declaration
  (#not-match? @keyword "^auto$"))
((keyword_declaration) @type
  (#eq? @type "auto"))
(keyword_operator) @keyword @keyword.declaration
(keyword_constant) @constant @constant.language
(function_declaration "func" @keyword @keyword.declaration)
(function_declaration "returns" @keyword @keyword.declaration)
(lambda_expression "func" @keyword @keyword.declaration)
(lambda_expression "returns" @keyword @keyword.declaration)

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
(function_declaration name: (identifier) @function)
(call_expression function: (identifier) @function.call)
(call_expression function: (field_access field: (identifier) @function.call))
(field_access field: (identifier) @function.field)

((identifier) @type
 (#match? @type "^(string|bool|void|int|float|char|optional|result|list|map|tuple|set|range|Stringable|Hashable|Thread|Outer|Point)$"))

((identifier) @variable.builtin
 (#eq? @variable.builtin "self"))
