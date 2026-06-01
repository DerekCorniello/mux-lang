;; Comments
(line_comment) @comment @comment.line
(block_comment) @comment @comment.block

;; Keywords
(function_declaration "func" @keyword @keyword.declaration)
(function_declaration "returns" @keyword @keyword.declaration)
(function_declaration "common" @keyword @keyword.declaration)
(interface_method_declaration "func" @keyword @keyword.declaration)
(interface_method_declaration "returns" @keyword @keyword.declaration)
(interface_method_declaration "common" @keyword @keyword.declaration)
(lambda_expression "func" @keyword @keyword.declaration)
(lambda_expression "returns" @keyword @keyword.declaration)
(auto_declaration "auto" @type)
(return_statement "return" @keyword @keyword.declaration)
(class_declaration "class" @keyword @keyword.declaration)
(interface_declaration "interface" @keyword @keyword.declaration)
(enum_declaration "enum" @keyword @keyword.declaration)
(import_statement "import" @keyword @keyword.declaration)
(const_declaration "const" @keyword @keyword.declaration)
(if_statement "if" @keyword @keyword.control)
(else_clause "else" @keyword @keyword.control)
(while_statement "while" @keyword @keyword.control)
(for_statement "for" @keyword @keyword.control)
(for_statement "in" @keyword @keyword.operator)
(match_statement "match" @keyword @keyword.control)
(guard_clause "if" @keyword @keyword.control)
(break_statement) @keyword @keyword.control
(continue_statement) @keyword @keyword.control
(trait_clause "is" @keyword @keyword.operator)
(import_alias "as" @keyword @keyword.operator)
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
["(" ")" "{" "}" "[" "]" ","] @punctuation.bracket

;; Member access
(field_access field: (identifier) @property)
(type_path (identifier) "." (identifier) @property)

;; Structured names
(class_declaration name: (identifier) @type)
(interface_declaration name: (identifier) @type)
(enum_declaration name: (identifier) @type)
(function_declaration name: (identifier) @function @function.declaration)
(interface_method_declaration name: (identifier) @function @function.declaration)
(call_expression function: (call_target (identifier) @function.call))
(parameter type: (type_name) @type)
(field_declaration type: (type_name) @type)
(typed_declaration type: (type_name) @type)
(const_declaration type: (type_name) @type)
(function_declaration return_type: (type_name) @type)
(interface_method_declaration return_type: (type_name) @type)
(trait_clause (type_path) @type)
(type_bound_clause (type_path) @type)
(enum_variant name: (identifier) @constructor)

((identifier) @type
 (#match? @type "^(string|bool|void|int|float|char|optional|result|list|map|tuple|set|range|Stringable|Hashable|Thread|Error|Self)$"))

((identifier) @variable.builtin
 (#eq? @variable.builtin "self"))

((identifier) @variable.other)
