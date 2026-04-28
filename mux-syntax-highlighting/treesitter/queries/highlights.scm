;; Comments
(line_comment) @comment.line.double-slash.mux
(block_comment) @comment.block.mux

;; Keywords
(keyword) @keyword.control.mux
((keyword) @keyword.declaration.mux
  (#match? @keyword.declaration.mux "^(auto|func|returns|return|const|class|interface|enum|import)$"))
((keyword) @keyword.operator.mux
  (#match? @keyword.operator.mux "^(as|in|is)$"))
((keyword) @constant.language.mux
  (#match? @constant.language.mux "^(none|true|false|common)$"))

;; Literals
(integer) @constant.numeric.integer.mux
(float) @constant.numeric.float.mux
(boolean) @constant.language.mux
(char_literal) @string.quoted.single.mux
(string_literal) @string.quoted.double.mux
(triple_string_literal) @string.quoted.triple.double.mux
(underscore) @variable.language.mux

;; Operators
(operator) @keyword.operator.mux

;; Delimiters
(delimiter) @punctuation.mux

;; Identifiers
(identifier) @variable.other.mux
