module.exports = grammar({
  name: "mux",

  rules: {
    source_file: $ => repeat(choice($.comment, $.statement, $.expression)),

    // Comments
    comment: $ => choice($.line_comment, $.block_comment),
    line_comment: $ => token(seq("//", /.*/)),
    block_comment: $ => token(seq("/*", /[^*]*\*+(?:[^/*][^*]*\*+)*/, "/")),

    // Keywords
    keyword: $ => choice(
      "auto", "func", "returns", "return", "if", "else", "for", "while",
      "match", "const", "class", "interface", "enum", "import", "is", "as",
      "in", "break", "continue", "none", "common"
    ),

    // Literals
    literal: $ => choice(
      $.integer,
      $.float,
      $.boolean,
      $.char_literal,
      $.string_literal,
      $.triple_string_literal,
      $.underscore
    ),

    integer: $ => token(seq(optional(choice("-", "+")), /[0-9][0-9_]*/)),
    float: $ => token(seq(
      optional(choice("-", "+")),
      choice(
        seq(/[0-9][0-9_]*/, ".", optional(/[0-9][0-9_]*/)),
        seq(".", /[0-9][0-9_]*/)
      ),
      optional(seq(choice("e", "E"), optional(choice("-", "+")), /[0-9][0-9_]*/))
    )),
    boolean: $ => choice("true", "false"),
    char_literal: $ => token(seq("'", choice(/\\./, /[^'\\]/), "'")),
    string_literal: $ => token(seq(
      "\"",
      repeat(choice(/\\./, /[^"\\]/)),
      "\""
    )),
    triple_string_literal: $ => token(seq(
      "\"\"\"",
      repeat(choice(/\\./, /[^"\\]/, "\n")),
      "\"\"\""
    )),
    underscore: $ => "_",

    // Operators
    operator: $ => choice(
      "=", "+=", "-=", "*=", "/=", "%=",
      "+", "-", "*", "**", "/", "%", "++", "--",
      "==", "!=", "<", "<=", ">", ">=",
      "&&", "||", "!",
      "&", "..", "."
    ),

    // Delimiters
    delimiter: $ => choice("(", ")", "{", "}", "[", "]", ",", ":"),

    // Identifiers
    identifier: $ => token(seq(/[a-zA-Z_]/, repeat(/[a-zA-Z0-9_]/))),

    // Basic constructs (simplified for highlighting)
    statement: $ => choice(
      seq($.keyword, repeat(choice($.identifier, $.literal, $.operator, $.delimiter))),
      $.comment
    ),

    expression: $ => choice(
      $.identifier,
      $.literal,
      $.operator,
      $.delimiter,
      seq("(", $.expression, ")")
    )
  }
});
