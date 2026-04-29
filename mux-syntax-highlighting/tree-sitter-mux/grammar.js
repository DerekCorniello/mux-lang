module.exports = grammar({
  name: "mux",

  conflicts: $ => [
    [$.param_item, $.type_arg],
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.function_declaration,
      $.lambda_expression,
      $.call_expression,
      $.field_access,
      $.line_comment,
      $.block_comment,
      $.keyword_control,
      $.keyword_declaration,
      $.keyword_operator,
      $.keyword_constant,
      $.op_assign,
      $.op_arithmetic,
      $.op_comparison,
      $.op_logical,
      $.op_other,
      $.int_literal,
      $.float_literal,
      $.boolean,
      $.char_literal,
      $.string_literal,
      $.triple_string_literal,
      $.underscore,
      $.delimiter,
      $.identifier
    )),

    lambda_expression: $ => prec.right(seq(
      'func',
      '(',
      optional($.param_list),
      ')',
      'returns',
      field('return_type', $.type_name),
      $.block
    )),

    function_declaration: $ => prec.right(seq(
      'func',
      field('name', $.identifier),
      optional($.type_params),
      '(',
      optional($.param_list),
      ')',
      'returns',
      field('return_type', $.type_name),
      $.block
    )),

    block: $ => seq(
      '{',
      repeat(choice(
        $.function_declaration,
        $.lambda_expression,
        $.call_expression,
        $.field_access,
        $.line_comment,
        $.block_comment,
        $.keyword_control,
        $.keyword_declaration,
        $.keyword_operator,
        $.keyword_constant,
        $.op_assign,
        $.op_arithmetic,
        $.op_comparison,
        $.op_logical,
        $.op_other,
        $.int_literal,
        $.float_literal,
        $.boolean,
        $.char_literal,
        $.string_literal,
        $.triple_string_literal,
        $.underscore,
        $.delimiter,
        $.identifier
      )),
      '}'
    ),

    call_expression: $ => prec.left(seq(
      field('function', choice($.field_access, $.identifier)),
      '(',
      optional($.argument_list),
      ')'
    )),

    field_access: $ => prec.left(2, seq(
      field('object', choice($.call_expression, $.field_access, $.identifier, $.type_name)),
      '.',
      field('field', $.identifier)
    )),

    type_params: $ => seq(
      '<',
      optional($.type_param_list),
      '>'
    ),

    type_param_list: $ => seq(
      $.type_param,
      repeat(seq(',', $.type_param))
    ),

    type_param: $ => choice(
      $.identifier,
      $.type_name
    ),

    param_list: $ => seq(
      $.param_item,
      repeat(seq(',', $.param_item))
    ),

    param_item: $ => choice(
      $.type_name,
      $.identifier,
      $.keyword_constant,
      $.keyword_operator,
      $.lambda_expression,
      $.int_literal,
      $.float_literal,
      $.boolean,
      $.char_literal,
      $.string_literal,
      $.triple_string_literal,
      $.underscore,
      $.field_access,
      $.call_expression
    ),

    argument_list: $ => seq(
      $.argument_item,
      repeat(seq(',', $.argument_item))
    ),

    argument_item: $ => choice(
      $.call_expression,
      $.field_access,
      $.type_name,
      $.identifier,
      $.keyword_constant,
      $.keyword_operator,
      $.lambda_expression,
      $.delimiter,
      $.int_literal,
      $.float_literal,
      $.boolean,
      $.char_literal,
      $.string_literal,
      $.triple_string_literal,
      $.underscore,
      $.op_other,
      $.op_comparison,
      $.op_logical,
      $.op_arithmetic,
      $.op_assign
    ),

    type_name: $ => choice(
      $.identifier,
      $.type_path,
      $.function_type
    ),

    type_path: $ => prec.left(1, seq(
      $.identifier,
      repeat(seq('.', $.identifier)),
      optional($.type_args)
    )),

    type_args: $ => seq(
      '<',
      optional($.type_arg_list),
      '>'
    ),

    type_arg_list: $ => seq(
      $.type_arg,
      repeat(seq(',', $.type_arg))
    ),

    type_arg: $ => choice(
      $.type_name,
      $.identifier
    ),

    function_type: $ => seq(
      'func',
      '(',
      optional($.type_arg_list),
      ')',
      'returns',
      $.type_name
    ),

    line_comment: $ => token(prec(1, seq('//', /.*/))),

    block_comment: $ => token(prec(1, seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'))),

    keyword_control: $ => token(prec(1, choice(
      'if', 'else', 'for', 'while', 'match', 'break', 'continue'
    ))),

    keyword_declaration: $ => token(prec(1, choice(
      'auto', 'func', 'returns', 'return', 'const', 'class', 'interface', 'enum', 'import',
      'is', 'as', 'in'
    ))),

    keyword_constant: $ => token(prec(1, choice(
      'none', 'true', 'false', 'common'
    ))),

    keyword_operator: $ => token(prec(1, choice(
      'is', 'as', 'in'
    ))),

    op_assign: $ => token(prec(1, choice(
      '=', '+=', '-=', '*=', '/=', '%='
    ))),

    op_arithmetic: $ => token(prec(1, choice(
      '+', '-', '*', '**', '/', '%', '++', '--'
    ))),

    op_comparison: $ => token(prec(1, choice(
      '==', '!=', '<', '<=', '>', '>='
    ))),

    op_logical: $ => token(prec(1, choice(
      '&&', '||'
    ))),

    op_other: $ => token(prec(1, choice(
      '!', '&', '.', '..'
    ))),

    int_literal: $ => token(prec(1, seq(
      optional('-'), /[0-9][0-9_]*/
    ))),

    float_literal: $ => token(prec(1, choice(
      seq(optional('-'), /[0-9][0-9_]*\.[0-9][0-9_]*/),
      seq(optional('-'), /[0-9][0-9_]*\.[0-9][0-9_]*[eE][-+]?[0-9][0-9_]*/),
      seq(optional('-'), /\.[0-9][0-9_]*/),
      seq(optional('-'), /\.[0-9][0-9_]*[eE][-+]?[0-9][0-9_]*/)
    ))),

    boolean: $ => token(choice('true', 'false')),

    char_literal: $ => token(seq(
      "'",
      choice(/\\./, /[^'\\]/),
      "'"
    )),

    string_literal: $ => token(seq(
      '"',
      repeat(choice(/\\./, /[^"\\]/)),
      '"'
    )),

    triple_string_literal: $ => token(seq(
      '"""',
      repeat(choice(/\\./, /[^"\\]/, '\n')),
      '"""'
    )),

    underscore: $ => token('_'),

    delimiter: $ => token(choice(
      '(', ')', '{', '}', '[', ']', ',', ':'
    )),

    identifier: $ => token(seq(
      /[a-zA-Z_]/,
      repeat(/[a-zA-Z0-9_]/)
    ))
  }
});
