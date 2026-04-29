module.exports = grammar({
  name: 'mux',

  conflicts: $ => [
    [$.call_expression, $.field_access],
    [$.type_name, $.call_expression],
    [$.primary_expression, $.type_path],
    [$.field_access, $.type_path],
    [$.declaration, $.primary_expression],
    [$.source_file, $.expression_statement],
    [$.block, $.expression_statement],
    [$.postfix_expression, $.call_expression],
    [$.postfix_expression, $.index_access],
    [$.param_item, $.type_arg],
    [$.param_item, $.type_path],
    [$.param_item, $.type_list],
    [$.type_param, $.type_path],
    [$.identifier_pattern, $.enum_variant_pattern],
    [$.if_statement, $.guard_clause],
    [$.pattern, $.expression],
    [$.match_arm, $.statement],
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.declaration,
      $.statement,
      $.expression,
      $.line_comment,
      $.block_comment,
      $.keyword_control,
      $.keyword_declaration,
      $.keyword_operator,
      $.op_assign,
      $.op_arithmetic,
      $.op_comparison,
      $.op_logical,
      $.op_other,
      $.delimiter
    )),

    declaration: $ => choice(
      $.function_declaration,
      $.lambda_expression,
      $.auto_declaration,
      $.typed_declaration,
      $.const_declaration,
      $.import_statement,
      $.class_declaration,
      $.interface_declaration,
      $.enum_declaration,
    ),

    statement: $ => choice(
      $.if_statement,
      $.while_statement,
      $.for_statement,
      $.match_statement,
      $.return_statement,
      $.break_statement,
      $.continue_statement,
      $.block,
      $.expression_statement,
    ),

    block: $ => seq(
      '{',
      repeat(choice(
        $.declaration,
        $.statement,
        $.expression,
        $.line_comment,
        $.block_comment,
        $.keyword_control,
        $.keyword_declaration,
        $.keyword_operator,
        $.op_assign,
        $.op_arithmetic,
        $.op_comparison,
        $.op_logical,
        $.op_other,
        $.delimiter
      )),
      '}'
    ),

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

    lambda_expression: $ => prec.right(seq(
      'func',
      '(',
      optional($.param_list),
      ')',
      'returns',
      field('return_type', $.type_name),
      $.block
    )),

    auto_declaration: $ => seq(
      'auto',
      field('name', $.identifier),
      '=',
      field('value', $.expression)
    ),

    typed_declaration: $ => seq(
      field('type', $.type_name),
      field('name', $.identifier),
      '=',
      field('value', $.expression)
    ),

    const_declaration: $ => seq(
      'const',
      field('type', $.type_name),
      field('name', $.identifier),
      '=',
      field('value', $.expression)
    ),

    import_statement: $ => seq(
      'import',
      field('module', $.module_path),
      optional($.import_alias)
    ),

    import_alias: $ => seq(
      'as',
      choice($.identifier, $.underscore)
    ),

    class_declaration: $ => seq(
      'class',
      field('name', $.identifier),
      optional($.type_params),
      optional($.trait_clause),
      $.class_body
    ),

    interface_declaration: $ => seq(
      'interface',
      field('name', $.identifier),
      optional($.type_params),
      $.interface_body
    ),

    enum_declaration: $ => seq(
      'enum',
      field('name', $.identifier),
      optional($.type_params),
      $.enum_body
    ),

    trait_clause: $ => seq(
      'is',
      $.type_path,
      repeat(seq(',', $.type_path))
    ),

    class_body: $ => seq('{', repeat(choice($.field_declaration, $.function_declaration, $.statement, $.line_comment, $.block_comment)), '}'),
    interface_body: $ => seq('{', repeat(choice($.field_declaration, $.function_declaration, $.statement, $.line_comment, $.block_comment)), '}'),
    enum_body: $ => seq('{', repeat(choice($.enum_variant, $.line_comment, $.block_comment)), '}'),

    field_declaration: $ => seq(
      optional('const'),
      field('type', $.type_name),
      field('name', $.identifier),
      optional(seq('=', $.expression))
    ),

    param_list: $ => seq(
      $.param_item,
      repeat(seq(',', $.param_item))
    ),

    param_item: $ => choice(
      $.type_name,
      $.identifier,
      $.keyword_constant,
      $.boolean,
      $.char_literal,
      $.string_literal,
      $.triple_string_literal,
      $.underscore,
      $.call_expression,
      $.field_access
    ),

    enum_variant: $ => seq(
      field('name', $.identifier),
      optional(seq('(', optional($.type_list), ')'))
    ),

    if_statement: $ => seq(
      'if',
      field('condition', $.expression),
      $.block,
      optional($.else_clause)
    ),

    else_clause: $ => seq(
      'else',
      choice($.if_statement, $.block)
    ),

    while_statement: $ => seq(
      'while',
      field('condition', $.expression),
      $.block
    ),

    for_statement: $ => seq(
      'for',
      optional($.type_name),
      field('name', $.identifier),
      'in',
      field('iter', $.expression),
      $.block
    ),

    match_statement: $ => seq(
      'match',
      field('value', $.expression),
      '{',
      repeat($.match_arm),
      '}'
    ),

    match_arm: $ => seq(
      field('pattern', $.pattern),
      optional($.guard_clause),
      choice($.block, $.statement, $.expression_statement)
    ),

    guard_clause: $ => seq(
      'if',
      field('condition', $.expression)
    ),

    return_statement: $ => prec.right(seq(
      'return',
      optional(field('value', $.expression))
    )),

    break_statement: $ => 'break',
    continue_statement: $ => 'continue',
    expression_statement: $ => $.expression,

    expression: $ => choice(
      $.assignment_expression,
      $.logic_or_expression,
    ),

    assignment_expression: $ => prec.right(seq(
      field('left', $.logic_or_expression),
      field('operator', choice('=', '+=', '-=', '*=', '/=', '%=')),
      field('right', $.expression)
    )),

    logic_or_expression: $ => prec.left(seq(
      $.logic_and_expression,
      repeat(seq('||', $.logic_and_expression))
    )),

    logic_and_expression: $ => prec.left(seq(
      $.equality_expression,
      repeat(seq('&&', $.equality_expression))
    )),

    equality_expression: $ => prec.left(seq(
      $.comparison_expression,
      repeat(seq(choice('==', '!='), $.comparison_expression))
    )),

    comparison_expression: $ => prec.left(seq(
      $.sum_expression,
      repeat(seq(choice('<', '<=', '>', '>='), $.sum_expression))
    )),

    sum_expression: $ => prec.left(seq(
      $.product_expression,
      repeat(seq(choice('+', '-'), $.product_expression))
    )),

    product_expression: $ => prec.left(seq(
      $.power_expression,
      repeat(seq(choice('*', '/', '%'), $.power_expression))
    )),

    power_expression: $ => prec.right(choice(
      seq($.unary_expression, '**', $.power_expression),
      $.unary_expression
    )),

    unary_expression: $ => prec.right(choice(
      seq(choice('!', '-', '&', '*', '++', '--'), $.unary_expression),
      $.postfix_expression
    )),

    postfix_expression: $ => prec.left(seq(
      $.primary_expression,
      repeat(choice(
        $.call_expression,
        $.field_access,
        $.index_access
      ))
    )),

    call_expression: $ => prec.left(seq(
      field('function', choice($.field_access, $.identifier)),
      optional($.type_args),
      '(',
      optional($.argument_list),
      ')'
    )),

    field_access: $ => prec.left(seq(
      field('object', choice($.call_expression, $.field_access, $.identifier, $.type_name)),
      '.',
      field('field', $.identifier)
    )),

    index_access: $ => prec.left(seq(
      field('object', choice($.call_expression, $.field_access, $.identifier)),
      '[',
      field('index', $.expression),
      ']'
    )),

    primary_expression: $ => choice(
      $.literal,
      $.keyword_constant,
      $.identifier,
      $.underscore,
      $.grouped_expression,
      $.list_literal,
      $.lambda_expression
    ),

    grouped_expression: $ => seq('(', $.expression, ')'),

    list_literal: $ => seq('[', optional($.argument_list), ']'),

    argument_list: $ => seq(
      $.expression,
      repeat(seq(',', $.expression))
    ),

    type_name: $ => choice(
      $.reference_type,
      $.function_type,
      $.type_path
    ),

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

    type_list: $ => seq(
      $.type_name,
      repeat(seq(',', $.type_name))
    ),

    reference_type: $ => seq('&', $.type_name),

    function_type: $ => seq(
      'func',
      '(',
      optional($.type_list),
      ')',
      'returns',
      $.type_name
    ),

    type_path: $ => prec.left(seq(
      $.identifier,
      repeat(seq('.', $.identifier)),
      optional($.type_args)
    )),

    type_args: $ => seq(
      '<',
      optional($.type_list),
      '>'
    ),

    type_list: $ => seq(
      $.type_name,
      repeat(seq(',', $.type_name))
    ),

    pattern: $ => choice(
      $.wildcard_pattern,
      $.literal_pattern,
      $.enum_variant_pattern,
      $.list_pattern,
      $.identifier_pattern
    ),

    wildcard_pattern: $ => $.underscore,

    literal_pattern: $ => choice($.boolean, $.keyword_constant, $.char_literal, $.string_literal, $.int_literal, $.float_literal),

    identifier_pattern: $ => $.identifier,

    enum_variant_pattern: $ => seq(
      field('name', $.identifier),
      optional(seq('(', optional($.pattern_list), ')'))
    ),

    list_pattern: $ => seq(
      '[',
      optional($.pattern_list),
      optional(seq('..', optional($.pattern))),
      ']'
    ),

    pattern_list: $ => seq(
      $.pattern,
      repeat(seq(',', $.pattern))
    ),

    module_path: $ => seq(
      $.identifier,
      repeat(seq('.', $.identifier))
    ),

    literal: $ => choice(
      $.int_literal,
      $.float_literal,
      $.boolean,
      $.char_literal,
      $.string_literal,
      $.triple_string_literal
    ),

    line_comment: $ => token(prec(1, seq('//', /.*/))),
    block_comment: $ => token(prec(1, seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'))),

    keyword_control: $ => token(prec(1, choice(
      'if', 'else', 'for', 'while', 'match', 'break', 'continue'
    ))),

    keyword_declaration: $ => token(prec(1, choice(
      'auto', 'func', 'returns', 'return', 'const', 'class', 'interface', 'enum', 'import'
    ))),

    keyword_operator: $ => token(prec(1, choice(
      'is', 'as', 'in'
    ))),

    keyword_constant: $ => token(prec(1, choice(
      'none', 'common'
    ))),

    boolean: $ => token(prec(1, choice('true', 'false'))),

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

    int_literal: $ => token(prec(1, seq(optional('-'), /[0-9][0-9_]*/))),
    float_literal: $ => token(prec(1, choice(
      seq(optional('-'), /[0-9][0-9_]*\.[0-9][0-9_]*/),
      seq(optional('-'), /[0-9][0-9_]*\.[0-9][0-9_]*[eE][-+]?[0-9][0-9_]*/),
      seq(optional('-'), /\.[0-9][0-9_]*/),
      seq(optional('-'), /\.[0-9][0-9_]*[eE][-+]?[0-9][0-9_]*/)
    ))),

    char_literal: $ => token(seq("'", choice(/\\./, /[^'\\]/), "'")),
    string_literal: $ => token(seq('"', repeat(choice(/\\./, /[^"\\]/)), '"')),
    triple_string_literal: $ => token(seq('"""', repeat(choice(/\\./, /[^"\\]/, '\n')), '"""')),

    underscore: $ => token('_'),
    delimiter: $ => token(choice('(', ')', '{', '}', '[', ']', ',', ':')),

    identifier: $ => token(seq(/[a-zA-Z]/, repeat(/[a-zA-Z0-9_]/))),
  }
});
