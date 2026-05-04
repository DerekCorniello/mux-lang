const syntax = require('../shared/syntax-matrix.json');

const choice_of = values => choice(...values);
const token_of = values => (
  values.length === 1
    ? token(prec(1, values[0]))
    : token(prec(1, choice(...values)))
);
const regex_of = pattern => new RegExp(pattern);

const declaration_keywords = syntax.keywords.declaration.filter(keyword => keyword !== 'common');

module.exports = grammar({
  name: 'mux',

  conflicts: $ => [
    [$.primary_expression, $.type_path],
    [$.source_file, $.expression_statement],
    [$.block, $.expression_statement],
    [$.identifier_pattern, $.enum_variant_pattern],
    [$.if_statement, $.guard_clause],
    [$.match_arm, $.statement],
    [$.primary_expression, $.call_target],
    [$.primary_expression, $.call_target, $.type_path],
    [$.primary_expression, $.member_object],
    [$.primary_expression, $.member_object, $.type_path],
    [$.member_object, $.type_path],
    [$.member_object, $.reference_type],
    [$.postfix_expression, $.member_object],
    [$.postfix_expression, $.call_target],
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.declaration,
      $.statement,
      $.expression,
      $.line_comment,
      $.block_comment
    )),

    declaration: $ => choice(
      $.function_declaration,
      $.interface_method_declaration,
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
        $.block_comment
      )),
      '}'
    ),

    function_declaration: $ => prec.right(seq(
      optional('common'),
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

    interface_method_declaration: $ => prec.right(seq(
      optional('common'),
      'func',
      field('name', $.identifier),
      optional($.type_params),
      '(',
      optional($.param_list),
      ')',
      'returns',
      field('return_type', $.type_name)
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
    interface_body: $ => seq('{', repeat(choice($.field_declaration, $.interface_method_declaration, $.line_comment, $.block_comment)), '}'),
    enum_body: $ => seq(
      '{',
      optional(seq(
        $.enum_variant,
        repeat(seq(',', $.enum_variant)),
        optional(',')
      )),
      repeat(choice($.line_comment, $.block_comment)),
      '}'
    ),

    field_declaration: $ => seq(
      optional('const'),
      field('type', $.type_name),
      field('name', $.identifier),
      optional(seq('=', $.expression))
    ),

    param_list: $ => seq(
      $.parameter,
      repeat(seq(',', $.parameter))
    ),

    parameter: $ => seq(
      field('type', $.type_name),
      field('name', choice($.identifier, $.underscore)),
      optional(seq('=', field('default', $.expression)))
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
      field('operator', choice_of(syntax.operators.assignment.map(op => op.symbol))),
      field('right', $.expression)
    )),

    logic_or_expression: $ => prec.left(seq(
      $.logic_and_expression,
      repeat(seq(syntax.operators.logical.find(op => op.symbol === '||').symbol, $.logic_and_expression))
    )),

    logic_and_expression: $ => prec.left(seq(
      $.equality_expression,
      repeat(seq(syntax.operators.logical.find(op => op.symbol === '&&').symbol, $.equality_expression))
    )),

    equality_expression: $ => prec.left(seq(
      $.comparison_expression,
      repeat(seq(choice_of(syntax.operators.comparison.filter(op => op.symbol === '==' || op.symbol === '!=').map(op => op.symbol)), $.comparison_expression))
    )),

    comparison_expression: $ => prec.left(seq(
      $.range_expression,
      repeat(seq(choice_of(syntax.operators.comparison.map(op => op.symbol)), $.range_expression))
    )),

    range_expression: $ => prec.left(seq(
      $.sum_expression,
      repeat(seq('..', $.sum_expression))
    )),

    sum_expression: $ => prec.left(seq(
      $.product_expression,
      repeat(seq(choice_of(syntax.operators.arithmetic.filter(op => op.symbol === '+' || op.symbol === '-').map(op => op.symbol)), $.product_expression))
    )),

    product_expression: $ => prec.left(seq(
      $.power_expression,
      repeat(seq(choice_of(syntax.operators.arithmetic.filter(op => op.symbol === '*' || op.symbol === '/' || op.symbol === '%').map(op => op.symbol)), $.power_expression))
    )),

    power_expression: $ => prec.right(choice(
      seq($.unary_expression, syntax.operators.arithmetic.find(op => op.symbol === '**').symbol, $.power_expression),
      $.unary_expression
    )),

    unary_expression: $ => prec.right(choice(
      seq(choice_of([
        syntax.operators.logical.find(op => op.symbol === '!').symbol,
        syntax.operators.arithmetic.find(op => op.symbol === '-').symbol,
        syntax.operators.other.find(op => op.symbol === '&').symbol,
        syntax.operators.arithmetic.find(op => op.symbol === '*').symbol,
        syntax.operators.arithmetic.find(op => op.symbol === '++').symbol,
        syntax.operators.arithmetic.find(op => op.symbol === '--').symbol,
      ]), $.unary_expression),
      $.postfix_expression
    )),

    postfix_expression: $ => choice(
      $.call_expression,
      $.field_access,
      $.index_access,
      $.primary_expression
    ),

    call_expression: $ => prec.left(seq(
      field('function', $.call_target),
      optional($.type_args),
      '(',
      optional($.argument_list),
      ')'
    )),

    field_access: $ => prec.left(seq(
      field('object', $.member_object),
      '.',
      field('field', $.identifier)
    )),

    index_access: $ => prec.left(seq(
      field('object', $.member_object),
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

    call_target: $ => choice(
      $.identifier,
      $.grouped_expression,
      $.field_access,
      $.index_access,
      $.call_expression,
      $.lambda_expression
    ),

    member_object: $ => choice(
      $.identifier,
      $.grouped_expression,
      $.call_expression,
      $.field_access,
      $.index_access,
      $.list_literal,
      $.type_name
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

    type_param: $ => seq(
      $.identifier,
      optional($.type_bound_clause)
    ),

    type_bound_clause: $ => seq(
      'is',
      $.type_path,
      repeat(seq('&', $.type_path))
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

    reference_type: $ => prec.right(seq('&', $.type_name)),

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

    line_comment: $ => token(prec(1, regex_of(syntax.comments.line.pattern))),
    block_comment: $ => token(prec(1, regex_of(syntax.comments.block.pattern))),

    keyword_constant: $ => token_of(syntax.keywords.constant),

    boolean: $ => token_of(syntax.keywords.boolean_literals),

    int_literal: $ => token(prec(1, regex_of(syntax.literals.integer.pattern))),
    float_literal: $ => token(prec(1, regex_of(syntax.literals.float.pattern))),

    char_literal: $ => token(regex_of(syntax.literals.char.pattern)),
    string_literal: $ => token(regex_of(syntax.literals.string.single_line.pattern)),
    triple_string_literal: $ => token(regex_of(syntax.literals.string.multi_line.pattern)),

    underscore: $ => token(syntax.identifiers.underscore),

    identifier: $ => token(regex_of(syntax.identifiers.pattern)),
  }
});
