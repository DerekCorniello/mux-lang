#!/usr/bin/env node

const fs = require('node:fs');
const path = require('node:path');

const root = path.resolve(__dirname, '..');
const matrix = JSON.parse(fs.readFileSync(path.join(root, 'shared/syntax-matrix.json'), 'utf8'));

function writeText(relPath, text) {
  fs.writeFileSync(path.join(root, relPath), `${text.endsWith('\n') ? text : `${text}\n`}`);
}

function escapeRegex(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function wordBoundaryRegex(words) {
  return `\\b(?:${words.map(escapeRegex).join('|')})\\b`;
}

function charClassRegex(symbols) {
  return `[${symbols.map(symbol => escapeRegex(symbol)).join('')}]`;
}

function generatedSyntaxModule() {
  return `// Generated from ../shared/syntax-matrix.json
module.exports = ${JSON.stringify(matrix, null, 2)};
`;
}

function textmateGrammar() {
  const builtinTypes = matrix.types.builtin;
  const control = matrix.keywords.control;
  const declaration = matrix.keywords.declaration;
  const operator = matrix.keywords.operator;
  const booleanLiterals = matrix.keywords.boolean_literals;
  const delimiterSymbols = matrix.delimiters.map(item => item.symbol);
  const identifierPattern = matrix.identifiers.pattern;

  return JSON.stringify({
    name: 'Mux',
    scopeName: 'source.mux',
    fileTypes: ['.mux'],
    patterns: [
      { include: '#comments' },
      { include: '#literals' },
      { include: '#declarations' },
      { include: '#calls' },
      { include: '#types' },
      { include: '#keywords' },
      { include: '#operators' },
      { include: '#delimiters' },
      { include: '#identifiers' },
    ],
    repository: {
      comments: {
        patterns: [
          {
            name: 'comment.line.double-slash.mux',
            match: matrix.comments.line.pattern,
          },
          {
            name: 'comment.block.mux',
            begin: '/\\*',
            end: '\\*/',
            patterns: [
              {
                name: 'comment.block.mux',
                match: '/\\*.*?\\*/',
              },
            ],
          },
        ],
      },
      keywords: {
        patterns: [
          {
            name: 'keyword.control.mux',
            match: wordBoundaryRegex(control),
          },
          {
            name: 'keyword.declaration.mux',
            match: wordBoundaryRegex(declaration),
          },
          {
            name: 'storage.type.mux',
            match: '\\bauto\\b',
          },
          {
            name: 'keyword.operator.mux',
            match: wordBoundaryRegex(operator),
          },
          {
            name: 'constant.language.mux',
            match: wordBoundaryRegex(['none', ...booleanLiterals]),
          },
        ],
      },
      operators: {
        patterns: [
          {
            name: 'keyword.operator.arithmetic.mux',
            match: '(\\+\\+|--|\\*\\*|/|%|\\+|\\-|\\*)',
          },
          {
            name: 'keyword.operator.assignment.mux',
            match: '(\\+=|-=|\\*=|/=|%=|=)',
          },
          {
            name: 'keyword.operator.comparison.mux',
            match: '(==|!=|<=|>=|<|>)',
          },
          {
            name: 'keyword.operator.logical.mux',
            match: '(&&|\\|\\|)',
          },
          {
            name: 'keyword.operator.mux',
            match: '(!|&|\\.\\.|\\.)',
          },
        ],
      },
      literals: {
        patterns: [
          {
            name: 'constant.numeric.integer.mux',
            match: matrix.literals.integer.pattern,
          },
          {
            name: 'constant.numeric.float.mux',
            match: matrix.literals.float.pattern,
          },
          {
            name: 'string.quoted.single.mux',
            begin: "'",
            end: "'",
            patterns: [
              {
                name: 'constant.character.escape.mux',
                match: '\\\\([nrt0\\\\\'" ])',
              },
            ],
          },
          {
            name: 'string.quoted.double.mux',
            begin: '"',
            end: '"',
            patterns: [
              {
                name: 'constant.character.escape.mux',
                match: '\\\\([nrt0\\\\\'" ])',
              },
            ],
          },
          {
            name: 'string.quoted.triple.double.mux',
            begin: '"""',
            end: '"""',
            patterns: [
              {
                name: 'constant.character.escape.mux',
                match: '\\\\([nrt0\\\\\'" ])',
              },
            ],
          },
          {
            name: 'variable.language.mux',
            match: `\\b${escapeRegex(matrix.identifiers.underscore)}\\b`,
          },
        ],
      },
      delimiters: {
        patterns: [
          {
            name: 'punctuation.mux',
            match: charClassRegex(delimiterSymbols),
          },
        ],
      },
      declarations: {
        patterns: [
          {
            match: `\\b(func)\\s+(${identifierPattern})\\b`,
            captures: {
              1: { name: 'keyword.declaration.mux' },
              2: { name: 'entity.name.function.mux' },
            },
          },
          {
            match: `\\b(class|interface|enum)\\s+(${identifierPattern})\\b`,
            captures: {
              1: { name: 'keyword.declaration.mux' },
              2: { name: 'entity.name.type.mux' },
            },
          },
        ],
      },
      calls: {
        patterns: [
          {
            name: 'support.function.mux',
            match: `\\b${identifierPattern}\\b\\s*(?=(?:<[^()<>\\n]+>\\s*)?\\()`,
          },
          {
            match: `\\b(${identifierPattern})(\\s*\\.\\s*)(${identifierPattern})\\b\\s*(?=(?:<[^()<>\\n]+>\\s*)?\\()`,
            captures: {
              1: { name: 'variable.other.mux' },
              2: { name: 'punctuation.delimiter.mux' },
              3: { name: 'support.function.mux' },
            },
          },
        ],
      },
      types: {
        patterns: [
          {
            match: `\\b(returns)\\s+(${identifierPattern}(?:<[^>\\n]+>)?)`,
            captures: {
              1: { name: 'keyword.declaration.mux' },
              2: { name: 'storage.type.mux' },
            },
          },
          {
            match: `\\b(const)\\s+(${identifierPattern}(?:<[^>\\n]+>)?)\\s+(${identifierPattern})\\b`,
            captures: {
              1: { name: 'keyword.declaration.mux' },
              2: { name: 'storage.type.mux' },
              3: { name: 'variable.other.mux' },
            },
          },
          {
            name: 'storage.type.mux',
            match: `\\b(?:${builtinTypes.map(escapeRegex).join('|')}|[A-Z][a-zA-Z0-9_]*)\\b`,
          },
        ],
      },
      identifiers: {
        patterns: [
          {
            name: 'variable.other.mux',
            match: `\\b${identifierPattern}\\b`,
          },
        ],
      },
    },
  }, null, 2);
}

function treeSitterHighlights() {
  const builtinTypes = matrix.types.builtin.map(escapeRegex).join('|');
  return [
    ';; Comments',
    '(line_comment) @comment @comment.line',
    '(block_comment) @comment @comment.block',
    '',
    ';; Keywords',
    '(function_declaration "func" @keyword @keyword.declaration)',
    '(function_declaration "returns" @keyword @keyword.declaration)',
    '(function_declaration "common" @keyword @keyword.declaration)',
    '(interface_method_declaration "func" @keyword @keyword.declaration)',
    '(interface_method_declaration "returns" @keyword @keyword.declaration)',
    '(interface_method_declaration "common" @keyword @keyword.declaration)',
    '(lambda_expression "func" @keyword @keyword.declaration)',
    '(lambda_expression "returns" @keyword @keyword.declaration)',
    '(auto_declaration "auto" @type)',
    '(return_statement "return" @keyword @keyword.declaration)',
    '(class_declaration "class" @keyword @keyword.declaration)',
    '(interface_declaration "interface" @keyword @keyword.declaration)',
    '(enum_declaration "enum" @keyword @keyword.declaration)',
    '(import_statement "import" @keyword @keyword.declaration)',
    '(const_declaration "const" @keyword @keyword.declaration)',
    '(if_statement "if" @keyword @keyword.control)',
    '(else_clause "else" @keyword @keyword.control)',
    '(while_statement "while" @keyword @keyword.control)',
    '(for_statement "for" @keyword @keyword.control)',
    '(for_statement "in" @keyword @keyword.operator)',
    '(match_statement "match" @keyword @keyword.control)',
    '(guard_clause "if" @keyword @keyword.control)',
    '(break_statement) @keyword @keyword.control',
    '(continue_statement) @keyword @keyword.control',
    '(trait_clause "is" @keyword @keyword.operator)',
    '(import_alias "as" @keyword @keyword.operator)',
    '(keyword_constant) @constant @constant.language',
    '',
    ';; Numbers',
    '(int_literal) @number @constant.numeric.integer',
    '(float_literal) @number @constant.numeric.float',
    '',
    ';; Booleans',
    '(boolean) @constant @constant.language',
    '',
    ';; Literals',
    '(char_literal) @string @string.quoted.single',
    '(string_literal) @string @string.quoted.double',
    '(triple_string_literal) @string @string.quoted.triple.double',
    '(underscore) @variable @variable.language',
    '',
    ';; Delimiters',
    '["(" ")" "{" "}" "[" "]" ","] @punctuation.bracket',
    '',
    ';; Member access',
    '(field_access field: (identifier) @property)',
    '(type_path (identifier) "." (identifier) @property)',
    '',
    ';; Structured names',
    '(class_declaration name: (identifier) @type)',
    '(interface_declaration name: (identifier) @type)',
    '(enum_declaration name: (identifier) @type)',
    '(function_declaration name: (identifier) @function @function.declaration)',
    '(interface_method_declaration name: (identifier) @function @function.declaration)',
    '(call_expression function: (call_target (identifier) @function.call))',
    '(parameter type: (type_name) @type)',
    '(field_declaration type: (type_name) @type)',
    '(typed_declaration type: (type_name) @type)',
    '(const_declaration type: (type_name) @type)',
    '(function_declaration return_type: (type_name) @type)',
    '(interface_method_declaration return_type: (type_name) @type)',
    '(trait_clause (type_path) @type)',
    '(type_bound_clause (type_path) @type)',
    '(enum_variant name: (identifier) @constructor)',
    '',
    '((identifier) @type',
    ` (#match? @type "^(${builtinTypes})$"))`,
    '',
    '((identifier) @variable.builtin',
    ' (#eq? @variable.builtin "self"))',
    '',
  ].join('\n');
}

writeText('tree-sitter-mux/generated/syntax.js', generatedSyntaxModule());
writeText('textmate-mux/source.mux.json', textmateGrammar());
writeText('tree-sitter-mux/queries/highlights.scm', treeSitterHighlights());
