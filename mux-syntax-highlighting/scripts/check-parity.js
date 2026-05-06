#!/usr/bin/env node

const fs = require('node:fs');
const path = require('node:path');

const root = path.resolve(__dirname, '..');
const { execFileSync } = require('node:child_process');

function readText(relPath) {
  return fs.readFileSync(path.join(root, relPath), 'utf8');
}

function readJson(relPath) {
  return JSON.parse(readText(relPath));
}

function sortValue(value) {
  if (Array.isArray(value)) {
    return value.map(sortValue);
  }
  if (value && typeof value === 'object') {
    return Object.keys(value)
      .sort((a, b) => a.localeCompare(b))
      .reduce((acc, key) => {
        if (key === '$schema') {
          return acc;
        }
        acc[key] = sortValue(value[key]);
        return acc;
      }, {});
  }
  return value;
}

function stableStringify(value) {
  return JSON.stringify(sortValue(value), null, 2);
}

function flattenNumbers(matrix) {
  return Object.values(matrix.operators).flatMap(items => items.map(item => item.symbol));
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function assertContainsAll(haystack, needles, label) {
  const missing = needles.filter(needle => !haystack.includes(needle));
  assert(missing.length === 0, `${label} missing: ${missing.join(', ')}`);
}

const matrix = readJson('shared/syntax-matrix.json');
execFileSync('node', [path.join(root, 'scripts', 'generate-syntax.js')], { stdio: 'inherit' });
execFileSync('tree-sitter', ['generate', 'grammar.js'], { cwd: path.join(root, 'tree-sitter-mux'), stdio: 'inherit' });
const generatedSyntax = require(path.join(root, 'tree-sitter-mux/generated/syntax.js'));
const textmateCanonical = readJson('textmate-mux/source.mux.json');
const textmatePackage = readJson('textmate-mux/vscode-language-mux/source.mux.json');
const treeSitterGrammar = readText('tree-sitter-mux/grammar.js');
const treeSitterQueries = readText('tree-sitter-mux/queries/highlights.scm');

assert(
  stableStringify(textmateCanonical) === stableStringify(textmatePackage),
  'TextMate canonical grammar and VSCode package grammar are out of sync',
);

assert(
  stableStringify(generatedSyntax) === stableStringify(matrix),
  'Tree-sitter generated syntax module is out of sync with shared syntax matrix',
);

const normalizedTextmate = readText('textmate-mux/source.mux.json').replaceAll('\\', '');
const normalizedTreeSitter = readText('tree-sitter-mux/src/grammar.json').replaceAll('\\', '');

const expectedKeywords = [
  ...matrix.keywords.control,
  ...matrix.keywords.declaration,
  ...matrix.keywords.operator,
  ...matrix.keywords.constant,
  ...matrix.keywords.boolean_literals,
];
const expectedOperators = flattenNumbers(matrix);
const expectedDelimiters = matrix.delimiters.map(item => item.symbol);

assertContainsAll(normalizedTextmate, expectedKeywords, 'TextMate keywords');
assertContainsAll(normalizedTextmate, expectedOperators, 'TextMate operators');
assertContainsAll(normalizedTextmate, expectedDelimiters, 'TextMate delimiters');

assertContainsAll(normalizedTreeSitter, expectedKeywords, 'Tree-sitter keywords');
assertContainsAll(normalizedTreeSitter, expectedOperators, 'Tree-sitter operators');
assertContainsAll(normalizedTreeSitter, expectedDelimiters, 'Tree-sitter delimiters');

assertContainsAll(
  normalizedTextmate,
  [
    'comment.line.double-slash.mux',
    'comment.block.mux',
    'string.quoted.single.mux',
    'string.quoted.double.mux',
    'string.quoted.triple.double.mux',
    'variable.language.mux',
    'variable.other.mux',
  ],
  'TextMate scopes',
);

assertContainsAll(
  treeSitterQueries,
  [
    '@comment',
    '@comment.line',
    '@comment.block',
    '@keyword',
    '@keyword.control',
    '@keyword.declaration',
    '@constant.language',
    '@number',
    '@string.quoted.single',
    '@string.quoted.double',
    '@string.quoted.triple.double',
    '@variable.language',
    '@variable.other',
    '@punctuation.bracket',
    '@function',
    '@function.call',
    '@function.declaration',
    '@type',
  ],
  'Tree-sitter highlight captures',
);

assert(
  !fs.existsSync(path.join(root, 'tree-sitter-mux/queries/mux/highlights.scm')),
  'Remove tree-sitter-mux/queries/mux/highlights.scm and keep only tree-sitter-mux/queries/highlights.scm',
);

console.log('Mux syntax parity checks passed.');
