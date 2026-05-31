import type * as Monaco from 'monaco-editor';

export const conf: Monaco.languages.LanguageConfiguration = {
  comments: {
    lineComment: '//',
    blockComment: ['/*', '*/'],
  },
  brackets: [
    ['{', '}'],
    ['[', ']'],
    ['(', ')'],
  ],
  autoClosingPairs: [
    { open: '{', close: '}' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
    { open: '"', close: '"' },
    { open: "'", close: "'" },
  ],
  surroundingPairs: [
    { open: '{', close: '}' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
    { open: '"', close: '"' },
    { open: "'", close: "'" },
  ],
  indentationRules: {
    increaseIndentPattern: /^\s*(func|class|interface|enum|if|else|for|while|match)\b.*\{[^}]*$/,
    decreaseIndentPattern: /^\s*\}.*$/,
  },
};

export const language: Monaco.languages.IMonarchLanguage = {
  defaultToken: '',
  tokenPostfix: '.mux',
  ignoreCase: false,
  keywords: [
    'auto', 'func', 'returns', 'return', 'const', 'class', 'interface', 'enum',
    'import', 'common', 'if', 'else', 'for', 'while', 'match', 'break',
    'continue', 'is', 'as', 'in', 'ref', 'true', 'false', 'none', 'ok', 'err', 'some',
  ],
  typeKeywords: [
    'string', 'bool', 'void', 'int', 'float', 'char', 'optional', 'result',
    'list', 'map', 'tuple', 'set', 'range', 'Stringable', 'Hashable',
    'Thread', 'Error', 'Self',
  ],
  builtinTypes: [
    'int', 'float', 'string', 'bool', 'void', 'char', 'list', 'map', 'set',
    'optional', 'result', 'range', 'tuple',
  ],
  operators: [
    '=', '>', '<', '!', '~', '?', ':',
    '==', '<=', '>=', '!=', '&&', '||', '++', '--',
    '+', '-', '*', '/', '%', '**',
    '+=', '-=', '*=', '/=', '%=',
    '&', '..',
  ],
  symbols: /[=><!~?:&|+*/^%-]+/,
  escapes: /\\(?:[nrt0\\'"])/,
  tokenizer: {
    root: [
      [/[a-zA-Z_]\w*/, {
        cases: {
          '@keywords': 'keyword',
          '@typeKeywords': 'type',
          '@builtinTypes': 'type',
          '@default': 'identifier',
        },
      }],
      { include: '@whitespace' },
      [/[{}()[\]]/, '@brackets'],
      [/@symbols/, {
        cases: {
          '@operators': 'operator',
          '@default': '',
        },
      }],
      [/\d[\d_]*/, 'number.integer'],
      [/\d\.\d*([eE][-+]?\d+)?/, 'number.float'],
      [/\.\d+([eE][-+]?\d+)?/, 'number.float'],
      [/"/, 'string', '@string_double'],
      [/'/, 'string', '@string_single'],
    ],
    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/\/\*/, 'comment', '@comment'],
      [/\/\/.*$/, 'comment'],
    ],
    comment: [
      [/[^/*]+/, 'comment'],
      [/\/\*/, 'comment', '@push'],
      ['\\*/', 'comment', '@pop'],
      [/[/*]/, 'comment'],
    ],
    string_double: [
      [/[^\\"]+/, 'string'],
      [/@escapes/, 'string.escape'],
      [/\\./, 'string.escape.invalid'],
      [/"/, 'string', '@pop'],
    ],
    string_single: [
      [/[^\\']+/, 'string'],
      [/@escapes/, 'string.escape'],
      [/\\./, 'string.escape.invalid'],
      [/'/, 'string', '@pop'],
    ],
  },
};

export function registerMuxLanguage(monaco: typeof Monaco): void {
  monaco.languages.register({ id: 'mux', extensions: ['.mux'] });
  monaco.languages.setMonarchTokensProvider('mux', language);
  monaco.languages.setLanguageConfiguration('mux', conf);
}