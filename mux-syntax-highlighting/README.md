# Mux Syntax Highlighting

First-class syntax highlighting support for the Mux programming language across editors and tools.

## Structure
- `textmate/` - TextMate grammar (VSCode, Sublime, JetBrains, Nova)
  - `source.mux.json` - TextMate grammar (JSON format)
  - `source.mux.tmLanguage` - TextMate grammar (XML/PLIST format, better VSCode compatibility)
  - `vscode-language-mux/` - VSCode extension package
- `treesitter/` - Tree-sitter parser (Neovim, Helix, GitHub code intelligence)
  - `grammar.js` - Tree-sitter grammar
  - `queries/` - Highlight queries
  - `corpus/` - Tests
- `shared/` - Canonical syntax data and cross-track artifacts
  - `syntax-matrix.json` - Single source of truth for syntax elements
  - `linguist/` - GitHub Linguist contribution artifacts
  - `samples/` - Validation samples for both tracks

---

## TextMate Grammar Development

### Prerequisites
- Node.js and npm
- vsce: `npm install -g @vscode/vsce`

### Working with the Grammar
The TextMate grammar is available in two formats:
1. **JSON** (`textmate/source.mux.json`) - Easier to read/edit
2. **XML/PLIST** (`textmate/vscode-language-mux/source.mux.tmLanguage`) - Better VSCode compatibility

When editing, update both files to keep them in sync.

### Building and Testing (VSCode)
```bash
cd mux-syntax-highlighting/textmate/vscode-language-mux

# Edit source.mux.tmLanguage (XML grammar)

# Package the extension
vsce package

# Install locally
code --install-extension language-mux-0.2.1.vsix

# Reload VSCode
code -r /path/to/mux-lang
```

### Scope Names
The grammar uses standard TextMate scope names. Colors are determined by the active theme:
- `keyword.control.mux` - if, else, for, while, match, as, in, is
- `keyword.declaration.mux` - auto, func, returns, return, const, class, interface, enum, import
- `constant.language.mux` - none, true, false, common
- `keyword.operator.*.mux` - Assignment, arithmetic, comparison, logical operators
- `string.quoted.*.mux` - Single and double-quoted strings
- `constant.numeric.*.mux` - Integer and float literals
- `comment.*.mux` - Line and block comments
- `punctuation.mux` - Delimiters (parentheses, braces, brackets)
- `variable.other.mux` - Identifiers

### Editor Compatibility
See `textmate/COMPATIBILITY.md` for Sublime Text, JetBrains, and Nova setup.

---

## Tree-sitter Grammar Development

### Prerequisites
- Node.js and npm
- tree-sitter CLI: `npm install -g tree-sitter-cli`

### Building and Testing
```bash
cd mux-syntax-highlighting/treesitter

# Generate parser
tree-sitter generate grammar.js

# Run corpus tests
tree-sitter test

# Parse a file
echo 'auto x = 42' | tree-sitter parse
```

### Highlight Queries
- `queries/highlights.scm` - Syntax highlighting queries
- Queries map tree-sitter nodes to TextMate-like scope names

### Editor Integration
See `treesitter/INTEGRATION.md` for Neovim and Helix setup instructions.

---

## Validation
Test samples are in `shared/linguist/sample.mux` and `shared/samples/`. Use:
- TextMate: Paste sample code into VSCode with extension installed
- Tree-sitter: `tree-sitter test` in `treesitter/` directory

## GitHub Highlighting
Contribute to GitHub Linguist using artifacts in `shared/linguist/`. Status tracked in `shared/linguist/README.md`.
