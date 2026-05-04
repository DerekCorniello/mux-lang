# Mux Syntax Highlighting

First-class syntax highlighting support for the Mux programming language across editors and tools.

## Structure
- `textmate-mux/` - TextMate grammar (VSCode, Sublime, JetBrains, Nova)
  - `vscode-language-mux/` - VSCode extension package
- `tree-sitter-mux/` - Tree-sitter parser (Neovim, Helix, GitHub code intelligence)
  - `grammar.js` - Tree-sitter grammar
  - `tree-sitter.json` - ABI 15 config
  - `queries/` - Highlight queries
  - `corpus/` - Tests
- `shared/` - Canonical syntax data and cross-track artifacts
  - `syntax-matrix.json` - Single source of truth for syntax elements
  - `linguist/` - GitHub Linguist contribution artifacts
  - `samples/` - Validation samples for both tracks
- `scripts/` - Generates both grammars from `shared/syntax-matrix.json`

---

## TextMate Grammar Development

### Prerequisites
- Node.js and npm
- vsce: `npm install -g @vscode/vsce`

### Working with the Grammar
The canonical TextMate grammar is generated from `shared/syntax-matrix.json`.

Run `node scripts/generate-syntax.js` before packaging or testing editor output.

### Building and Testing (VSCode)
```bash
cd mux-syntax-highlighting
node scripts/generate-syntax.js
cd textmate-mux/vscode-language-mux
vsce package
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
See `textmate-mux/COMPATIBILITY.md` for Sublime Text, JetBrains, and Nova setup.

---

## Tree-sitter Grammar Development

### Prerequisites
- Node.js and npm
- tree-sitter CLI: `npm install -g tree-sitter-cli`

### Building and Testing
```bash
cd mux-syntax-highlighting/tree-sitter-mux

# Generate parser (ABI 15, requires tree-sitter.json)
tree-sitter generate grammar.js

# Run corpus tests (files in test/corpus/)
tree-sitter test

# Update test expectations with current parser output
tree-sitter test --update

# Parse a file and see the syntax tree (pipe input)
echo 'func main() returns void { auto x = 42 }' | tree-sitter parse

# Parse an actual file
tree-sitter parse /home/derekcorn/code/mux-lang/mux-syntax-highlighting/shared/samples/validation.mux
```

**Note:** The `tree-sitter highlight` command requires proper language registration in your config. For reliable highlighting, integrate with Neovim/Helix (see INTEGRATION.md).

### Highlight Queries
- `queries/highlights.scm` - Generated syntax highlighting queries
- Queries map tree-sitter nodes to TextMate-like scope names

### Editor Integration
See `tree-sitter-mux/INTEGRATION.md` for Neovim and Helix setup instructions.

---

## Validation
Test samples are in `shared/samples/`. Use:

1. Regenerate both grammars after editing `shared/syntax-matrix.json`:
   `node scripts/generate-syntax.js`
2. TextMate: package from `textmate-mux/vscode-language-mux/` and install the VSIX locally
3. Tree-sitter: `tree-sitter test` in `tree-sitter-mux/`
4. Parity: `node scripts/check-parity.js`

## GitHub Highlighting
Contribute to GitHub Linguist using artifacts in `shared/linguist/`. Status tracked in `shared/linguist/README.md`.

## Release Workflow
- `./scripts/release-syntax.sh` regenerates the syntax files, rebuilds Tree-sitter, and packages the VSCode extension.
- Generated editor outputs are intentionally not committed.
