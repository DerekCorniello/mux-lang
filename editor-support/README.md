# Mux Editor Syntax Highlighting

This directory contains generated syntax-highlighting artifacts for multiple editors.

Mux uses two highlighting paths:

1. TextMate path for VSCode, Sublime Text, and JetBrains TextMate bundles.
2. Tree-sitter query path for Neovim and Helix.

Both paths are generated from a single source of truth at
`editor-support/spec/definitions.json` so keyword, literal, type, operator, and
punctuation treatment stays consistent between editors.

## Build

```bash
python3 scripts/build_syntax_highlighting.py
```

## Verify generated output is up to date

```bash
python3 scripts/build_syntax_highlighting.py --check
```

## Generated targets

- `editor-support/textmate/mux.tmLanguage.json`
- `editor-support/vscode/syntaxes/mux.tmLanguage.json`
- `editor-support/vscode/language-configuration.json`
- `editor-support/jetbrains/textmate/mux.tmLanguage.json`
- `editor-support/sublime/Mux.sublime-syntax`
- `editor-support/treesitter/queries/highlights.scm`
- `editor-support/neovim/queries/mux/highlights.scm`
- `editor-support/helix/runtime/queries/mux/highlights.scm`

## Editor packaging notes

- VSCode package scaffold: `editor-support/vscode/package.json`
- Helix language registration example: `editor-support/helix/languages.toml`
- Neovim integration notes: `editor-support/neovim/README.md`

Neovim and Helix require a `tree-sitter-mux` parser to use the generated query
files.

Do not hand-edit generated files. Update `editor-support/spec/definitions.json`
and rebuild.
