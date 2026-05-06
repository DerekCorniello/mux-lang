# Mux Language Support for VSCode

Provides syntax highlighting and language configuration for Mux files (`.mux`) in Visual Studio Code.

## Features
- Syntax highlighting (keywords, strings, comments, operators, literals)
- Language configuration (bracket matching, auto-closing pairs, comment toggling)
- Mux language icon in the Extensions panel

## Development

### Prerequisites
- Node.js and npm
- vsce: `npm install -g @vscode/vsce`

The extension grammar is generated from `../../shared/syntax-matrix.json` via `../../scripts/generate-syntax.js`.

### Build and Package
```bash
cd mux-syntax-highlighting
../scripts/release-syntax.sh
```

### Install Locally
```bash
code --install-extension language-mux-0.2.1.vsix
code -r /path/to/mux-lang
```

### Test Changes
1. Edit `../../shared/syntax-matrix.json`
2. Run `node ../../scripts/generate-syntax.js`
3. Repackage with `vsce package`
4. Install the `.vsix` in VSCode
5. Reload window (Ctrl+Shift+P -> "Developer: Reload Window")

## File Structure
- `language-configuration.json` - Editor behavior (brackets, comments)
- `mux-icon.png` - Extension icon
- `package.json` - VSCode extension manifest

## Scope Names
The grammar uses standard TextMate scope names:
- `keyword.control.mux` - if, else, for, while, match, as, in, is
- `keyword.declaration.mux` - auto, func, returns, return, const, class, interface, enum, import
- `constant.language.mux` - none, true, false, common
- `string.quoted.*.mux` - Single and double-quoted strings
- `comment.*.mux` - Line and block comments

Colors are determined by the active VSCode theme, not the grammar itself.
