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

### Build and Package
```bash
cd mux-syntax-highlighting/textmate/vscode-language-mux
vsce package
```

### Install Locally
```bash
code --install-extension language-mux-0.2.1.vsix
code -r /path/to/mux-lang
```

### Test Changes
1. Edit `source.mux.tmLanguage` (XML TextMate grammar)
2. Repackage with `vsce package`
3. Reinstall the `.vsix` in VSCode
4. Reload window (Ctrl+Shift+P → "Developer: Reload Window")

## File Structure
- `source.mux.tmLanguage` - TextMate grammar (XML/PLIST format)
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
