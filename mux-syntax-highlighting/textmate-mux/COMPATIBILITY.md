# TextMate Grammar Compatibility

The TextMate grammar is generated from `shared/syntax-matrix.json`.
Run `./scripts/release-syntax.sh` or `node scripts/generate-syntax.js` before packaging.

## Sublime Text
1. Copy the generated TextMate grammar into `Packages/User/Mux/`
2. Add to `Packages/User/Package.sublime-settings`:
```json
{
  "syntax": [
    {
      "name": "Mux",
      "scope": "source.mux",
      "file_extensions": [".mux"],
       "path": "User/Mux/source.mux.json"
    }
  ]
}
```

## JetBrains (IntelliJ, WebStorm, etc.)
1. Install the "TextMate Bundles" plugin
2. Import the generated TextMate grammar as a custom grammar
3. Associate `.mux` files with the Mux grammar in Settings > Editor > File Types

## Nova (Panic)
1. Create a new extension in Nova
2. Add the generated TextMate grammar as the syntax grammar
3. Set file extension to `.mux` in extension manifest

## Notes
- Keep `shared/syntax-matrix.json` as the source of truth for TextMate updates.
- Rebuild the VSCode package after grammar edits so the packaged copy stays in sync.
