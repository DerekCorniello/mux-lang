# TextMate Grammar Compatibility

The `source.mux.json` TextMate grammar works with all editors supporting TextMate grammars:

## Sublime Text
1. Copy `source.mux.json` to `Packages/User/Mux/`
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
1. Install "TextMate Bundles" plugin
2. Import `source.mux.json` as a custom grammar
3. Associate `.mux` files with the Mux grammar in Settings > Editor > File Types

## Nova (Panic)
1. Create a new extension in Nova
2. Add `source.mux.json` as the syntax grammar
3. Set file extension to `.mux` in extension manifest
