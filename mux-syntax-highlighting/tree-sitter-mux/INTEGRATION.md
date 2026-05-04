# Tree-sitter Mux Integration

## Neovim

1. Edit `shared/syntax-matrix.json` if the vocabulary changes, then run `node scripts/generate-syntax.js` from `mux-syntax-highlighting/`
2. Clone this repository or copy `tree-sitter-mux/` to `~/.local/share/nvim/site/parser/`
3. Add to your Neovim config (`~/.config/nvim/init.lua`):
```lua
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.mux = {
  install_info = {
    url = "https://github.com/DerekCorniello/mux-lang",
    files = {"mux-syntax-highlighting/tree-sitter-mux/grammar.js"},
    branch = "main"
  },
  filetype = "mux",
}
```

4. Run `:TSInstall mux` in Neovim, then `:TSBufEnable highlight`

## Helix

1. Edit `shared/syntax-matrix.json` if the vocabulary changes, then run `node scripts/generate-syntax.js` from `mux-syntax-highlighting/`
2. Add the parser to Helix's runtime:
```bash
mkdir -p ~/.config/helix/runtime/grammars
cd mux-syntax-highlighting/tree-sitter-mux
tree-sitter generate grammar.js
cp mux.so ~/.config/helix/runtime/grammars/
```

3. Add to `~/.config/helix/languages.toml`:
```toml
[[language]]
name = "mux"
scope = "source.mux"
file-types = ["mux"]
roots = []
grammar = true

[language.highlight]
paths = ["queries/highlights.scm"]
```

## Validation
- `tree-sitter test` in `tree-sitter-mux/`
- `node ../scripts/check-parity.js` from `tree-sitter-mux/`
