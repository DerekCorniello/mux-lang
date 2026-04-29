# Tree-sitter Mux Integration

## Neovim

1. Clone this repository or copy `tree-sitter-mux/` to `~/.local/share/nvim/site/parser/`
2. Add to your Neovim config (`~/.config/nvim/init.lua`):
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

3. Run `:TSInstall mux` in Neovim, then `:TSBufEnable highlight`

## Helix

1. Add the parser to Helix's runtime:
```bash
mkdir -p ~/.config/helix/runtime/grammars
tree-sitter generate mux-syntax-highlighting/tree-sitter-mux/grammar.js
cp mux.so ~/.config/helix/runtime/grammars/
```

2. Add to `~/.config/helix/languages.toml`:
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
