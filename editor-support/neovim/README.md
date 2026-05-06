# Neovim Integration

Mux highlighting in Neovim uses the generated Tree-sitter query at:

- `editor-support/neovim/queries/mux/highlights.scm`

To wire this into `nvim-treesitter`, point the `mux` parser to your
`tree-sitter-mux` grammar and copy the `queries/mux` directory into your runtime
path.

The query file is generated. Do not edit it directly.
