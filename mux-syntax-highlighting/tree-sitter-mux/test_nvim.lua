-- Test Tree-sitter highlighting for Mux in Neovim
-- Run with: nvim --headless -c "set filetype=mux" -c "TSBufEnable highlight" -c "redir! > /tmp/ts-highlights.txt" -c "echo 'test'" -c "q" test.mux

-- This script checks if Tree-sitter is highlighting correctly
local buf = vim.api.nvim_get_current_buf()
local parser = vim.treesitter.get_parser(buf)

if parser then
  print("Parser loaded successfully: " .. parser:lang())
  local tree = parser:parse()
  if tree and tree[1] then
    print("Parse tree root: " .. tree[1]:root():type())
  end
else
  print("Failed to load parser")
end
