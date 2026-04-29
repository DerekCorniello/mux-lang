#!/bin/bash
# Test script for Tree-sitter Mux grammar

set -e

echo "=== Testing Tree-sitter Mux Grammar ==="
echo

echo "1. Testing parser generation..."
tree-sitter generate grammar.js
echo "   Parser generated successfully"
echo

echo "2. Testing parsing (pipe input)..."
echo 'func main() returns void { auto x = 42 }' | tree-sitter parse
echo

echo "3. Testing parsing (file input)..."
echo 'auto y = "hello"' > test_sample.mux
tree-sitter parse test_sample.mux
rm test_sample.mux
echo

echo "4. Testing corpus tests..."
tree-sitter test
echo

echo "5. Testing highlighting (if configured)..."
if [ -f "test.mux" ]; then
  tree-sitter highlight test.mux 2>&1 | head -20
else
  echo "   No test.mux file found, skipping highlight test"
fi
echo

echo "=== All tests completed ==="
