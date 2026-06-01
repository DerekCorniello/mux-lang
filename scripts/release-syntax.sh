#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if [[ -n "${1:-}" ]]; then
  echo "Usage: $0"
  exit 1
fi

VERSION="$(tr -d '[:space:]' < "$REPO_ROOT/VERSION")"

if [[ -z "$VERSION" ]]; then
  echo "VERSION file is empty"
  exit 1
fi

cd "$REPO_ROOT"
./scripts/sync-version.sh
pushd mux-syntax-highlighting >/dev/null
node scripts/generate-syntax.js
popd >/dev/null
pushd mux-syntax-highlighting/tree-sitter-mux >/dev/null
tree-sitter generate grammar.js
tree-sitter build
sha256sum mux.so > mux.so.sha256
popd >/dev/null
pushd mux-syntax-highlighting/textmate-mux/vscode-language-mux >/dev/null
npx --yes --ignore-scripts @vscode/vsce@3.9.1 package
sha256sum language-mux-*.vsix > language-mux.vsix.sha256
popd >/dev/null

echo "Prepared syntax release ${VERSION}"
