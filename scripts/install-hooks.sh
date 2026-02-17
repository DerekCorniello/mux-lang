#!/bin/sh
set -e

if ! command -v git >/dev/null 2>&1; then
  echo "git is required to install hooks"
  exit 1
fi

repo_root=$(git rev-parse --show-toplevel 2>/dev/null || true)
if [ -z "$repo_root" ]; then
  echo "could not find git repository root"
  exit 1
fi

hooks_path="$repo_root/.github/hooks"
if [ ! -d "$hooks_path" ]; then
  echo "hooks directory not found at $hooks_path"
  exit 1
fi

if [ -f "$hooks_path/pre-commit" ]; then
  chmod +x "$hooks_path/pre-commit"
fi

git config core.hooksPath "$hooks_path"

echo "git hooks configured to use $hooks_path"
