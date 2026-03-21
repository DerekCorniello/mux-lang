#!/usr/bin/env bash
set -euo pipefail

if [[ $# -eq 0 ]]; then
  echo "Usage: scripts/dev-cargo.sh <cargo args...>"
  exit 1
fi

if command -v llvm-config-17 >/dev/null 2>&1; then
  llvm_config="$(command -v llvm-config-17)"
elif [[ -x /usr/lib/llvm17/bin/llvm-config ]]; then
  llvm_config="/usr/lib/llvm17/bin/llvm-config"
else
  echo "Could not find llvm-config-17. Run scripts/bootstrap-dev.sh first."
  exit 1
fi

llvm_prefix="$($llvm_config --prefix)"

export LLVM_CONFIG_PATH="$llvm_config"
export LLVM_SYS_170_PREFIX="$llvm_prefix"

if command -v clang-17 >/dev/null 2>&1; then
  export CC="clang-17"
fi

if command -v clang++-17 >/dev/null 2>&1; then
  export CXX="clang++-17"
fi

exec cargo "$@"
