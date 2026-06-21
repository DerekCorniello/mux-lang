#!/usr/bin/env bash
set -euo pipefail

if [[ $# -eq 0 ]]; then
  echo "Usage: scripts/dev-cargo.sh <cargo args...>"
  exit 1
fi

if command -v llvm-config-22 >/dev/null 2>&1; then
  llvm_config="$(command -v llvm-config-22)"
elif command -v llvm-config >/dev/null 2>&1; then
  llvm_config="$(command -v llvm-config)"
  llvm_major="$("$llvm_config" --version 2>/dev/null | cut -d. -f1)"
  if [[ "$llvm_major" != "22" ]]; then
    echo "Found llvm-config but it reports version $llvm_major, not 22."
    echo "Run scripts/bootstrap-dev.sh to install LLVM 22."
    exit 1
  fi
else
  echo "Could not find llvm-config-22. Run scripts/bootstrap-dev.sh first."
  exit 1
fi

llvm_prefix="$($llvm_config --prefix)"

export LLVM_CONFIG_PATH="$llvm_config"
export LLVM_SYS_221_PREFIX="$llvm_prefix"
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-target/dev-cargo}"

if command -v clang-22 >/dev/null 2>&1; then
  export CC="clang-22"
elif command -v clang >/dev/null 2>&1; then
  export CC="clang"
fi

if command -v clang++-22 >/dev/null 2>&1; then
  export CXX="clang++-22"
elif command -v clang++ >/dev/null 2>&1; then
  export CXX="clang++"
fi

exec cargo "$@"
