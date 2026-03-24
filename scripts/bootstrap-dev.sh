#!/usr/bin/env bash
set -euo pipefail

if [[ "${EUID:-$(id -u)}" -eq 0 ]]; then
  sudo_cmd=""
else
  sudo_cmd="sudo"
fi

llvm17_ready() {
  if ! command -v llvm-config-17 >/dev/null 2>&1; then
    return 1
  fi

  local version major
  version="$(llvm-config-17 --version 2>/dev/null || true)"
  major="${version%%.*}"
  [[ "$major" == "17" ]]
}

if [[ -f /etc/arch-release ]]; then
  if llvm17_ready; then
    echo "LLVM 17 is already installed on Arch Linux"
  else
    if ! command -v yay >/dev/null 2>&1; then
      echo "LLVM 17 packages were not found in pacman repositories."
      echo "Install yay, then rerun this script."
      echo "Example: sudo pacman -S --needed base-devel git"
      echo "Then install yay from AUR and rerun scripts/bootstrap-dev.sh"
      exit 1
    fi

    echo "Installing LLVM 17 toolchain for Arch Linux via yay"
    yay -S --needed llvm17 llvm17-libs clang17 lld17

    if ! llvm17_ready; then
      echo "LLVM 17 installation did not complete successfully."
      echo "Expected llvm-config-17 version 17.x after install."
      exit 1
    fi
  fi
elif [[ -f /etc/debian_version ]]; then
  echo "Installing LLVM 17 toolchain for Debian/Ubuntu"
  $sudo_cmd apt-get update
  $sudo_cmd apt-get install -y llvm-17 llvm-17-dev clang-17 lld-17 libpolly-17-dev
elif [[ "$(uname -s)" == "Darwin" ]]; then
  echo "Installing LLVM 17 toolchain with Homebrew"
  brew install llvm@17
else
  echo "Unsupported OS for automatic setup. Install LLVM 17 and clang 17 manually."
  exit 1
fi

echo
echo "Installed contributor dependencies."
echo "Use scripts/dev-cargo.sh for source builds without manual env vars."
echo "Example: scripts/dev-cargo.sh build"
