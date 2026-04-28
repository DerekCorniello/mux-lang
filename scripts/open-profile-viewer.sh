#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

"$repo_root/scripts/dev-cargo.sh" run --manifest-path "$repo_root/Cargo.toml" --bin mux-profile-viewer -p mux-profile-viewer -- "$@"
