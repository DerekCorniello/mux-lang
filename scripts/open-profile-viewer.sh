#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

if [[ ${1:-} == "-h" || ${1:-} == "--help" ]]; then
  printf 'Usage: %s <summary.json>\n' "${BASH_SOURCE[0]##*/}"
  exit 0
fi

"$repo_root/scripts/dev-cargo.sh" run -p mux-profiling --features mux-profiling/viewer --bin mux-profile-viewer -- "$@"
