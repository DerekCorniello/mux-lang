#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

input_arg="${1:-}"
output_arg="${2:-}"

if [[ "$input_arg" == "-h" || "$input_arg" == "--help" ]]; then
  printf 'Usage: %s [input.mux] [output-prefix]\n' "${BASH_SOURCE[0]##*/}"
  exit 0
fi

input_file="${input_arg:-$repo_root/test_scripts/test_std_http_server.mux}"
input_name="${input_file##*/}"
input_name="${input_name%.*}"
output_prefix="${output_arg:-$repo_root/target/profile/runtime/$input_name}"

mkdir -p "$(dirname "$output_prefix")"

MUX_RUNTIME_PROFILE_OUTPUT="$output_prefix" \
  "$repo_root/scripts/dev-cargo.sh" run -- run "$input_file"

echo "Wrote runtime profile to ${output_prefix}.summary.json"
echo "Wrote Speedscope trace to ${output_prefix}.speedscope.json"
"$repo_root/scripts/open-profile-viewer.sh" "${output_prefix}.summary.json"
