#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

input_file="${1:-$repo_root/test_scripts/test_std_http_server.mux}"
output_prefix="${2:-$repo_root/target/profile/runtime/test_std_http_server}"

mkdir -p "$(dirname "$output_prefix")"

MUX_RUNTIME_PROFILE_OUTPUT="$output_prefix" \
  "$repo_root/scripts/dev-cargo.sh" run -- "$input_file"

echo "Wrote runtime profile to ${output_prefix}.summary.json"
echo "Wrote Speedscope trace to ${output_prefix}.speedscope.json"
