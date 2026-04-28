#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

resolve_path() {
  local path="$1"
  if [[ "$path" = /* ]]; then
    printf '%s\n' "$path"
  else
    printf '%s/%s\n' "$repo_root" "$path"
  fi
}

report_path="$(resolve_path "${1:-target/perf-baseline.json}")"
mkdir -p "$(dirname "$report_path")"

names=()
durations=()

cargo_cmd=(cargo)
if [[ -x "$repo_root/scripts/dev-cargo.sh" ]] && [[ -z "${LLVM_CONFIG_PATH:-}" ]] && [[ -z "${LLVM_SYS_170_PREFIX:-}" ]]; then
  cargo_cmd=("$repo_root/scripts/dev-cargo.sh")
fi

measure() {
  local name="$1"
  shift
  local start end duration

  start="$(date +%s)"
  echo
  echo ">>> measuring ${name}"
  "$@"
  end="$(date +%s)"
  duration="$((end - start))"
  echo "<<< ${name}: ${duration}s"

  names+=("$name")
  durations+=("$duration")
}

measure "cargo build" "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" build
measure "cargo test --test executable_integration -- --nocapture" \
  "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" test --test executable_integration -- --nocapture
measure "cargo run -- run test_scripts/test_std_http_server.mux" \
  "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" run -- run test_scripts/test_std_http_server.mux
measure "cargo run -- run test_scripts/test_std_sql_sqlite.mux" \
  "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" run -- run test_scripts/test_std_sql_sqlite.mux

{
  printf '{\n'
  printf '  "generated_by": "scripts/measure-baseline.sh",\n'
  printf '  "steps": [\n'
  for i in "${!names[@]}"; do
    comma=","
    if [[ "$i" -eq $((${#names[@]} - 1)) ]]; then
      comma=""
    fi
    printf '    {"name":"%s","duration_seconds":%s}%s\n' \
      "${names[$i]}" "${durations[$i]}" "$comma"
  done
  printf '  ]\n'
  printf '}\n'
} > "$report_path"

echo
cat "$report_path"
