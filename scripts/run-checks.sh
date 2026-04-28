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

with_service_tests=0
report_path=""

cargo_cmd=(cargo)
if [[ -x "$repo_root/scripts/dev-cargo.sh" ]] && [[ -z "${LLVM_CONFIG_PATH:-}" ]] && [[ -z "${LLVM_SYS_170_PREFIX:-}" ]]; then
  cargo_cmd=("$repo_root/scripts/dev-cargo.sh")
fi

while [[ $# -gt 0 ]]; do
  case "$1" in
    --with-service-tests)
      with_service_tests=1
      shift
      ;;
    --timings-file)
      report_path="$(resolve_path "$2")"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

step_names=()
step_durations=()

run_step() {
  local name="$1"
  shift

  local start end duration
  start="$(date +%s)"
  echo
  echo ">>> ${name}"
  "$@"
  end="$(date +%s)"
  duration="$((end - start))"
  echo "<<< ${name} completed in ${duration}s"

  step_names+=("$name")
  step_durations+=("$duration")
}

run_step "cargo build" "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" build
run_step "cargo clippy --all-targets --all-features -- -D warnings" \
  "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" clippy --all-targets --all-features -- -D warnings
if [[ "$with_service_tests" == "1" ]]; then
  run_step "cargo test" env MUX_RUN_SERVICE_TESTS=0 "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" test
else
  run_step "cargo test" "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" test
fi

if [[ "$with_service_tests" == "1" ]]; then
  run_step "cargo test --test service_integration" \
    "${cargo_cmd[@]}" --manifest-path "$repo_root/mux-compiler/Cargo.toml" test --test service_integration -- --nocapture
fi

if [[ -n "$report_path" ]]; then
  mkdir -p "$(dirname "$report_path")"
  {
    printf '{\n'
    printf '  "service_tests": %s,\n' "$( [[ "$with_service_tests" == "1" ]] && echo true || echo false )"
    printf '  "steps": [\n'
    for i in "${!step_names[@]}"; do
      comma=","
      if [[ "$i" -eq $((${#step_names[@]} - 1)) ]]; then
        comma=""
      fi
      printf '    {"name":"%s","duration_seconds":%s}%s\n' \
        "${step_names[$i]}" "${step_durations[$i]}" "$comma"
    done
    printf '  ]\n'
    printf '}\n'
  } > "$report_path"
  echo
  echo "Wrote timing report to $report_path"
fi
