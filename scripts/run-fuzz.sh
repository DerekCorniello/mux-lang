#!/bin/bash
set -euo pipefail

# Mux fuzz test runner
#
# Usage:
#   ./scripts/run-fuzz.sh                    # Run all fuzz targets for 60s each
#   ./scripts/run-fuzz.sh lexer              # Run only the lexer fuzzer
#   ./scripts/run-fuzz.sh parser             # Run only the parser fuzzer
#   ./scripts/run-fuzz.sh lexer 300          # Run lexer fuzzer for 300 seconds
#
# Requires: cargo-fuzz (install via `cargo install cargo-fuzz`)
# Must be run with nightly Rust: rustup run nightly ./scripts/run-fuzz.sh

TARGET="${1:-all}"
DURATION="${2:-60}"

FUZZ_DIR="$(cd "$(dirname "$0")/.." && pwd)/fuzz"

run_fuzz_target() {
    local target="$1"
    local seconds="$2"
    echo ""
    echo "=== Fuzzing $target for ${seconds}s ==="
    cd "$FUZZ_DIR"
    cargo +nightly fuzz run "$target" -- \
        -max_total_time="$seconds" \
        -max_len=4096
    echo "=== Done fuzzing $target ==="
}

case "$TARGET" in
    lexer)
        run_fuzz_target fuzz_lexer "$DURATION"
        ;;
    parser)
        run_fuzz_target fuzz_parser "$DURATION"
        ;;
    all)
        run_fuzz_target fuzz_lexer "$DURATION"
        run_fuzz_target fuzz_parser "$DURATION"
        ;;
    *)
        echo "Unknown target: $TARGET"
        echo "Usage: $0 [lexer|parser|all] [duration_seconds]"
        exit 1
        ;;
esac
