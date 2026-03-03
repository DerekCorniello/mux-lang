#!/bin/bash
set -euo pipefail

# Mux compiler test runner
# Runs all checks in sequence: fmt, clippy, build, test, snapshot verify

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

pass=0
fail=0

step() {
    echo ""
    echo -e "${YELLOW}=== $1 ===${NC}"
}

run_step() {
    local name="$1"
    shift
    step "$name"
    if "$@"; then
        echo -e "${GREEN}PASS: $name${NC}"
        pass=$((pass + 1))
    else
        echo -e "${RED}FAIL: $name${NC}"
        fail=$((fail + 1))
    fi
}

cd /mux

# 1. Formatting check
run_step "Formatting" cargo fmt -- --check

# 2. Clippy (no warnings)
run_step "Clippy" cargo clippy --all-targets --all-features -- -D warnings

# 3. Build
run_step "Build" cargo build

# 4. Tests
run_step "Tests" cargo test

# 5. Snapshot verification (non-interactive: fails if any snapshots are pending)
run_step "Snapshots" cargo insta test --check

# Summary
echo ""
echo "========================================"
echo -e "Results: ${GREEN}${pass} passed${NC}, ${RED}${fail} failed${NC}"
echo "========================================"

if [ "$fail" -gt 0 ]; then
    exit 1
fi
