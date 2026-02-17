#!/bin/sh
set -e

echo "Checking formatting"
cargo fmt -- --check

echo "Running clippy"
cargo clippy --all-targets --all-features -- -D warnings

echo "Running tests"
cargo test

echo "Building workspace"
cargo build
