# Testing Guide

This document describes how to run the Mux compiler test suite, including the
Docker-based testing environment and fuzz testing infrastructure.

## Quick Start

### Run all checks locally (no Docker)

```bash
cargo fmt -- --check
cargo clippy --all-targets --all-features -- -D warnings
cargo build
cargo test
cargo insta test --check
```

### Run all checks in Docker

```bash
docker compose up --build mux-tests
```

This builds the test image, starts PostgreSQL and a TCP/UDP echo server, then
runs formatting, clippy, build, tests, and snapshot verification inside the
container.

## Docker Environment

### Prerequisites

- Docker and Docker Compose (v2)

### Services

| Service       | Description                        | Ports            |
|---------------|------------------------------------|------------------|
| `postgres`    | PostgreSQL 16 for SQL testing      | 5432             |
| `echo-server` | TCP (9000) and UDP (9001) echo server for networking tests | 9000, 9001/udp |
| `mux-tests`   | Compiler build and test runner     | --               |

### Usage

**Run the full test suite:**

```bash
docker compose up --build mux-tests
```

**Run only specific cargo commands inside the container:**

```bash
# Interactive shell
docker compose run --rm mux-tests bash

# Single command
docker compose run --rm mux-tests cargo test
docker compose run --rm mux-tests cargo clippy --all-targets --all-features -- -D warnings
```

**Start just the services (for manual testing):**

```bash
docker compose up -d postgres echo-server
```

This is useful when you want to run networking tests locally against the
containerized echo server and database.

**Tear down everything:**

```bash
docker compose down -v
```

### Environment Variables

The test container exposes connection details for the companion services:

| Variable                    | Value           |
|-----------------------------|-----------------|
| `MUX_TEST_POSTGRES_HOST`    | `postgres`      |
| `MUX_TEST_POSTGRES_PORT`    | `5432`          |
| `MUX_TEST_POSTGRES_USER`    | `mux_test`      |
| `MUX_TEST_POSTGRES_PASSWORD`| `mux_test_pass` |
| `MUX_TEST_POSTGRES_DB`      | `mux_test_db`   |
| `MUX_TEST_ECHO_TCP_HOST`    | `echo-server`   |
| `MUX_TEST_ECHO_TCP_PORT`    | `9000`          |
| `MUX_TEST_ECHO_UDP_HOST`    | `echo-server`   |
| `MUX_TEST_ECHO_UDP_PORT`    | `9001`          |

### Echo Server

The echo server (`scripts/echo-server.py`) provides:

- **TCP echo** on port 9000: connects, reads data, sends it back.
- **UDP echo** on port 9001: receives datagrams, sends them back to the sender.

This allows networking tests (like `test_std_tcp.mux`) to exercise the full
connect/read/write path instead of only testing connection-refused errors.

## Fuzz Testing

Fuzz testing uses [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz) with
libFuzzer to find panics in the lexer and parser. These components accept
arbitrary user input, so they must handle all inputs without crashing.

### Prerequisites

```bash
# Install cargo-fuzz
cargo install cargo-fuzz

# Nightly Rust is required for libFuzzer instrumentation
rustup install nightly
```

### Fuzz Targets

| Target         | Description                           |
|----------------|---------------------------------------|
| `fuzz_lexer`   | Feeds random bytes to the lexer       |
| `fuzz_parser`  | Lexes then parses random bytes        |

### Running Fuzz Tests

**Run all targets for 60 seconds each:**

```bash
./scripts/run-fuzz.sh
```

**Run a specific target:**

```bash
./scripts/run-fuzz.sh lexer 120   # Fuzz lexer for 2 minutes
./scripts/run-fuzz.sh parser 300  # Fuzz parser for 5 minutes
```

**Run directly with cargo-fuzz:**

```bash
cd fuzz
cargo +nightly fuzz run fuzz_lexer -- -max_total_time=60
cargo +nightly fuzz run fuzz_parser -- -max_total_time=60
```

### Corpus

Seed inputs are pre-populated in `fuzz/corpus/` from the existing `.mux` test
files. The fuzzer will grow this corpus automatically as it discovers new
coverage.

### Investigating Crashes

When the fuzzer finds a crash, it saves the input to
`fuzz/artifacts/fuzz_<target>/`. Reproduce it with:

```bash
cd fuzz
cargo +nightly fuzz run fuzz_lexer fuzz/artifacts/fuzz_lexer/<crash_file>
```

### Running Fuzz Tests in Docker

```bash
docker compose run --rm mux-tests bash -c \
  "rustup install nightly && cd /mux/fuzz && cargo +nightly fuzz run fuzz_lexer -- -max_total_time=60"
```

## CI Integration

The GitHub Actions workflow (`.github/workflows/build.yml`) already runs:

1. `cargo fmt -- --check`
2. `cargo clippy --all-targets --all-features -- -D warnings`
3. `cargo test`
4. `cargo build`

The Docker environment mirrors this setup with the same Rust and LLVM versions.
