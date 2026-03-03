# Dockerfile for building and testing the Mux compiler
#
# Provides: Rust 1.93.1, LLVM 17, clang-17, cargo-fuzz, cargo-insta
# Usage:
#   docker compose run --rm mux-tests
#   docker compose run --rm mux-tests cargo clippy --all-targets --all-features -- -D warnings

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies: LLVM 17, clang, build tools, Python (for echo server)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    build-essential \
    pkg-config \
    llvm-17-dev \
    clang-17 \
    libpolly-17-dev \
    python3 \
    && rm -rf /var/lib/apt/lists/*

# Install Rust 1.93.1 with rustfmt and clippy
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
    sh -s -- -y --default-toolchain 1.93.1 --component rustfmt,clippy
ENV PATH="/root/.cargo/bin:${PATH}"

# Install cargo-insta for snapshot testing review
RUN cargo install cargo-insta

ENV LLVM_SYS_170_PREFIX=/usr/lib/llvm-17

WORKDIR /mux

# Copy manifests first for dependency caching
COPY Cargo.toml Cargo.lock ./
COPY mux-runtime/Cargo.toml mux-runtime/Cargo.toml
COPY mux-compiler/Cargo.toml mux-compiler/Cargo.toml

# Create minimal source stubs so cargo can resolve the workspace and fetch deps.
# The mux-runtime crate-type includes staticlib/cdylib, so we provide a tiny lib.
# The mux-compiler build.rs sets env vars that build_config.rs reads via env!(),
# so the full build will fail here, but dependency fetching still succeeds.
RUN mkdir -p mux-runtime/src mux-compiler/src && \
    echo '#![allow(dead_code)]' > mux-runtime/src/lib.rs && \
    echo 'fn main() {}' > mux-compiler/src/main.rs && \
    echo '' > mux-compiler/src/lib.rs

# Fetch and pre-compile dependencies (cached unless Cargo.toml/Cargo.lock change)
RUN cargo fetch && \
    cargo build 2>/dev/null || true

# Remove stubs before copying real sources
RUN rm -rf mux-runtime/src mux-compiler/src

# Copy the full source tree
COPY mux-runtime/ mux-runtime/
COPY mux-compiler/ mux-compiler/
COPY test_scripts/ test_scripts/
COPY scripts/ scripts/
COPY fuzz/ fuzz/

# Set the runtime source path for the build.rs script
ENV MUX_RUNTIME_SRC=/mux/mux-runtime

CMD ["scripts/run-tests.sh"]
