# Stage 1: Build the Mux compiler (Rust + LLVM 22 + clang)
FROM rust:1.93.1-bookworm AS builder

# Install LLVM 22 via GPG-verified apt repository (avoids running unsigned llvm.sh)
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        gnupg \
        lsb-release \
        wget \
    && wget --max-redirect=0 -O /usr/share/keyrings/llvm-snapshot.gpg.key https://apt.llvm.org/llvm-snapshot.gpg.key \
    && echo "deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg.key] https://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-22 main" > /etc/apt/sources.list.d/llvm.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        clang-22 \
        libpolly-22-dev \
        llvm-22-dev \
    && rm -rf /var/lib/apt/lists/*

ENV LLVM_SYS_221_PREFIX=/usr/lib/llvm-22 \
    CC=clang-22

WORKDIR /build
COPY Cargo.lock Cargo.toml ./
COPY mux-compiler/Cargo.toml mux-runtime/Cargo.toml ./
COPY . .

# Build the runtime library first (produces .a and .so)
RUN cargo build -p mux-runtime --release --locked

# Build the compiler (needs runtime lib from previous step at build time)
RUN cargo build -p mux-lang --release --locked && \
    mkdir -p /usr/local/lib/mux && \
    cp target/release/mux /usr/local/bin/mux && \
    cp target/release/libmux_runtime.a /usr/local/lib/mux/ && \
    cp target/release/libmux_runtime.so /usr/local/lib/mux/

# Stage 2: Runtime image (minimal Debian with clang + Python)
FROM debian:bookworm-slim

# Install LLVM 22 via GPG-verified apt repository
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        gnupg \
        lsb-release \
        wget \
    && wget --max-redirect=0 -O /usr/share/keyrings/llvm-snapshot.gpg.key https://apt.llvm.org/llvm-snapshot.gpg.key \
    && echo "deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg.key] https://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-22 main" > /etc/apt/sources.list.d/llvm.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        clang-22 \
        python3 \
        python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Copy the mux binary and runtime library
COPY --from=builder /usr/local/bin/mux /usr/local/bin/mux
COPY --from=builder /usr/local/lib/mux/libmux_runtime.a /usr/local/lib/mux/libmux_runtime.a
COPY --from=builder /usr/local/lib/mux/libmux_runtime.so /usr/local/lib/mux/libmux_runtime.so

# Copy LLVM shared libraries needed by the mux binary
COPY --from=builder /usr/lib/llvm-22/lib/libLLVM-*.so* /usr/lib/llvm-22/lib/
COPY --from=builder /usr/lib/llvm-22/lib/libLTO.so* /usr/lib/llvm-22/lib/
COPY --from=builder /usr/lib/llvm-22/lib/libRemarks.so* /usr/lib/llvm-22/lib/
COPY --from=builder /usr/lib/llvm-22/lib/libclang*.so* /usr/lib/llvm-22/lib/
COPY --from=builder /usr/lib/llvm-22/lib/LLVM*.so /usr/lib/llvm-22/lib/
COPY --from=builder /usr/lib/llvm-22/lib/liblldb*.so* /usr/lib/llvm-22/lib/

ENV MUX_RUNTIME_LIB=/usr/local/lib/mux/libmux_runtime.a \
    LD_LIBRARY_PATH=/usr/lib/llvm-22/lib

# Create a symlink so 'clang' resolves
RUN ln -sf /usr/bin/clang-22 /usr/local/bin/clang

# Install Python dependencies with uv (hash-pinned, binary-only)
COPY api/requirements.lock /app/api/requirements.lock
COPY api/requirements.txt /app/api/requirements.txt
COPY api/server.py /app/api/server.py
RUN echo 'uv==0.6.5' \
        '--hash=sha256:15dae245979add192c4845947da1a9141f95c19403d1c0d75019182e6882e7d4' \
        > /tmp/uv-req.txt && \
    pip3 install --no-cache-dir --break-system-packages --only-binary :all: --require-hashes \
        -r /tmp/uv-req.txt && \
    rm /tmp/uv-req.txt && \
    uv venv /opt/venv && \
    VIRTUAL_ENV=/opt/venv PATH="/opt/venv/bin:$PATH" \
        uv pip install --no-cache --only-binary :all: --require-hashes \
            -r /app/api/requirements.lock

ENV PATH="/opt/venv/bin:$PATH" \
    PYTHONPATH="/app"

# Run as non-root in production
RUN useradd --create-home --uid 10001 appuser
USER appuser

WORKDIR /tmp
EXPOSE 8080

CMD ["gunicorn", "--bind", "0.0.0.0:8080", "--workers", "2", "--timeout", "60", "api.server:app"]
