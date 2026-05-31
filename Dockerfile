# Stage 1: Build the Mux compiler (Rust + LLVM 17 + clang)
FROM rust:1.93.1-bookworm AS builder

# Install LLVM 17 via GPG-verified apt repository (avoids running unsigned llvm.sh)
RUN apt-get update && apt-get install -y --no-install-recommends \
        wget \
        ca-certificates \
        gnupg \
        lsb-release \
    && wget -O /usr/share/keyrings/llvm-snapshot.gpg.key https://apt.llvm.org/llvm-snapshot.gpg.key \
    && echo "deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg.key] http://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-17 main" > /etc/apt/sources.list.d/llvm.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        clang-17 \
        libpolly-17-dev \
        llvm-17-dev \
    && rm -rf /var/lib/apt/lists/*

ENV LLVM_SYS_170_PREFIX=/usr/lib/llvm-17 \
    CC=clang-17

WORKDIR /build
COPY Cargo.lock Cargo.toml ./
COPY mux-compiler/Cargo.toml mux-runtime/Cargo.toml ./
COPY . .

# Build the runtime library first (produces .a and .so)
RUN cargo build -p mux-runtime --release

# Build the compiler (needs runtime lib from previous step at build time)
RUN cargo build -p mux-lang --release && \
    mkdir -p /usr/local/lib/mux && \
    cp target/release/mux /usr/local/bin/mux && \
    cp target/release/libmux_runtime.a /usr/local/lib/mux/ && \
    cp target/release/libmux_runtime.so /usr/local/lib/mux/

# Stage 2: Runtime image (minimal Debian with clang + Python)
FROM debian:bookworm-slim

# Install LLVM 17 via GPG-verified apt repository
RUN apt-get update && apt-get install -y --no-install-recommends \
        wget \
        ca-certificates \
        gnupg \
        lsb-release \
    && wget -O /usr/share/keyrings/llvm-snapshot.gpg.key https://apt.llvm.org/llvm-snapshot.gpg.key \
    && echo "deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg.key] http://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-17 main" > /etc/apt/sources.list.d/llvm.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        clang-17 \
        python3 \
        python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Copy the mux binary and runtime library
COPY --from=builder /usr/local/bin/mux /usr/local/bin/mux
COPY --from=builder /usr/local/lib/mux/libmux_runtime.a /usr/local/lib/mux/libmux_runtime.a
COPY --from=builder /usr/local/lib/mux/libmux_runtime.so /usr/local/lib/mux/libmux_runtime.so

# Copy LLVM shared libraries needed by the mux binary
COPY --from=builder /usr/lib/llvm-17/lib/libLLVM-*.so* /usr/lib/llvm-17/lib/
COPY --from=builder /usr/lib/llvm-17/lib/libLTO.so* /usr/lib/llvm-17/lib/
COPY --from=builder /usr/lib/llvm-17/lib/libRemarks.so* /usr/lib/llvm-17/lib/
COPY --from=builder /usr/lib/llvm-17/lib/libclang*.so* /usr/lib/llvm-17/lib/
COPY --from=builder /usr/lib/llvm-17/lib/LLVM*.so /usr/lib/llvm-17/lib/
COPY --from=builder /usr/lib/llvm-17/lib/liblldb*.so* /usr/lib/llvm-17/lib/

ENV MUX_RUNTIME_LIB=/usr/local/lib/mux/libmux_runtime.a \
    LD_LIBRARY_PATH=/usr/lib/llvm-17/lib

# Create a symlink so 'clang' resolves
RUN ln -sf /usr/bin/clang-17 /usr/local/bin/clang

# Install Python dependencies with uv (hash-pinned, binary-only)
COPY api/requirements.lock /app/api/requirements.lock
COPY api/requirements.txt /app/api/requirements.txt
COPY api/server.py /app/api/server.py
RUN pip3 install --no-cache-dir --break-system-packages uv && \
    uv venv /opt/venv && \
    VIRTUAL_ENV=/opt/venv PATH="/opt/venv/bin:$PATH" uv pip install --no-cache --only-binary :all: --require-hashes -r /app/api/requirements.lock

ENV PATH="/opt/venv/bin:$PATH" \
    PYTHONPATH="/app"

# Run as non-root in production
RUN useradd --create-home --uid 10001 appuser
USER appuser

WORKDIR /tmp
EXPOSE 8080

CMD ["gunicorn", "--bind", "0.0.0.0:8080", "--workers", "2", "--timeout", "60", "api.server:app"]
