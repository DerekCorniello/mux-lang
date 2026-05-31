# Stage 1: Build the Mux compiler (Rust + LLVM 17 + clang)
FROM rust:1.93.1-bookworm AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    lsb-release \
    gnupg \
    software-properties-common \
    && wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh \
    && chmod +x /tmp/llvm.sh \
    && /tmp/llvm.sh 17 \
    && apt-get install -y --no-install-recommends \
        clang-17 \
        libpolly-17-dev \
        llvm-17-dev \
    && rm -rf /var/lib/apt/lists/* /tmp/llvm.sh

ENV LLVM_SYS_170_PREFIX=/usr/lib/llvm-17 \
    CC=clang-17

WORKDIR /build
COPY . .

# Build the runtime library first (produces .a and .so)
RUN cargo build -p mux-runtime --release

# Build the compiler (needs runtime lib from previous step at build time)
RUN cargo build -p mux-compiler --release && \
    cp target/release/mux /usr/local/bin/mux && \
    cp target/release/libmux_runtime.a /usr/local/lib/mux/ && \
    cp target/release/libmux_runtime.so /usr/local/lib/mux/

# Stage 2: Runtime image (minimal Debian with clang + Python)
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    lsb-release \
    gnupg \
    software-properties-common \
    ca-certificates \
    && wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh \
    && chmod +x /tmp/llvm.sh \
    && /tmp/llvm.sh 17 \
    && apt-get install -y --no-install-recommends \
        clang-17 \
        python3 \
        python3-pip \
    && rm -rf /var/lib/apt/lists/* /tmp/llvm.sh

# Copy the mux binary and runtime library
COPY --from=builder /usr/local/bin/mux /usr/local/bin/mux
COPY --from=builder /usr/local/lib/mux/libmux_runtime.a /usr/local/lib/mux/libmux_runtime.a
COPY --from=builder /usr/local/lib/mux/libmux_runtime.so /usr/local/lib/mux/libmux_runtime.so

# Copy LLVM shared libraries needed by the mux binary
COPY --from=builder /usr/lib/llvm-17/lib/libLLVM-17.so /usr/lib/llvm-17/lib/libLLVM-17.so
COPY --from=builder /usr/lib/llvm-17/lib/libLLVM-17.so.1 /usr/lib/llvm-17/lib/libLLVM-17.so.1
COPY --from=builder /usr/lib/llvm-17/lib/libclang-cpp.so /usr/lib/llvm-17/lib/libclang-cpp.so
COPY --from=builder /usr/lib/llvm-17/lib/libclang-cpp.so.17 /usr/lib/llvm-17/lib/libclang-cpp.so.17
COPY --from=builder /usr/lib/llvm-17/lib/libPolly.so /usr/lib/llvm-17/lib/libPolly.so
COPY --from=builder /usr/lib/llvm-17/lib/libPolly.so.17 /usr/lib/llvm-17/lib/libPolly.so.17

ENV MUX_RUNTIME_LIB=/usr/local/lib/mux/libmux_runtime.a \
    LD_LIBRARY_PATH=/usr/lib/llvm-17/lib

# Create a symlink so 'clang' resolves
RUN ln -sf /usr/bin/clang-17 /usr/local/bin/clang

# Install Python dependencies
COPY api/ /app/api/
RUN pip3 install --no-cache-dir -r /app/api/requirements.txt

# Run as non-root in production
RUN useradd --create-home --uid 10001 appuser
USER appuser

WORKDIR /tmp
EXPOSE 8080

CMD ["gunicorn", "--bind", "0.0.0.0:8080", "--workers", "2", "--timeout", "60", "api.server:app"]
