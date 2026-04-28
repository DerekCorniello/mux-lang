FROM rust:1.93.1-bookworm

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        gnupg \
        lsb-release \
        make \
        pkg-config \
        python3 \
        software-properties-common \
        wget \
    && wget --max-redirect=0 -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh \
    && chmod +x /tmp/llvm.sh \
    && /tmp/llvm.sh 17 \
    && apt-get install -y --no-install-recommends \
        clang-17 \
        libpolly-17-dev \
        llvm-17-dev \
    && rustup component add clippy rustfmt \
    && rm -rf /var/lib/apt/lists/* /tmp/llvm.sh

ENV LLVM_SYS_170_PREFIX=/usr/lib/llvm-17 \
    LLVM_CONFIG_PATH=/usr/bin/llvm-config-17 \
    LLVM_SYS_170_STRICT_VERSIONING=1 \
    CC=clang-17 \
    CXX=clang++-17 \
    MUX_RUNTIME_SRC=/workspace/mux-runtime

WORKDIR /workspace
