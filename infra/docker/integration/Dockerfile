FROM rust:1.93.1-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        gnupg \
        lsb-release \
        make \
        pkg-config \
        python3 \
        software-properties-common \
        wget \
    && wget --max-redirect=0 -O /usr/share/keyrings/llvm-snapshot.gpg.key https://apt.llvm.org/llvm-snapshot.gpg.key \
    && echo "deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg.key] https://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-17 main" > /etc/apt/sources.list.d/llvm.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        clang-17 \
        libpolly-17-dev \
        llvm-17-dev \
    && rustup component add clippy rustfmt \
    && rm -rf /var/lib/apt/lists/*

ENV LLVM_SYS_170_PREFIX=/usr/lib/llvm-17 \
    LLVM_CONFIG_PATH=/usr/bin/llvm-config-17 \
    LLVM_SYS_170_STRICT_VERSIONING=1 \
    CC=clang-17 \
    CXX=clang++-17 \
    MUX_RUNTIME_SRC=/workspace/mux-runtime

WORKDIR /workspace
