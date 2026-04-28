# Fuzzing

This directory contains `cargo-fuzz` targets for the highest-risk parser and runtime input surfaces.

## Targets
- `lexer`
- `parser`
- `json`

## Local usage
```bash
cd mux-compiler
cargo +nightly fuzz run lexer
cargo +nightly fuzz run parser
cargo +nightly fuzz run json
```

Crash reproducers and minimized inputs are written under `mux-compiler/fuzz/artifacts/`.
Seed corpora live under `mux-compiler/fuzz/corpus/`.
