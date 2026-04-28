# Contributing to Mux

Thanks for your interest! This guide explains how to contribute to Mux.

---

## How to Get Started

1. Fork the repository.
2. Clone your fork locally.
3. Install Rust (edition 2024 required): `rustup install stable`
4. Run the bootstrap script to install LLVM 17 automatically:

   ```bash
   ./scripts/bootstrap-dev.sh
   ```

   This script detects your OS and installs LLVM 17 and clang. It supports:
   - Arch Linux (via yay)
   - Debian/Ubuntu (via apt)
   - macOS (via Homebrew)

5. Build the compiler using the dev wrapper:

   ```bash
   ./scripts/dev-cargo.sh build
   ```

   The `dev-cargo.sh` script automatically sets the correct LLVM environment variables.

6. Run tests to make sure everything is working:

   ```bash
   ./scripts/run-checks.sh
   ```

7. Running Mux programs during development:

    ```bash
    cargo run -- run test_scripts/your_file.mux
    ```

    The workspace defaults to the compiler crate, so this also works from the repo root.

8. Run the Docker-backed integration suite when touching networking or external SQL providers:

   ```bash
   ./scripts/integration-checks.sh
   ```

9. Capture a profiling baseline when investigating slow checks or compile/runtime regressions:

   ```bash
   ./scripts/measure-baseline.sh
   ```

10. Profile the compiler when you want to investigate slow parsing, generics, semantics, or codegen:

    ```bash
    ./scripts/profile-compiler.sh test_scripts/nested_generics.mux
    ```

    This writes profiles under `target/profile/compiler/<script-name>` by default.

11. Profile runtime behavior when you want to investigate hot string, JSON, network, or SQL paths:

    ```bash
    ./scripts/profile-runtime.sh test_scripts/test_std_http_server.mux
    ```

    This writes profiles under `target/profile/runtime/<script-name>` by default.

12. Always run `cargo fmt` and `cargo clippy` before committing changes.
13. For releases, update `VERSION` first, then update the matching section in `CHANGELOG.md`, then run `./scripts/sync-version.sh`.
14. Create a new branch for your changes, named with the tag first, and description after, e.g., `bug/xyz-fix` or `feature/new-feat`.
15. Make your changes.
16. Run tests again to ensure nothing is broken.
17. Commit your changes with clear messages.
18. Push your branch to your fork.
19. Open a Pull Request against the `main` branch of the original repository.
20. AI agents should follow the guidelines in [AGENTS.md](AGENTS.md).

---

## What Contributions Are Welcome

- Bug fixes
- Documentation improvements
- Design discussions
- Compiler frontend improvements
- Playground components

We generally don't accept contributions that:
- Break core language design
- Add unnecessary complexity
- Go against project goals (see README for design philosophy)

---

## Code Style

- Rust code should follow `rustfmt` defaults (edition 2024).
- Use `clippy` to catch common mistakes.
- Write clear, concise comments where necessary.
- Ensure all new code is covered by tests.
- Snapshot testing uses [insta](https://insta.rs/) - run `cargo insta test` to review snapshots.
- I don't care what your commit messages look like, as long as they are clear about what changed.

---

## Tooling

- Standard Rust tooling: `cargo fmt`, `cargo clippy`, and `cargo test`.
- Canonical local verification: `./scripts/run-checks.sh`
- Docker-backed integration verification: `./scripts/integration-checks.sh`
- Profiling baseline capture: `./scripts/measure-baseline.sh`
- Compiler profiling: `./scripts/profile-compiler.sh`
- Runtime profiling: `./scripts/profile-runtime.sh`
- Fuzzing targets live under `mux-compiler/fuzz/` and are intended for local `cargo fuzz` runs.

---

## Issue & PR Guidelines

- Check for existing issues before opening a new one.
- Use the provided issue templates for bugs and features.
- Link your PR to an existing issue when possible.
- Respond to review comments promptly and respectfully.
- Add me, @DerekCorniello, as a reviewer to your PRs.

---

## Project Resources

- **Language Spec**: [README.md](README.md)
- **AI Agent Workflow**: [AGENTS.md](AGENTS.md)
- **License**: MIT

---
