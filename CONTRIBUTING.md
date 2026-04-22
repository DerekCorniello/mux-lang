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
   ./scripts/dev-cargo.sh test
   ```

7. Running Mux programs during development:

   ```bash
   ./scripts/dev-cargo.sh run test_scripts/your_file.mux
   ```

8. Always run `cargo fmt` and `cargo clippy` before committing changes.
9. For releases, update `VERSION` first, then update the matching section in `CHANGELOG.md`, then run `./scripts/sync-version.sh`.
10. Create a new branch for your changes, named with the tag first, and description after, e.g., `bug/xyz-fix` or `feature/new-feat`.
11. Make your changes.
12. Run tests again to ensure nothing is broken.
13. Commit your changes with clear messages.
14. Push your branch to your fork.
15. Open a Pull Request against the `main` branch of the original repository.
16. AI agents should follow the guidelines in [AGENTS.md](AGENTS.md).

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
