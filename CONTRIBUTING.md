# Contributing to Mux

Thanks for your interest! This guide explains how to contribute to Mux.

---

## How to Get Started

1. Fork the repository.
2. Clone your fork locally.
3. Run the tests (`cargo test`) to make sure everything is working.
4. Running your code should be done via `cargo run -- `, followed by the command you want to run, e.g. `cargo run -- run test_file.mux`. So `mux` is substituted by `cargo run -- `.
4. Create a new branch for your changes, named with the tag first, and description after, e.g., `bug/xyz-fix` or `feature/new-feat`.
5. Make your changes.
6. Run tests again to ensure nothing is broken.
7. Commit your changes with clear messages.
8. Push your branch to your fork.
9. Open a Pull Request against the `main` branch of the original repository.

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

- Rust code should follow `rustfmt` defaults.
- Use `clippy` to catch common mistakes.
- Write clear, concise comments where necessary.
- Ensure all new code is covered by tests.
- I don't care what your commit messages look like, as long as they are clear about what changed.

---

## Issue & PR Guidelines

- Check for existing issues before opening a new one.
- Use the provided issue templates for bugs and features.
- Link your PR to an existing issue when possible.
- Respond to review comments promptly and respectfully.
- Add me, @DerekCorniello, as a reviewer to your PRs.

---
