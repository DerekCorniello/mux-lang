# Mux Compiler: AI Agent Guidelines

Please also read the 10x Dev Skill that covers best practices at ~/.opencode/skills/derek-10x-dev-practices/SKILL.md if present.

## Critical Rules
- **No hacks or workarounds** - write clean, production‑ready code. No hardcoding, no temporary solutions, no fighting the type system. If something is hard, ask for clarification, then do it the right way.
- **No special characters** - avoid em‑dashes, emojis, or other non‑ASCII characters in code, comments, or commit messages.
- **Understand existing code first** - read relevant modules before implementing anything new. Follow existing patterns.
- **Ask when unsure** - converse with the user to clarify requirements. If still unsure, explore the codebase and propose a solution.
- **Small, tested edits** - build and test frequently during development.
- **Follow Rust best practices** - idiomatic code, proper error handling, clear naming.
- **Never touch git** - do not run git commands, create commits, or modify git config. Let the user handle version control.
- **If confused about language design**, check README.md first, then stop and ask for clarification.
- **No clippy warnings** - code must pass `cargo clippy --all-targets --all-features -- -D warnings` (run this exact command for strict linting).
- **Pre-existing Issues** - if you encounter an issue that seems to be pre‑existing, _ALWAYS_ bring it up that you uncovered it, and then proceed with fixes.
- **Remove outdated comments** - ensure comments reflect current code.
- **NEVER READ THE `.env` FILE** - do not read or parse the `.env` file. If environment variables are needed, ask the user to provide them explicitly.
- When running commands that require environment variables, execute `source .env` in the shell to load them. This keeps secrets in process memory without exposing file contents to the AI context.

## Critical Understanding
Mux is a statically‑typed, reference‑counted language that aims for clean, zero‑cost abstractions. The compiler generates LLVM IR and links with a C/Rust runtime. The goal is a modern, easy‑to‑learn language with strong static typing.

## Development Process

### Quick Reference Commands
```bash
# Build the project
cargo build

# Test a Mux file (primary test method)
cargo run -- run test_scripts/test_file.mux

# Check for errors (no build)
cargo check

# Format code
cargo fmt

# Run clippy (no errors allowed)
cargo clippy
```

### Testing Approach

All of the following tools should be installed, if you find an issue running these commands, please stop and ask the user for help.

Only run the following commands after you have completed your code changes and are ready to test, do not run them during development, as they can be time‑consuming.

When feature complete and ready for testing a feature:
1. Run `cargo build` to verify compilation.
2. Run `cargo run -- test_scripts/test_file.mux` to test functionality.
   - Create test files that cover the new feature.
   - Add them to existing test files if appropriate, or remove ad‑hoc tests.
3. Run `cargo fmt` for consistent formatting.
4. Run `cargo clippy` to ensure no warnings/errors.

> [!TIP]
> Only use the following commands (Steps 5 and 6) for larger changes that require comprehensive testing, 
> or if the user explicitly asks. For small edits, rely on `cargo check` and unit tests.

5. Run SonarQube analysis locally to check code quality:
   ```bash
   source .env && npx --yes sonar-scanner -Dsonar.token="$SONARQUBE_TOKEN" -Dsonar.host.url="$SONARQUBE_URL"
   ```
   Results appear at `https://sonarcloud.io/dashboard?id=DerekCorniello_mux-lang`, please fetch them (this does not require login or auth).
6. Run greptile cli tool for contrast against main via:
   ```bash
   greptile review -b main --plain
   ```

The user will run `cargo test` and insta snapshot tests separately. Do not manually edit the snapshots.

### Common Issues
- Executable output seems cut off → likely a segfault due to incorrect LLVM IR generation. Review codegen changes carefully.

## Release Process

The root `VERSION` file is the single source of truth. Follow this checklist in order when cutting a new release.

Steps 1-5 are agent-safe and should be done by the agent. Steps 6-8 are **MAINTAINER-ONLY**: the agent must NOT run git, crates.io publish, or deploy commands. The agent should prepare everything, then hand these commands to the user to run themselves.

1. **Gather changes**: Review merged PRs, closed issues, and commits since the last release tag (use the GitHub MCP tools or `git log <last-tag>..HEAD`). Be diligent: read each PR/issue body so the changelog is accurate, not just commit subjects.
2. **Update `CHANGELOG.md`**: Add a new `## [X.Y.Z] - YYYY-MM-DD` section above the previous one, grouped into `Added` / `Changed` / `Fixed` (and `Security` if relevant). Reference issue/PR numbers (e.g. `Closes #211`). `sync-version.sh` fails if the latest changelog section does not match `VERSION`.
3. **Bump `VERSION`**: Set it to the new `X.Y.Z`.
4. **Sync version fields**: Run `./scripts/sync-version.sh`. This updates and validates all 8 tracked fields:
   - `mux-compiler/Cargo.toml` package version
   - `mux-compiler/Cargo.toml` `mux-runtime` dependency version
   - `mux-runtime/Cargo.toml` package version
   - `README.md` version badge
   - `README.md` `- **Current Version:**` line
   - `mux-website/package.json` version
   - `mux-syntax-highlighting/textmate-mux/vscode-language-mux/package.json` version
   - `mux-syntax-highlighting/tree-sitter-mux/tree-sitter.json` version
5. **Refresh lockfiles**: Run `cargo build` to update `Cargo.lock` with the new workspace versions. If website lockfile metadata needs refreshing, run `npm install` in `mux-website/`. Re-run `./scripts/sync-version.sh` to confirm it prints `Version check passed`.

The following steps are **MAINTAINER-ONLY**. The agent must hand these to the user, who will run them; the agent must not execute them.

6. **Git tag** (USER RUNS THIS): `git tag -a vX.Y.Z -m "Release vX.Y.Z"` then `git push origin vX.Y.Z`.
7. **Publish to crates.io** (USER RUNS THIS): Order matters because `mux-lang` (the `mux-compiler` crate) depends on `mux-runtime`. Publish the runtime first, wait for it to be indexed, then the compiler:
   ```bash
   cargo publish -p mux-runtime
   cargo publish -p mux-lang   # package name is "mux-lang"; binary/crate name is "mux"
   ```
   Note: `mux-compiler/Cargo.toml` is published as `mux-lang`; the `[[bin]]` name is `mux`.
8. **Deploy the compiler API** (USER RUNS THIS): The Fly.io app is `mux-lang-api` (see `fly.toml`, builds from `Dockerfile`). Run `fly deploy` from the repo root. `fly.toml` has no hardcoded version, so no edit is needed there.

Note: The CLI/editor tooling (VSCode extension, tree-sitter) version fields are synced by step 4 but are not separately published by this checklist.

## System Architecture
The Mux compiler is a workspace with three main partitions:

### mux-compiler (Rust)
Responsible for parsing, semantic analysis, and LLVM IR generation:
- **lexer** - tokenizes source code
- **parser** - builds AST
- **semantics** - type checking and symbol resolution
- **codegen** - generates LLVM IR (output `.ll` files)

Use `mux run -i <file.mux>` to view the generated IR.

### mux-runtime (C/Rust)
Provides runtime support for compiled Mux programs:
- Memory allocation and reference counting
- String operations (UTF‑8)
- Collection implementations (list, map, set)
- Type conversions and utilities
- Standard library

The compiler generates calls to runtime functions; understanding this interface is essential for codegen changes.

## Code Style Guidelines
- Write idiomatic Rust – clean, readable, well‑structured.
- Use Rust's type system to prevent compile‑time errors.
- Prefer `Result<T, E>` for fallible operations.
- Document public APIs with rustdoc comments (`///`).
- Keep functions small and focused.

### Naming Conventions
- **Types**: `PascalCase`
- **Functions/Methods**: `snake_case`
- **Variables**: `snake_case`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Modules**: `snake_case`
- **Type Parameters**: single uppercase letter (`T`, `U`) or descriptive (`Elem`).
- **Unused variables**: prefix with `_`.

### Error Handling
- Return `Result<T, String>` or `Result<T, Box<dyn Error>>`.
- Use the `?` operator for propagation.
- Provide context: `Err(format!("failed to {}", action))`.
- No `.unwrap()` except in tests.

### Type System
- Use concrete LLVM types (e.g., `i64`, `f64`, `*mut c_char`); do **not** box `*mut Value`.
- Leverage Rust's type system for compile‑time safety.
- Avoid unnecessary boxing or dynamic dispatch.
- Use `Option<T>` for nullable values.

### Comments
- `///` for public API documentation.
- `//` for implementation notes.
- Document *why*, not *what*.
- Do not state the obvious.

## Project Structure
Key directories:
- `mux-compiler/src/` – compiler implementation.
- `mux-runtime/src/` – runtime library.
- `mux-website/docs/` – documentation.
- `test_scripts/` – sample Mux programs.

## Key Constraints
- No dynamic typing.
- No implicit type conversions.
- No runtime reflection.
- All generics monomorphize at compile time.
- Interfaces use static dispatch (no vtables).

## Codegen Module Architecture
- Submodules import from `crate::ast`, not `crate::parser`.
- Visibility: `pub(super)` for submodule functions, `fn` for private helpers.
- Memory management uses reference counting (see `memory.rs`).
- Expressions vs. statements: expressions return values; statements perform actions.
- Boxing/unboxing: all primitive values are boxed into `*mut Value` pointers.
- Three type representations: AST (`TypeNode`), semantic (`Type`), LLVM (`BasicTypeEnum`).

## When to Ask for Clarification
- Unclear requirements or specifications.
- Language design questions (check README.md first).
- Architectural decisions affecting multiple components.
- Trade‑offs between correctness and performance.
- Changes to existing public APIs.
- Anything that seems like a "hack" or workaround.

## Workflow for New Work
1. Check README.md if confused about language design.
2. Read existing relevant code to understand patterns.
3. Implement the feature using best Rust practices.
4. Run `cargo build` to verify compilation.
5. Run `cargo run -- test_scripts/test_file.mux` to test functionality.
6. Run `cargo clippy` to ensure no warnings/errors.
7. Let the user run `cargo test` for comprehensive testing.
8. After tests pass, update documentation (website and root README as needed).

**Add to this document as you learn vital information.**
