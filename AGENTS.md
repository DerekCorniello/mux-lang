# Mux Compiler: AI Agent Guidelines

## Critical Rules
- **No hacks or workarounds** - write clean, production‑ready code. No hardcoding, no temporary solutions, no fighting the type system. If something is hard, ask for clarification, then do it the right way.
- **No special characters** - avoid em‑dashes, emojis, or other non‑ASCII characters in code, comments, or commit messages.
- **Understand existing code first** - read relevant modules before implementing anything new. Follow existing patterns.
- **Ask when unsure** - converse with the user to clarify requirements. If still unsure, explore the codebase and propose a solution.
- **Small, tested edits** - build and test frequently during development.
- **Follow Rust best practices** - idiomatic code, proper error handling, clear naming.
- **Never touch git** - do not run git commands, create commits, or modify git config. Let the user handle version control.
- **If confused about language design**, check README.md first, then stop and ask for clarification.
- **No clippy warnings** - code must pass `cargo clippy` without warnings.
- **Remove outdated comments** - ensure comments reflect current code.

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
When testing a feature:
1. Run `cargo build` to verify compilation.
2. Run `cargo run -- test_scripts/test_file.mux` to test functionality.
   - Create test files that cover the new feature.
   - Add them to existing test files if appropriate, or remove ad‑hoc tests.
3. Run `cargo fmt` for consistent formatting.
4. Run `cargo clippy` to ensure no warnings/errors.
5. Use the SonarQube MCP tool to check code quality.

The user will run `cargo test` and insta snapshot tests separately.

### Common Issues
- Executable output seems cut off → likely a segfault due to incorrect LLVM IR generation. Review codegen changes carefully.

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
