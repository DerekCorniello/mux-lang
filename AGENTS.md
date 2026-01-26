# Mux Compiler: AI Agent Guidelines

## Critical Rules
- **MOST IMPORTANT: No hacks or workarounds** - write clean, production-ready code. No hardcoding, no temporary solutions, no fighting the type system. If something is hard, ask for clarification. Then, do it the right way.
- **Use concrete LLVM types only** - no `*mut Value` boxing. All types must be statically known at compile time.
- **Thoroughly understand existing code** - read relevant modules before implementing anything new. Follow existing patterns.
- **Ensure edits are small and tested often** - build and test frequently during development.
- **Follow Rust best practices** - idiomatic code, proper error handling, clear naming.
- **NEVER touch git** - do not run git commands, do not create commits, do not modify git config
- **If confused about language design**, check README.md first, then stop and ask for clarification
- **No clippy errors or warnings** - code must pass `cargo clippy`
- **Rust-like code** - idiomatic, readable, well-structured
- **Old Comments** - remove outdated comments, ensure comments reflect current code

## Critical Understanding
This project is a complete rewrite of the compiler's codegen system from a `*mut Value` boxing architecture to concrete LLVM types. The goal is a clean, statically-typed language implementation with zero-cost abstractions.

## Quick Reference Commands

### Command line overrides:
grep -> ripgrep (rg)
cat -> bat
cd -> zoxide

### Common issues to watch for:
Executable's output seems to be cutoff -> this is likely a segfault due to incorrect LLVM IR generation. Check codegen changes carefully.

### Building & Testing
```bash
# Build the project
cargo build

# Test a Mux file (primary test method)
cargo run -- test_scripts/test_file.mux

# Check for errors (no build)
cargo check

# Format code
cargo fmt

# Run clippy (no errors allowed)
cargo clippy

# Accept insta snapshot updates (only when intentionally updating snapshots)
INSTA_UPDATE=always cargo test
```

### Running Tests (User Will Do These)
```bash
# Run all tests - user handles this
cargo test

# Run insta tests - user handles this
cargo test --doc
```

## Testing Approach

When testing a feature, the agent should:
1. Run `cargo build` to verify compilation succeeds
2. Run `cargo run -- test_scripts/test_file.mux` to execute and test functionality
3. Use best understanding based on test file contents to verify correctness
4. Run `cargo clippy` to ensure no warnings or errors

The user will separately run `cargo test` and insta testing for comprehensive validation.

## Current System Architecture

The Mux compiler is organized as a workspace with two main crates:

### mux-compiler (Rust)
Responsible for parsing, semantic analysis, and code generation:
- **lexer/** - Tokenizes source code into tokens
- **parser/** - Builds AST from tokens  
- **semantics/** - Type checking and symbol resolution
- **codegen/** - LLVM IR generation (this is being rewritten)
- **source/** - File handling utilities

The compiler generates LLVM IR which is compiled to a .ll file, then linked with clang against the runtime.

### mux-runtime (C/Rust)
Provides runtime support for compiled Mux programs:
- **Memory allocation and garbage collection**
- **String operations** (UTF-8 handling)
- **Collection implementations** (list, map, set)
- **Type conversions** and utilities

The compiler generates calls to runtime functions. Understanding this interface is important for any codegen changes.

### Compilation Pipeline
```
.mux source → Lexer → Parser → Semantic Analyzer → CodeGen → .ll file
                                                                          ↓
                                                              clang + mux_runtime → executable
```

## Code Style Guidelines

### General Principles
- Write idiomatic Rust code - clean, readable, well-structured
- Use Rust's type system to prevent errors at compile time
- Prefer Result<T, E> for error handling throughout
- Document public APIs with rustdoc comments
- Keep functions small and focused on single tasks

### First: Understand Existing Code
When working on any feature:
1. Read relevant existing code in the codebase first
2. Understand how similar features are implemented
3. Follow existing patterns and conventions
4. Only then implement the new feature

### Naming Conventions
- **Types**: PascalCase (e.g., `SemanticAnalyzer`, `TypeNode`)
- **Functions/Methods**: snake_case (e.g., `analyze()`, `generate_ir()`)
- **Variables**: snake_case (e.g., `tokens`, `error_count`)
- **Constants**: SCREAMING_SNAKE_CASE (e.g., `MAX_BUFFER_SIZE`)
- **Modules**: snake_case (e.g., `lexer`, `parser`, `semantics`)
- **Type Parameters**: Single uppercase letter (e.g., `T`, `U`) or descriptive (e.g., `Elem`)

### Imports & Module Structure
- Use absolute imports with `crate::`, `super::`, or root
- Group paths imports: std → external → local
- Prefer `use` statements over path prefixes in code
- Keep modules focused: lexer, parser, semantics, codegen, source

### Formatting
- Run `cargo fmt` before committing
- Use 4-space indentation
- Limit line length to 100 characters
- Use trailing commas in multi-line expressions

### Error Handling
- Return `Result<T, String>` or `Result<T, Box<dyn Error>>` for fallible operations
- Use `?` operator for error propagation
- Provide context in error messages: `Err(format!("failed to {}", action))`
- Handle all Result variants explicitly (no `.unwrap()` except in tests)

### Type System
- Use concrete LLVM types (i64, f64, *mut c_char) - NO *mut Value boxing
- Leverage Rust's type system for compile-time safety
- Avoid unnecessary boxing or dynamic dispatch
- Use Option<T> for nullable values

### Comments & Documentation
- Use `///` for public API documentation
- Use `//` for implementation notes
- Document WHY not WHAT
- Add TODO comments with issue references: `// TODO(#123): description`

### Testing
- Write tests alongside implementation using `#[cfg(test)]` modules
- Use descriptive test names: `#[test] fn parses_valid_expression()`
- Use insta for snapshot testing with `assert_debug_snapshot!`
- Group related tests in mod tests blocks

### Git Workflow (DO NOT TOUCH)
- Never run git commands
- Never create commits
- Never modify git config
- Let the user handle all version control

## Project Structure
```
mux-compiler/src/
├── codegen/      # LLVM IR generation (being rewritten)
├── lexer/        # Tokenization
├── parser/       # AST construction
├── semantics/    # Type checking
└── source/       # File handling
```

## Key Constraints
- NO dynamic typing or *mut Value boxing
- NO implicit type conversions
- NO runtime reflection
- All generics must monomorphize at compile time
- Interfaces use static dispatch (no vtables)

## When to Ask for Clarification
- Unclear requirements or specifications
- Language design questions - check README.md first
- Architectural decisions affecting multiple components
- Trade-offs between correctness and performance
- Changes to existing public APIs
- Anything that seems like a "hack" or workaround

## Workflow for New Work
1. Check README.md if confused about language design
2. Read existing relevant code to understand patterns
3. Implement the feature using best Rust practices
4. Run `cargo build` to verify compilation
5. Run `cargo run -- test_scripts/test_file.mux` to test functionality
6. Run `cargo clippy` to ensure no warnings/errors
7. Let user run `cargo test` for comprehensive testing
