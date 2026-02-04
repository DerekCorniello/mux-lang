# Mux Compiler: AI Agent Guidelines

## Critical Rules
- **MOST IMPORTANT: No hacks or workarounds** - write clean, production-ready code. No hardcoding, no temporary solutions, no fighting the type system. If something is hard, ask for clarification. Then, do it the right way.
- **Thoroughly understand existing code** - read relevant modules before implementing anything new. Follow existing patterns.
- **Ensure edits are small and tested often** - build and test frequently during development.
- **Follow Rust best practices** - idiomatic code, proper error handling, clear naming.
- **NEVER touch git** - do not run git commands, do not create commits, do not modify git config
- **If confused about language design**, check README.md first, then stop and ask for clarification
- **No clippy errors or warnings** - code must pass `cargo clippy`
- **Rust-like code** - idiomatic, readable, well-structured
- **Old Comments** - remove outdated comments, ensure comments reflect current code

## Critical Understanding
The project is a compiler for a programming langauge, mux. The goal of this language is creating a clean, strongly and statically-typed language implementation with zero-cost abstractions. It is meant to be a clean and modern language, with ease of use and learning as a priority.

## Quick Reference Commands

### Recommended Tools:
- grep -> ripgrep (rg)
- cat -> bat
- cd -> zoxide

### Common issues to watch for:
Executable's output seems to be cutoff -> this is likely a segfault due to incorrect LLVM IR generation. Check codegen changes carefully.

### Building & Testing
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

### Running Tests (User Will Do These)
```bash
# Run all tests - user handles this
cargo test

# Run insta snapshot tests - user handles this
cargo insta test

# Review insta snapshots - user handles this
cargo insta review
```

### Do not do these test commands (user will handle):

## Testing Approach

When testing a feature, the agent should:
1. Run `cargo build` to verify compilation succeeds
2. Run `cargo run -- test_scripts/test_file.mux` to execute and test functionality
    a. You may create test files that cover the new feature
    b. After creating these test files, you may either
         i. add them to existing test files,
         ii. remove them if they were only for ad-hoc testing, or
         iii. leave them if they are useful and do not fit into existing tests. this will require tests to be reviewed manually by the user later.

3. Run `cargo fmt` to ensure consistent formatting
4. Run `cargo clippy` to ensure no warnings or errors

The user will separately run `cargo test` and insta testing for comprehensive validation.

## Current System Architecture

The Mux compiler is organized as a workspace with two main crates:

### mux-compiler (Rust)
Responsible for parsing, semantic analysis, and code generation:
- **lexer** - Tokenizes source code into tokens
- **parser** - Builds AST from tokens  
- **semantics** - Type checking and symbol resolution
- **codegen** - LLVM IR generation (split into 12 focused modules)
  - `mod.rs` - Core module with CodeGenerator struct, `new()`, `generate()`, `emit_ir_to_file()`
  - `expressions.rs` - All expression types (literals, binary ops, function calls, etc.)
  - `statements.rs` - Statement generation (if, while, for, match, return, variable decls)
  - `functions.rs` - Function declaration and generation
  - `methods.rs` - Method call generation for all types
  - `classes.rs` - Class, interface, and enum type generation
  - `constructors.rs` - Constructor generation for classes and enums
  - `operators.rs` - Binary and logical operators
  - `generics.rs` - Generic type instantiation and specialization
  - `types.rs` - Type conversions between Mux types, LLVM types, and type nodes
  - `memory.rs` - RC (reference counting) memory management
  - `runtime.rs` - Runtime function calls and value boxing/unboxing
- **source** - File handling utilities

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
- DO NOT ADD STUPID COMMENTS THAT STATE THE OBVIOUS

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
mux-compiler/
├── Cargo.lock
├── Cargo.toml
├── src
│   ├── codegen/
│   │   ├── mod.rs          # Core module: struct, new(), generate(), emit_ir_to_file()
│   │   ├── expressions.rs  # Expression generation (~3500 lines)
│   │   ├── statements.rs   # Statement generation (~1400 lines)
│   │   ├── functions.rs    # Function declaration and generation
│   │   ├── methods.rs      # Method call generation
│   │   ├── classes.rs      # Class/interface/enum type generation
│   │   ├── constructors.rs # Constructor generation
│   │   ├── operators.rs    # Binary and logical operators
│   │   ├── generics.rs     # Generic type instantiation
│   │   ├── types.rs        # Type conversions
│   │   ├── memory.rs       # RC memory management
│   │   └── runtime.rs      # Runtime boxing/unboxing
│   ├── lexer.rs
│   ├── lib.rs
│   ├── main.rs
│   ├── module_resolver.rs
│   ├── parser.rs
│   ├── semantics.rs
│   └── source.rs
└── tests
    ├── executable_integration.rs
    ├── lexer_integration.rs
    ├── parser_integration.rs
    ├── semantics_integration.rs
    └── snapshots
        └── ...
mux-runtime/
├── Cargo.toml
└── src
    ├── lib.rs
    ├── boxing.rs
    ├── refcount.rs
    ├── bool.rs
    ├── int.rs
    ├── float.rs
    ├── string.rs
    ├── list.rs
    ├── map.rs
    ├── set.rs
    ├── optional.rs
    ├── result.rs
    ├── object.rs
    ├── io.rs
    ├── math.rs
    └── std.rs
```

## Key Constraints
- NO dynamic typing
- NO implicit type conversions
- NO runtime reflection
- All generics must monomorphize at compile time
- Interfaces use static dispatch (no vtables)

## Codegen Module Architecture Notes

### Import Pattern
All codegen submodules follow this pattern:
```rust
use super::CodeGenerator;
use crate::ast::{...};  // Import from ast module, NOT parser
use inkwell::types::{BasicType, ...};
use inkwell::values::{...};

impl<'a> CodeGenerator<'a> {
    pub(super) fn function_name(...) { ... }
}
```

**Important:** Always import types from `crate::ast` (e.g., `PrimitiveType`, `TypeKind`, `TypeNode`), not from `crate::parser` (which has private re-exports).

### Critical Types to Import
When working with inkwell types:
- Use `inkwell::types::BasicType` trait when calling `.fn_type()` or `.size_of()` on `BasicTypeEnum`
- Most files need: `use inkwell::types::{BasicType, BasicTypeEnum};`

### Visibility Rules
- All functions in submodules should be `pub(super)` to be accessible from `mod.rs`
- The `CodeGenerator` struct and its impl block are defined in `mod.rs`
- Helper functions that are only used within a module can be `fn` (private)

### Memory Management (RC)
RC (reference counting) functions are defined in `memory.rs`:
- `push_rc_scope()` - Create new scope for RC tracking
- `track_rc_variable()` - Add variable to current scope
- `generate_all_scopes_cleanup()` - Clean up all scopes
- `rc_inc_if_pointer()` - Increment RC before returning values
- `type_needs_rc_tracking()` - Check if type needs RC

These are core to Mux's memory safety - all heap-allocated values use reference counting.

### Expression vs Statement Distinction
- **Expressions** return values and can be nested (literals, function calls, binary ops)
- **Statements** perform actions and don't return values (variable declarations, if/while/for, return)
- The `generate_expression()` function in `expressions.rs` handles ~30 different expression types
- The `generate_statement()` function in `statements.rs` handles all statement types

### Boxing/Unboxing
Mux boxes all primitive values into a uniform `Value*` representation:
- `box_value()` - Wraps LLVM values into boxed Value pointers
- Runtime provides typed extractors: `get_raw_int_value()`, `get_raw_float_value()`, etc.
- This enables uniform handling in collections and generic functions

### Type System Integration
The codegen works with three type representations:
1. **Mux AST types** (`TypeNode`, `TypeKind`) - from parser/AST
2. **Semantic types** (`Type`, `ResolvedType`) - from semantic analyzer
3. **LLVM types** (`BasicTypeEnum`, `PointerValue`) - for IR generation

Conversion functions in `types.rs` bridge these representations.

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
