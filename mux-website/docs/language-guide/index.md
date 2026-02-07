# Language Reference

Mux is a statically-typed, reference-counted programming language combining simplicity with safety.

## Overview

Mux (fully "MuxLang") is designed with the following principles:

- **Java-style explicit typing** with **local type inference**
- **Python-style collection literals**
- **Rust-style pattern matching with guards**
- **Curly-brace syntax** and **no semicolons**
- **Minimal trait/class model** using `is` instead of `implements`
- **Built-in `Result<T,E>` and `Optional<T>` for error handling**

## Key Features

### Static Type System

Mux uses strict static typing with no implicit type conversions. All type conversions must be explicit.

```mux title="type_inference.mux"
auto x = 42              // Type inferred as int
auto y = 3.14            // Type inferred as float
auto name = "Mux"        // Type inferred as string

// Explicit conversions required
auto sum = x + y.to_int()           // OK
// auto bad = x + y                 // ERROR: cannot add int and float
```

### No Semicolons

Statements are terminated by end-of-line only:

```mux title="no_semicolons.mux"
auto x = 10
auto y = 20
auto sum = x + y
```

### Pattern Matching

Rust-style pattern matching with guards:

```mux title="pattern_matching.mux"
match value {
    Some(v) if v > 10 {
        print("Large value: " + v.to_string())
    }
    Some(v) {
        print("Small value: " + v.to_string())
    }
    None {
        print("No value")
    }
}
```

### Memory Safety

Reference counting provides deterministic memory management without manual allocation or garbage collection pauses.

## Language Sections

Explore the detailed language documentation:

- [Types](./types.md) - Primitive types, conversions, and the type system
- [Variables](./variables.md) - Variable declarations, constants, and type inference
- [Functions](./functions.md) - Function definitions, parameters, and return types
- [Control Flow](./control-flow.md) - If/else, loops, match statements
- [Classes](./classes.md) - Classes, interfaces, and object-oriented features
- [Enums](./enums.md) - Tagged unions and pattern matching
- [Generics](./generics.md) - Generic functions and classes
- [Collections](./collections.md) - Lists, maps, sets, and collection methods
- [Error Handling](./error-handling.md) - Result and Optional types
- [Memory](./memory.md) - Reference counting and memory model
- [Modules](./modules.md) - Import system and code organization

## Lexical Structure

- **Case-sensitive** identifiers: letters, digits, `_`, not starting with a digit
- **Whitespace** (spaces, tabs, newlines) separates tokens
- **Comments**:
  - Single-line: `// comment`
  - Multi-line: `/* comment */`
- **Statement termination**: by end-of-line only (no semicolons)
- **Underscore placeholder**: `_` can be used for unused parameters, variables, or pattern matching wildcards

### Keywords

`func`, `returns`, `const`, `auto`, `class`, `interface`, `enum`, `match`, `if`, `else`, `for`, `while`, `break`, `continue`, `return`, `import`, `is`, `as`, `in`, `true`, `false`, `common`, `None`, `Some`, `Result`, `Optional`, `Ok`, `Err`

## Type System Principles

**Critical Design Decisions:**

- **NO dynamic typing**
- **NO implicit type conversions**
- **NO runtime reflection**
- All generics must monomorphize at compile time
- Interfaces use static dispatch (no vtables)

## Getting Help

- [Quick Start Guide](../getting-started/quick-start.md)
- [Examples](./examples.md) - Code examples for common patterns
- [GitHub Issues](https://github.com/derekcorniello/mux-lang/issues)
