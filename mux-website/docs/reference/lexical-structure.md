# Lexical Structure

This document describes the low-level lexical structure of Mux: tokens, keywords, identifiers, literals, and comments.

## Source Files

Mux source files use UTF-8 encoding. File extension is `.mux`.

## Case Sensitivity

Mux is case-sensitive. Keywords, identifiers, and type names must match the exact case as defined.

```mux
auto x = 1
auto X = 2  // Different variable
x = 3  // Reassign to existing variable
```

## Whitespace

The only significant whitespace in Mux is the Newline character (`\n`), which separates statements. Other whitespace characters (space, tab, carriage return) are ignored except for separating tokens.

```mux

## Comments

### Single-Line Comments

Single-line comments start with `//` and continue to the end of the line:

```mux
// This is a single-line comment
auto x = 42  // Inline comment
```

### Multi-Line Comments

Multi-line comments start with `/*` and end with `*/`. They can span multiple lines:

```mux
/* This is a
   multi-line comment
   spanning three lines */
auto x = 42  /* Can also be inline */
```

Multi-line comments do not nest. The first `*/` terminates the comment.

## Identifiers

Identifiers name variables, functions, types, and other entities.

### Rules

- Must start with a letter
- Can contain letters, digits, and underscores
- Cannot be a reserved keyword

### Valid Identifiers

```mux
auto x = 1
auto camelCase = 3
auto snake_case = 4
// auto _123 = 5 // ERROR: variable names cannot start with _
auto Greek_alpha = 'α'
```

**Note**: Most Mux code uses `snake_case`, with some exceptions of course, but this is a convention, not a requirement.

### Invalid Identifiers

```mux
auto 123 = 1        // ERROR: starts with digit
auto my-var = 2     // ERROR: hyphen is not allowed
auto class = 3      // ERROR: reserved keyword
```

### Underscore Placeholder

The underscore `_` is a special identifier used as a placeholder:

```mux
func process(int data, int _) returns void {
    // Second parameter is unused
}

// Pattern matching wildcard
match result {
    ok(value) { print(value) }
    err(_) { print("error") }  // Ignore error details
}

// Loop when index is not needed
for int _ in range(0, 10) {
    doSomething()
}
```

The underscore has special semantics:
- Cannot be read (assigning to `_` discards the value)
- Multiple uses of `_` in the same scope do not conflict

## Keywords

The following words are reserved keywords and cannot be used as identifiers:

### Declaration Keywords

| Keyword | Purpose |
|---------|---------|
| `func` | Function declaration |
| `returns` | Return type specification for functions |
| `const` | Constant declaration |
| `auto` | Type inference declaration |
| `class` | Class declaration |
| `interface` | Interface declaration |
| `enum` | Enum declaration |
| `common` | Static/class method |

### Control Flow Keywords

| Keyword | Purpose |
|---------|---------|
| `match` | Pattern matching expression |
| `if` | Conditional expression |
| `else` | Else branch of conditional |
| `for` | Iteration loop |
| `while` | While loop |
| `break` | Exit loop |
| `continue` | Skip to next iteration |
| `return` | Return from function |

### Module Keywords

| Keyword | Purpose |
|---------|---------|
| `import` | Module import |
| `as` | Import alias |

### Operator Keywords

| Keyword | Purpose |
|---------|---------|
| `is` | Type constraint / interface implementation |
| `in` | Membership test |
| `true` | Boolean true literal |
| `false` | Boolean false literal |
| `none` | optional none literal |
| `some` | optional some literal |
| `ok` | result ok literal |
| `err` | result error literal |

### Special Keywords

| Keyword | Purpose |
|---------|---------|
| `self` | Instance reference (inside class methods only) |

## Literals

### Integer Literals

Decimal integers:

```mux
auto x = 42
int y = 42
```

### Float Literals

```mux
auto pi = 3.14159
auto scientific = 1.23e-4
```

### Character Literals

Single Unicode code point in single quotes:

```mux
auto a = 'a'
auto newline = '\n'
auto unicode = 'α'
auto quote = '\''    // Escaped single quote
auto backslash = '\\'
```

### String Literals

UTF-8 sequences in double quotes:

```mux
auto greeting = "Hello, World!"
auto with_quote = "She said \"hello\""
auto multiline = "line1\nline2"
```

Escape sequences:
- `\\` - Backslash
- `\"` - Double quote
- `\'` - Single quote
- `\n` - Newline
- `\t` - Tab
- `\r` - Carriage return

### Boolean Literals

```mux
auto yes = true
bool no = false
```

### none Literal

```mux
auto absent = none
```

## Operators and Punctuation

### Arithmetic Operators

| Symbol | Meaning |
|--------|---------|
| `+` | Addition |
| `-` | Subtraction / Negation |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |
| `**` | Exponentiation |

### Comparison Operators

| Symbol | Meaning |
|--------|---------|
| `==` | Equality |
| `!=` | Inequality |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

### Logical Operators

| Symbol | Meaning |
|--------|---------|
| `&&` | Logical AND (short-circuit) |
| `||` | Logical OR (short-circuit) |
| `!` | Logical NOT |

### Other Operators

| Symbol | Meaning |
|--------|---------|
| `=` | Assignment |
| `+=` | Compound assignment |
| `-=` | Compound assignment |
| `*=` | Compound assignment |
| `/=` | Compound assignment |
| `%=` | Compound assignment |
| `++` | Postfix increment |
| `--` | Postfix decrement |
| `.` | Member access |
| `&` | Reference creation |
| `*` | Dereference |

### Punctuation

| Symbol | Usage |
|--------|-------|
| `(` `)` | tuple literals, Grouping, function calls, parameters |
| `[` `]` | List literals, indexing |
| `{` `}` | Block statements, map literals, set literals |
| `<` `>` | Type parameters (generics) |
| `,` | Separator |

## Line Continuation

Mux does not support implicit line continuation. Each statement must be on its own line. The only way to continue an expression across lines is with explicit grouping:

```mux
// Valid: continuation with parentheses
auto sum = (1 + 2 +
           3 + 4)

// Invalid without grouping:
// auto sum = 1 + 2
//          + 3 + 4  // ERROR
```

## See Also

- [Language Guide](../language-guide/overview.md) - Practical examples
