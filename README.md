
  ![MuxLang Version](https://img.shields.io/badge/MuxLang-0.1.2-4c1?style=for-the-badge&link=https://github.com/DerekCorniello/mux-lang/releases)&nbsp;
  ![Rust](https://img.shields.io/badge/Rust-000000?style=for-the-badge&logo=rust&logoColor=white&link=https://www.rust-lang.org/)&nbsp;
  ![LLVM](https://img.shields.io/badge/LLVM-262D3A?style=for-the-badge&logo=llvm&logoColor=white&link=https://llvm.org/)&nbsp;
  ![crates.io](https://img.shields.io/badge/crates.io-MuxLang-orange?style=for-the-badge&logo=rust&link=https://crates.io/crates/mux-lang)&nbsp;
  ![Docs](https://img.shields.io/badge/docs-online-blue?style=for-the-badge&logo=readthedocs&logoColor=white&link=https://mux-lang.dev)&nbsp;
  ![Status](https://img.shields.io/badge/status-alpha-lightgrey?style=for-the-badge&link=https://github.com/DerekCorniello/mux-lang)

<p align="center">
  <img src="./mux-website/static/img/mux-logo.png" alt="Mux Logo" style="width: 10%; height: auto;">
</p>

# Mux - A Programming Language For The People

By Derek Corniello

## Why Mux?

- **Simple yet powerful:** Combines Go-like minimalism with Rust-inspired safety.
- **Strong static typing:** Helps catch errors early and ensures safer code.
- **LLVM-powered:** Fast compilation and native performance.
- **Flexible memory management:** Ease of use through reference counting.
- **Extensible:** Designed to evolve with features like traits, concurrency, and a standard library.

## Quick Start Guide

Check out the [docs](https://mux-lang.dev)!

### Runtime Setup

Mux builds a small runtime library the first time you compile or run a program. If you want to do this up front, run:

```bash
mux doctor
```

This verifies dependencies and builds the runtime if it is missing.

## Wanna Contribute?

Mux is an open-source project, and I welcome contributions from the community! Whether you're interested in adding new features, fixing bugs, improving documentation, or helping with testing, your contributions are valuable. Please check out the [CONTRIBUTING.md](CONTRIBUTING.md) file for guidelines on how to get started.

## Important Notes!

While I take pride in this project, please be aware that Mux is still in its early stages of development. The language specification, compiler, and tooling are actively evolving. Expect breaking changes and incomplete features as I work towards a stable release.

I also want to acknowlege that I am aware that there are likely far better ways to do some of the things I have done here. This is a personal project and learning experience for me, and I appreciate your understanding as I continue to improve Mux. 

Finally, I want to acknowledge that I have also been using this project as a way to experiment with AI tools to help me write, review, test and document code. While I have made every effort to ensure the accuracy and quality of the content, there may be occasional "bad code", errors, or inconsistencies. I appreciate your understanding as I continue to refine both the language and my use of these tools.

# Mux Language Specification

## 1. Overview

Mux (fully "MuxLang") is a statically-typed, reference-counted language that combines:

- **Java-style explicit typing** with **local type inference**
- **Python-style collection literals**
- **Rust-style pattern-matching with guards**
- **Curly-brace syntax** and **no semicolons**
- **Minimal trait/Class model** (use `is` instead of `implements` like Java)
- **Built-in `result<T,E>` and `optional<T>` for error handling**

---

## 2. Lexical Structure

- **Case-sensitive** identifiers: letters, digits, `_`, not starting with a digit
- **Whitespace** (spaces, tabs, newlines) separates tokens
- **Comments**:
  - Single-line: `// comment`
  - Multi-line: `/* comment */`
- **Statement termination**: by end-of-line only (no semicolons)
- **Underscore placeholder**: `_` can be used for unused parameters, variables, or pattern matching wildcards
- **Keywords**:
  `func`, `returns`, `const`, `auto`, `class`, `interface`, `enum`, `match`, `if`, `else`, `for`, `while`, `break`, `continue`, `return`, `import`, `is`, `as`, `in`, `true`, `false`, `common`, `none`, `optional`, `result`, `ok`, `err`

---

## 3. Types

**Type System**: Mux uses **strict static typing** with **NO implicit type conversions**. All type conversions must be explicit using conversion methods.

### 3.1 Primitive Types

```
int      // 64-bit signed integer
float    // 64-bit IEEE-754
bool     // true | false
char     // Unicode code point
string   // UTF-8 sequence
```

### 3.2 Type Conversions

Mux requires **explicit type conversions** for all operations. There are no implicit conversions between types.

#### 3.2.1 Numeric Conversions

```mux
// Integer conversions
auto x = 42
auto x_float = x.to_float()     // int -> float
auto x_str = x.to_string()      // int -> str
auto x_same = x.to_int()        // int -> int (identity)

// Float conversions
auto pi = 3.14
auto pi_int = pi.to_int()       // float -> int (truncates: 3)
auto pi_str = pi.to_string()    // float -> str
auto pi_same = pi.to_float()    // float -> float (identity)

// Boolean conversions
auto flag = true
auto flag_int = flag.to_int()   // bool -> int (true=1, false=0)
auto flag_float = flag.to_float() // bool -> float (true=1.0, false=0.0)
auto flag_str = flag.to_string() // bool -> string ("true" or "false")

// Char conversions
auto ch = 'A'
auto ch_str = ch.to_string()    // char -> str

// Method calls on literals require parentheses
auto num = (3).to_string()      // Valid
auto val = (42).to_float()      // Valid
// auto bad = 3.to_string()     // ERROR: parsed as float 3.0
```

#### 3.2.2 String Parsing (Fallible Conversions)

String and char parsing methods return `result<T, string>` because they can fail:

```mux
// String to number (returns result)
auto num_str = "42"
auto result = num_str.to_int()
match result {
    ok(value) {
        print("Parsed: " + value.to_string())  // "Parsed: 42"
    }
    err(error) {
        print("Parse error: " + error)
    }
}

// String to float
auto float_str = "3.14159"
auto float_result = float_str.to_float()
match float_result {
    ok(value) { print(value.to_string()) }
    err(msg) { print("Error: " + msg) }
}

// Char to digit (only works for '0'-'9')
auto digit_char = '5'
auto digit_result = digit_char.to_int()
match digit_result {
    ok(digit) { print(digit.to_string()) }  // "5"
    err(msg) { print(msg) }
}

auto letter = 'A'
auto letter_result = letter.to_int()
match letter_result {
    ok(_) { print("Unexpected success") }
    err(msg) { print(msg) }  // "Character is not a digit (0-9)"
}
```

#### 3.2.3 No Implicit Conversions

The following operations are **compile-time errors**:

```mux
// Type mismatches in binary operations
auto bad1 = 1 + 1.0        // ERROR: cannot add int and float
auto bad2 = "hello" + 3    // ERROR: cannot add string and int
auto bad3 = true + false   // ERROR: cannot add bool and bool

// Type mismatches in comparisons
auto bad4 = 1 < 1.0        // ERROR: cannot compare int and float
auto bad5 = "a" == 1       // ERROR: cannot compare string and int

// Function argument type mismatches
func takes_string(string s) returns void { }
takes_string(123)          // ERROR: expected string, got int
// Correct usage requires explicit conversion
auto good1 = 1 + (1.0).to_int()           // OK: 2
auto good2 = "hello" + (3).to_string()    // OK: "hello3"
auto good3 = 1.to_float() < 1.0           // OK: true
auto good4 = (true).to_int() + (false).to_int()  // OK: 1
```

#### 3.2.4 Available Conversion Methods

| From Type | Method | Returns | Notes |
|-----------|--------|---------|-------|
| `int` | `.to_string()` | `string` | Converts to string representation |
| `int` | `.to_float()` | `float` | Converts to floating-point |
| `int` | `.to_int()` | `int` | Identity function |
| `float` | `.to_string()` | `string` | Converts to string representation |
| `float` | `.to_int()` | `int` | Truncates decimal part |
| `float` | `.to_float()` | `float` | Identity function |
| `bool` | `.to_string()` | `string` | Returns "true" or "false" |
| `bool` | `.to_int()` | `int` | Returns 1 or 0 |
| `bool` | `.to_float()` | `float` | Returns 1.0 or 0.0 |
| `char` | `.to_string()` | `string` | Converts char to string |
| `char` | `.to_int()` | `result<int, string>` | Digit value for '0'-'9' only |
| `string` | `.to_string()` | `str` | Identity function |
| `string` | `.to_int()` | `result<int, string>` | Parses string as integer |
| `string` | `.to_float()` | `result<float, string>` | Parses string as float |

### 3.2.5 Technical Design: Uniform Value Representation

Mux uses a **single unified type** (`Value` enum) to represent all runtime values, enabling uniform handling in collections and generics.

#### The Value Enum

```rust
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(OrderedFloat<f64>),
    String(String),
    List(Vec<Value>),
    Map(BTreeMap<Value, Value>),
    Set(BTreeSet<Value>),
    Tuple(Box<Tuple>),
    optional(Option<Box<Value>>),
    result(result<Box<Value>, String>),
    Object(ObjectRef),
}
```

#### Boxing Strategy

All primitives are **boxed** into `*mut Value` pointers:

1. **Allocation**: `mux_rc_alloc(value)` allocates RefHeader + Value
2. **Storage**: Pointers stored in variables, collections, and function returns
3. **Extraction**: Typed accessors (`mux_value_get_int`, etc.) unwrap values

This design enables:
- **Generic collections**: `list<T>` works uniformly for all types
- **Polymorphic functions**: Same function can handle any type
- **Simple FFI**: C code receives consistent `void*` pointers

#### Type Representations Through Compilation

Mux maintains **three distinct type representations**:

| Representation | Purpose |
|---------------|---------|
| `TypeNode` | AST representation, source locations |
| `Type` | Semantic analysis, type resolution |
| `BasicTypeEnum` | LLVM IR generation |

The separation enables error reporting with source locations while keeping semantic analysis LLVM-independent.

### 3.3 Built-in Functions

Mux provides essential built-in functions for output and utility operations. These are always available without imports.

#### Output Functions

**Design Note:** `print` is a direct runtime call that outputs to stdout. The runtime handles string formatting and newline appending.

#### Utility Functions

**`range(int start, int end) -> list<int>`** - Returns a list of integers from `start` (inclusive) to `end` (exclusive). The result is always a `list<int>`.

```mux
// Generate indices for iteration
for i in range(0, 5) {
    print(i.to_string())  // Prints 0, 1, 2, 3, 4
}

// Create a list of numbers
auto numbers = range(10, 15)  // [10, 11, 12, 13, 14]
```

**Design Note:** `range()` is the primary way to create numeric sequences for iteration, as Mux does not support C-style `for (int i = 0; i < n; i++)` loops.

#### Input Functions

**`read_line() -> string`** - Reads a line from standard input and returns it as a string (excluding the newline).

```mux
print("Enter your name: ")
auto name = read_line()
print("Hello, " + name)
```

### 3.4 Composite Types

```
optional<T>
result<T, E>
list<T>
map<K,V>
set<T>
tuple<T, U>

### 3.4.1 Tuples

Tuples are fixed size pairs. A tuple always has exactly two elements.

```mux
auto pair = (1, "one")
tuple<int, string> typed = (2, "two")

print(pair.left.to_string())   // "1"
print(pair.right.to_string())  // "one"
```

Tuples also support `to_string()` and a default constructor:

```mux
auto empty = tuple<int, string>.new()  // (0, "")
```
```

### 3.5 Generics

Mux supports Go/Rust-style generics with type parameters and interface bounds using the `is` keyword:

```mux
// Generic function with type constraints
func max<T is Comparable>(T a, T b) returns T {
    if a > b {
        return a
    }
    return b
}

// Generic function with Stringable bound for to_string()
func greet<T is Stringable>(T value) returns string {
    return "Hello, " + value.to_string()
}

// Arithmetic operators are builtin for primitive numeric types.
// Use explicit methods or domain-specific APIs for generic composition.

// Generic class
class Stack<T> {
    list<T> items
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns optional<T> {
        if self.items.is_empty() { return none }
        return self.items.pop_back()
    }
}
```

### 3.5.1 Technical Design: Monomorphization

Mux uses **compile-time monomorphization** for generics, generating specialized code for each type instantiation.

#### Monomorphization Process

1. **Type inference**: Determine concrete types from function arguments
2. **Name generation**: Create unique identifier: `FunctionName$int$string$`
3. **Type substitution**: Replace type parameters with concrete types
4. **Code generation**: Emit specialized function body
5. **Caching**: Store generated methods to avoid regeneration

#### Example

```mux
func identity<T>(T value) returns T {
    return value
}

auto a = identity(42)        // Generates: identity$int
auto b = identity("hello")   // Generates: identity$string
```

The compiler substitutes types in the AST before code generation:

```rust
// Original generic function
func identity<T>(T value) returns T { ... }

// After substitution for T = int
func identity$int(int value) returns int { ... }
```

#### Why Monomorphization?

- **Zero runtime cost**: No boxing, no vtables, no type checks
- **Static dispatch**: Methods resolved at compile time
- **LLVM optimization**: Each specialization can be fully optimized

The tradeoff is increased code size (one copy per type combination).

### 3.6 Built-in Interfaces

Mux provides built-in interfaces (traits) for common operations:

| Interface | Callable Method | Description |
|-----------|----------------|-------------|
| `Stringable` | `.to_string()` | Types that can be converted to string |
| `Equatable` | (none) | Enables `==` and `!=` operators |
| `Comparable` | (none) | Enables `<`, `>`, `<=`, `>=` operators |
| `Hashable` | (none) | Types that can be used as set/map keys |
| `Error` | `.message()` | Types that can be used as `result<T, E>` error values |

**Note:** For `Equatable` and `Comparable`, the methods are marker interfaces - you cannot call `.eq()` or `.cmp()` on built-in types. Use the operators directly (`==`, `<`, etc.) instead.

**Primitives and Interfaces:**
- `int`: Implements `Stringable`, `Equatable`, `Comparable`, `Hashable`
- `float`: Implements `Stringable`, `Equatable`, `Comparable`, `Hashable`
- `string`: Implements `Stringable`, `Equatable`, `Comparable`, `Hashable`, `Error`
- `bool`: Implements `Stringable`, `Equatable`, `Hashable`
- `char`: Implements `Stringable`, `Equatable`, `Comparable`, `Hashable`

`result<T, E>` requires `E` to implement `Error`.

**Example: Custom Type Implementing Interface**
```mux
interface Equatable {
    func eq(Self) returns bool
}

class Point is Equatable {
    int x
    int y
    
    func eq(Point other) returns bool {
        return self.x == other.x && self.y == other.y
    }
}
    }
}

auto p1 = Point.new(1, 2)
auto p2 = Point.new(1, 2)
auto same = p1 == p2  // true
```

### 3.7 Generic Type Constraints

```mux
// Using built-in interfaces
func process<T is Stringable>(list<T> items) returns void {
    for item in items {
        print(item.to_string())
    }
}

// Multiple bounds (AND semantics - type must implement all)
func combine<T is Comparable & Stringable>(T a, T b) returns string {
    if a < b {
        return a.to_string() + " < " + b.to_string()
    }
    return a.to_string() + " >= " + b.to_string()
}

// Type parameters must be explicitly specified
auto max_int = max<int>(3, 7)           // T = int, Comparable bound satisfied
auto names = ["apple", "banana"]
print(greet<string>(names[0]))          // T = string, Stringable bound satisfied
```

### 3.8 User-Defined Types

- **Structs**: simple aggregates
- **Enums**: tagged unions (see §8)
- **Classes**: with fields + methods (see §9)

---

## 4. Variable & Constant Declarations

### 4.1 Explicit Typing

```
const int MAX = 100

// Variables (explicit type required for declarations without inference)
int x = 5
bool flag = true
string name = "MuxLang"
```

### 4.2 Variable Declarations

Mux supports both explicit types and type inference with `auto`:

```
// Type inferred with 'auto'
auto x = 42          // inferred as int
auto pi = 3.14159    // inferred as float
auto name = "Mux"    // inferred as str

// Explicit type annotation
int count = 0
list<string> names = []
map<string, string | int> user = {"name": "Alice", "age": 30}

// Valid inference
auto value = someFunction()
auto numbers = [1, 2, 3]
map<string, string> userMap = {"key": "value"}

// Invalid - no initializer with 'auto'
auto x  // ERROR: cannot infer type without initializer

// Function parameters must be explicitly typed
func process(auto item) returns void { }  // ERROR
func process(int item) returns void { }   // Valid

// Unused parameter
func process(int item, int _) returns void { }  // second parameter unused
```

All declarations require either an explicit type or `auto` with an initializer; semicolons are not used.

### 4.3 Constants

Constants are immutable values that cannot be reassigned or modified after initialization:

```
// Function-level constants
func calculate() returns int {
    const int MULTIPLIER = 10
    const float TAX_RATE = 0.08
    int value = 100
    return value * MULTIPLIER
}

// Constants in classes
class Config {
    const int MAX_RETRIES
    int current_retry
    
    func increment() returns void {
        self.current_retry++  // OK - mutable field
        // self.MAX_RETRIES++  // ERROR: Cannot modify const field 'MAX_RETRIES'
    }
}

auto cfg = Config.new()
cfg.current_retry = 1  // OK - mutable field
// cfg.MAX_RETRIES = 5  // ERROR: Cannot assign to const field 'MAX_RETRIES'
```

**Const Enforcement:**
- Cannot reassign: `const_var = new_value` -> ERROR
- Cannot use compound assignment: `const_var += 1` -> ERROR
- Cannot increment/decrement: `const_var++` or `const_var--` -> ERROR
- Applies to both identifiers and class fields
- Use `const` when you want a value that won't change after initialization

---

## 5. Functions

```
func add(int a, int b) returns int {
    return a + b
}

func greet(string name, int times = 1) returns void {
    for i in range(0, times) {
        print("Hello, " + name)
    }
}

func processData() returns map<string, int> {
    map<string, int> results = {"processed": 100, "skipped": 5}
    auto total = results["processed"] + results["skipped"]
    results["total"] = total
    return results
}

// Function with unused parameters
func callback(string event, int timestamp, string _) returns void {
    print("Event: " + event + " at " + timestamp)
    // third parameter ignored
}
```

- Keyword `func`
- Parameter list with explicit types; default values optional
- `returns` clause for return type (explicit, no inference)
- Body enclosed in `{…}`; no semicolons
- Local variables within functions can use `auto` inference
- Use `_` for unused parameters

---

## 6. Operators

### 6.1 Arithmetic Operators

Mux supports standard arithmetic operations with strict type requirements (no implicit conversions):

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `+` | Addition | `int`, `float`, `string` | `5 + 3`, `"a" + "b"` |
| `-` | Subtraction | `int`, `float` | `10 - 4` |
| `*` | Multiplication | `int`, `float` | `6 * 7` |
| `/` | Division | `int`, `float` | `15 / 3` |
| `%` | Modulo | `int`, `float` | `10 % 3` |
| `**` | Exponentiation | `int`, `float` | `2 ** 3` |

**Exponentiation Operator (`**`):**
- Right-associative: `2 ** 3 ** 2` equals `2 ** (3 ** 2)` = 512
- Higher precedence than `*` and `/`: `2 * 3 ** 2` equals `2 * 9` = 18
- Works on both `int` and `float` types

```mux
auto squared = 5 ** 2        // 25
auto cubed = 2.0 ** 3.0      // 8.0
auto complex = 2 ** 3 ** 2   // 512 (right-associative)
```

### 6.2 Increment and Decrement Operators

Mux provides postfix increment (`++`) and decrement (`--`) operators with specific design constraints:

**Design Constraints:**
- **Postfix only**: `counter++` is valid, `++counter` is not supported
- **Standalone only**: Must appear on their own statement line, not within expressions
- **Only on mutable variables**: Cannot be used on `const` declarations or literals
- **Type preservation**: Operates on `int` types only

```mux
auto counter = 0
counter++         // Valid: counter is now 1
counter--         // Valid: counter is now 0

// INVALID - cannot use in expressions:
// auto x = counter++    // ERROR: ++ cannot be used in expressions
// auto y = (counter++) + 5  // ERROR: standalone only

// INVALID - prefix not supported:
// ++counter            // ERROR: prefix increment not supported

// INVALID - cannot modify const:
const int MAX = 100
// MAX++               // ERROR: cannot modify const
```

**Rationale:** The postfix-only, standalone-only design prevents ambiguity and side-effect confusion that can occur with prefix operators or expression-embedded increments.

### 6.3 Comparison and Logical Operators

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `==` | Equality | All comparable types | `a == b` |
| `!=` | Inequality | All comparable types | `a != b` |
| `<` | Less than | `int`, `float`, `string` | `5 < 10` |
| `<=` | Less than or equal | `int`, `float`, `string` | `x <= 100` |
| `>` | Greater than | `int`, `float`, `string` | `y > 0` |
| `>=` | Greater than or equal | `int`, `float`, `string` | `age >= 18` |
| `&&` | Logical AND | `bool` | `a && b` (short-circuit) |
| `\|\|` | Logical OR | `bool` | `a \|\| b` (short-circuit) |
| `!` | Logical NOT | `bool` | `!flag` |

**Short-circuit Evaluation:**
- `&&` only evaluates right side if left is `true`
- `||` only evaluates right side if left is `false`

### 6.3.1 Technical Design: Short-Circuit Logical Operators

The `&&` and `||` operators use **LLVM control flow** for short-circuit evaluation, not simple boolean operations.

#### Phi Nodes for result Merging

Phi nodes select a value based on which predecessor block executed:

```llvm
%result = phi i1 [ 0, %left_block ], [ %b_value, %right_block ]
```

- From `left_block`: constant `0` (left was false)
- From `right_block`: computed `%b_value` (left was true)

#### Why This Approach?

If LLVM generated `a && b` as a single expression:
1. Both `a` and `b` would always be evaluated (no short-circuit)
2. No opportunity for branch prediction
3. Can't exploit constant operands

The basic block approach preserves semantics while enabling LLVM optimizations (dead code elimination, inlining, vectorization).

### 6.4 The `in` Operator

The `in` operator tests for membership/containment with strict type requirements:

| Left Operand | Right Operand | Description |
|--------------|---------------|-------------|
| `T` | `list<T>` | Check if value exists in list |
| `T` | `set<T>` | Check if value exists in set |
| `string` | `string` | Check if substring exists |
| `char` | `string` | Check if character exists in string |

**Type Constraints:**
- Both operands must have compatible element types
- No implicit type conversions allowed
- Returns `bool`

```mux
// List containment
auto nums = [1, 2, 3, 4, 5]
auto hasThree = 3 in nums     // true
auto hasTen = 10 in nums      // false

// Set containment
auto tags = {"urgent", "important"}
auto isUrgent = "urgent" in tags    // true

// String containment
auto msg = "hello world"
auto hasWorld = "world" in msg      // true
auto hasFoo = "foo" in msg          // false

// Character in string
auto hasO = 'o' in msg              // true
auto hasZ = 'z' in msg              // false

// INVALID - type mismatch:
// auto bad = "1" in nums           // ERROR: string not in list<int>
// auto bad2 = 1 in msg             // ERROR: int not in string
```

### 6.5 Collection Operators

#### Concatenation with `+`

The `+` operator is overloaded for collection types with type-specific semantics:

| Types | Operation | result |
|-------|-----------|--------|
| `list<T> + list<T>` | Concatenation | Combined list |
| `map<K,V> + map<K,V>` | Merge | Combined map (latter overwrites former on key collision) |
| `set<T> + set<T>` | Union | Set containing all unique elements |
| `string + string` | Concatenation | Combined string |

### 6.5.1 Technical Design: Operator Overloading

Operators are built-in for primitive types and collections.

#### Operator Mapping

| Operator | Types |
|----------|-------|
| `+` | int, float, string, list, map, set |
| `-` | int, float, set (difference) |
| `*` | int, float |
| `/` | int, float, set (intersection) |
| `%` | int, float |
| `**` | int, float |
| `==` | all types |
| `<`, `>`, `<=`, `>=` | int, float, string, char |

#### Semantic Validation

The semantic analyzer checks operator types:

```rust
// For a + b
let left_type = analyzer.get_expression_type(left_expr)?;
let right_type = analyzer.get_expression_type(right_expr)?;

if !type_supports_addition(&left_type) {
    return err(format!("Type {} does not support +", left_type));
}
```

#### Code Generation

For primitive types, direct LLVM operations:

```llvm
%result = add i64 %a, %b
```

Arithmetic operators are not dispatched through `Add`/`Sub`/`Mul`/`Div` interfaces.
They are validated semantically and lowered to builtin operations for supported types.

**Type Constraints:**
- Both operands must be the exact same collection type
- No mixing of collection types (e.g., `list + set` is an error)
- No implicit element type conversions

```mux
// List concatenation
auto list1 = [1, 2]
auto list2 = [3, 4]
auto combined = list1 + list2    // [1, 2, 3, 4]

// Map merge (latter wins on key collision)
auto map1 = {"a": 1, "b": 2}
auto map2 = {"b": 3, "c": 4}     // Note: key "b" exists in both
auto merged = map1 + map2        // {"a": 1, "b": 3, "c": 4}

// Set operators
auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}
auto diff = set1 - set2          // {1, 2}
auto common = set1 / set2        // {3}

// String concatenation
auto greeting = "Hello, " + "World"  // "Hello, World"

// INVALID - type mismatch:
// auto bad = [1, 2] + {3, 4}       // ERROR: cannot add list and set
// auto bad2 = [1, 2] + [3.0, 4.0]  // ERROR: list<int> + list<float>
```

---

## 7. Lambdas & Closures

```
// Block-form lambda with explicit types and return type
auto square = func(int n) returns int {
    return n * n
}

auto doubler = func(float x) returns float {
    return x * 2.0
}

// Passing lambdas to functions
auto result = apply(10, func(int x) returns int {
    return x + 5
})

// Lambda with unused parameters
auto processFirst = func(int first, int _) returns int {
    return first * 2  // second parameter ignored
}

// Block-form lambda with mixed explicit/inferred types
auto filter = func(list<int> nums, func(int) returns bool cond) returns list<int> {
    list<int> out = []
    for n in nums {
        if cond(n) {
            out.push_back(n)
        }
    }
    return out
}
```

- All lambdas use block syntax with `func(params) { ... }`
- Lambda parameters can use `auto` when type can be inferred from context
- Use `_` for unused lambda parameters
- optional capture list in `[…]` form [needs more clarification]

---

## 8. Control Flow

### 8.1 If / Else

```
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}

// With type inference
auto message = if x > 0 { "positive" } else { "non-positive" }
```

### 8.2 Match with Guards

```
match (value) {
    some(v) if v > 10 {
        auto msg = "large: " + v  // local inference
        print(msg)
    }
    some(v) {
        print("small: " + v)
    }
    none {
        print("no value")
    }
    _ {
        print("unexpected case")  // wildcard pattern
    }
}
```

### 8.2.1 Match as Switch Statement

Match statements can be used as switch statements for any type:

```
// Match on int literals (like a switch)
auto status = 200
match status {
    200 { print("OK") }
    404 { print("Not Found") }
    500 { print("Server Error") }
    _ { print("Unknown status") }
}

// Match on string literals
auto command = "start"
match command {
    "start" { print("Starting...") }
    "stop" { print("Stopping...") }
    "restart" { print("Restarting...") }
    _ { print("Unknown command") }
}

// Variable binding in patterns
auto value = 42
match value {
    1 { print("one") }
    captured { print("got: " + captured.to_string()) }
    _ { print("other") }
}

// List literal matching
auto nums = [1, 2, 3]
match nums {
    [] { print("empty") }
    [1, 2, 3] { print("three elements") }
    [first, ..rest] { print("has elements") }
}

// Switch with guards
auto score = 85
match score {
    n if n >= 90 { print("A") }
    n if n >= 80 { print("B") }
    n if n >= 70 { print("C") }
    n if n >= 60 { print("D") }
    _ { print("F") }
}
```

### 8.3 For Loops

```
for item in myList {
    auto processed = transform(item)  // type inferred
    print(processed)
}

// Iterator with inference
for item in collection {
    // item type inferred from collection element type
    process(item)
}

// Ignoring loop variables when not needed
for _ in range(0, 10) {
    doSomething()  // don't care about the index
}

// Destructuring in loops with unused parts
for (key, _) in keyValuePairs {
    print("Key: " + key)  // value ignored
}
```

### 8.4 While Loops

```
while cond {
    auto currentTime = getCurrentTime()  // local inference
    // ...
}
```

### 8.5 Break / Continue / Return

Works as in C/Java.

---

## 9. Enums (Tagged Unions)

```
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}
```

```
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

// Usage with inference
auto myShape = Circle.new(5.0)  // type inferred as Shape
list<Shape> shapes = [Circle.new(1.0), Rectangle.new(2.0, 3.0)]

// Pattern matching with unused enum data
match (shape) {
    Circle(_) {
        print("It's a circle")  // radius value ignored
    }
    Rectangle(width, _) {
        print("Rectangle with width: " + width)  // height ignored
    }
    Square(size) {
        print("Square with size: " + size)
    }
}
```

Each variant may carry data. Pattern-match with destructuring and guards. Use `_` to ignore unused enum data in patterns.

---

## 10. Classes & Traits

### 10.1 Traits (Interfaces)

```
interface Drawable {
    func draw() returns void
}
```

### 10.2 Classes with `is` Clause

```
class Circle is Drawable, ShapeLike {
    float radius  // explicit type required for fields

    func draw() returns void {
        auto message = "Circle radius=" + radius  // local inference in methods
        print(message)
    }

    func area() returns float {
        const float PI = 3.1415  // inferred as float
        return PI * radius * radius
    }
    
    // Method with unused parameters
    func resize(float newRadius, string _) returns void {
        radius = newRadius  // second parameter ignored
    }
}

// Generic class example
class Stack<T> {
    list<T> items

    func push(T item) returns void {
        items.push_back(item)
    }

    func pop() returns optional<T> {
        if items.isEmpty() {
            return none
        }
        auto item = items.pop_back()
        return some(item)
    }
}

// Usage with inference
auto circle = Circle.new(5.0)  // type inferred as Circle
list<Drawable> shapes = [circle]
Stack<int> intStack = Stack<int>.new()  // explicit generic instantiation with .new()
```

### 10.3 Static Methods with `common`

Mux uses the `common` keyword to declare static (class-level) methods that can be called without an instance. This is distinct from `const` which declares immutable values.

**`common` vs `const`:**

| Keyword | Purpose | Usage | Example |
|---------|---------|-------|---------|
| `common` | Static methods and factory functions | Called on the class itself, not instances | `ClassName.method()` |
| `const` | Immutable constants | Values that cannot change after initialization | `const int MAX = 100` |

```mux
class Stack<T> {
    list<T> items
    
    // Instance method - called on instances
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    // Static method - called on the class
    common func who_am_i() returns string {
        return "I'm a Stack!"
    }
    
    // Factory pattern - static method that creates instances
    common func from(list<T> init_list) returns Stack<T> {
        auto new_stack = Stack<T>.new()
        new_stack.items = init_list
        return new_stack
    }
}

// Calling static methods
print(Stack.who_am_i())                    // "I'm a Stack!"
auto s = Stack<int>.from([1, 2, 3])        // Factory method

// Calling instance methods
auto stack = Stack<int>.new()
stack.push(42)                             // Instance method
```

**Key Differences:**
- **Instance methods** (no keyword) operate on `self` and require an instance
- **Static methods** (`common`) have no `self` and are called on the class
- **Const fields** are immutable instance/class fields, not methods
- Static methods cannot access instance fields (no `self` context)
- Factory patterns commonly use `common func from(...)` to create instances with pre-populated data

### 10.4 Class Instantiation

Classes are instantiated using the `.new()` method pattern:

```mux
// Basic instantiation
auto circle = Circle.new()           // No constructor arguments
auto circle2 = Circle.new(5.0)       // With constructor arguments

// Generic class instantiation
auto int_stack = Stack<int>.new()
auto string_stack = Stack<string>.new()

// Using factory methods
auto prebuilt = Stack<int>.from([1, 2, 3])
auto pair = Pair<string, int>.from("key", 42)
```

### 10.5 Technical Design: Object System

Mux objects use Rust's `Rc<Arc<ObjectData>>` pattern for shared ownership with type information.

#### ObjectRef Structure

```rust
struct ObjectData {
    ptr: *mut c_void,      // User's object data
    type_id: TypeId,       // Runtime type identifier
    size: usize,           // Size for deallocation
    ref_count: AtomicUsize, // Reference count
}

struct ObjectRef {
    data: Rc<ObjectData>,  // Shared ownership
}
```

#### Type Registry

```rust
lazy_static::lazy_static! {
    static ref TYPE_REGISTRY: Mutex<HashMap<TypeId, ObjectType>> = ...
    static ref NEXT_TYPE_ID: AtomicUsize = ...
}

pub struct ObjectType {
    pub id: TypeId,
    pub name: String,
    pub size: usize,
    pub destructor: Option<fn(*mut c_void)>,
}
```

Each class registers with the runtime, receiving a unique `TypeId`.

#### Allocation

```rust
pub fn alloc_object(type_id: TypeId) -> *mut Value {
    let obj_type = TYPE_REGISTRY.lock().get(&type_id);
    let size = obj_type.size;
    
    let ptr = std::alloc::alloc(size);
    let obj_ref = ObjectRef::new(ptr, type_id, size);
    
    mux_rc_alloc(Value::Object(obj_ref))
}
```

#### Why This Design?

- **Type information at runtime**: `type_id` enables type checks and casts
- **Proper cleanup**: `size` and optional destructor for cleanup
- **Shared ownership**: Multiple references to same object

**Design Note:** Mux uses explicit `.new()` rather than direct constructor calls to distinguish class instantiation from function calls and enum variant construction.

### 10.4.1 Technical Design: Interface Dispatch (Static)

Mux uses **static dispatch** for interfaces - no runtime vtable lookup.

#### VTable Generation

VTables are generated at compile time:

```llvm
@vtable_Circle = constant {
    i32,           // type tag
    void (i8*)*   // draw method pointer
} {
    i32 1,         // Circle's type ID
    void (i8*)* @Circle.draw
}
```

#### Method Name Mangling

Methods are prefixed with their class name:

```mux
class Circle {
    func draw() returns void { ... }
}
```

Generates LLVM function: `Circle.draw`

#### Why Static Dispatch?

- **Zero cost**: No pointer indirection, direct function calls
- **Inlining**: LLVM can inline interface methods
- **Optimization**: Better branch prediction, no indirect jumps

#### Comparison to Dynamic Dispatch

```rust
// Dynamic dispatch (Python, Java)
circle.draw()  // Look up vtable, find slot, call

// Static dispatch (Mux)
Circle.draw()  // Direct call to Circle.draw
```

The tradeoff: interfaces cannot be added to types from other modules (no "extension traits").

---

## 11. Collections & Literals

```
// Explicit typing
list<int> nums = [1, 2, 3, 4]
map<string, int> scores = {"Alice": 90, "Bob": 85}

// With type inference
auto nums = [1, 2, 3, 4]           // inferred as list<int>
map<string, int> scores = {"Alice": 90, "Bob": 85}
// mixed = [1, 2.5, 3]           // ERROR: conflicting types, explicit type needed

// Nested collections
list<list<int>> matrix = [[1, 2], [3, 4]]
map<string, list<int>> lookup = {"users": [1, 2, 3], "admins": [4, 5]}

// Complex nested structures
auto users = [
    {"name": "Alice", "scores": [95, 87, 92]},
    {"name": "Bob", "scores": [78, 85, 90]}
]  // inferred as list<map<string, string | list<int>>>

auto data = {
    "numbers": [1, 2, 3, 4, 5],
    "metadata": {"version": "1.0", "count": 5}
}  // inferred as map<string, list<int> | map<string, string | int>>

// Generic collections
list<Pair<int, string>> pairs = [Pair.new(1, "one"), Pair.new(2, "two")]
list<Container<int>> containers = list<Container<int>>()
```

### 11.1 Collection Methods

All collections provide a consistent API for access, mutation, and inspection.

#### List Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Returns the number of elements in the list |
| `.is_empty()` | `bool` | Returns `true` if list has no elements |
| `.get(int index)` | `optional<T>` | Safe access; returns `some(value)` or `none` if out of bounds |
| `[int index]` | `T` | Direct access; runtime error if out of bounds |
| `.push(T item)` | `void` | Appends item to the end of the list (alias for push_back) |
| `.push_back(T item)` | `void` | Appends item to the end of the list |
| `.pop()` | `optional<T>` | Removes and returns last item, or `none` if empty (alias for pop_back) |
| `.pop_back()` | `optional<T>` | Removes and returns last item, or `none` if empty |
| `.sort()` | `void` | Sorts the list in place (requires comparable `T`) |
| `.reverse()` | `void` | Reverses the list in place |
| `.contains(T item)` | `bool` | Returns `true` if item exists in the list |
| `.index_of(T item)` | `optional<int>` | Returns index of first match, or `none` |
| `.find(func(T) returns bool)` | `optional<T>` | Returns first element matching predicate, or `none` |
| `.filter(func(T) returns bool)` | `list<T>` | Returns elements that satisfy predicate |
| `.map(func(T) returns U)` | `list<U>` | Transforms each element into a new list |
| `.reduce(U init, func(U, T) returns U)` | `U` | Folds list into one value |
| `.to_string()` | `string` | Returns a string representation of the list |

```mux
auto nums = [1, 2, 3]

// Safe access with optional
match nums.get(0) {
    some(first) { print(first.to_string()) }  // "1"
    none { print("Index out of bounds") }
}

// Direct access (runtime error if index invalid)
auto second = nums[1]  // 2

// Mutation
nums.push_back(4)      // [1, 2, 3, 4]
match nums.pop_back() {
    some(last) { print(last.to_string()) }  // "4"
    none { }
}

// Inspection
print(nums.size().to_string())     // "3"
print(nums.is_empty().to_string())   // "false"
```

#### Map Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Returns the number of key-value pairs |
| `.is_empty()` | `bool` | Returns `true` if map has no entries |
| `.get(K key)` | `optional<V>` | Safe lookup; returns `some(value)` or `none` if key not found |
| `[K key]` | `V` | Direct access; runtime error if key not found |
| `.put(K key, V value)` | `void` | Inserts or updates a key-value pair |
| `.contains(K key)` | `bool` | Returns `true` if key exists in map |
| `.remove(K key)` | `optional<V>` | Removes key and returns value, or `none` if key not found |
| `.get_keys()` | `list<K>` | Returns all keys |
| `.get_values()` | `list<V>` | Returns all values |
| `.filter(func(K, V) returns bool)` | `map<K, V>` | Returns entries matching predicate |
| `.to_string()` | `string` | Returns a string representation of the map |

```mux
auto scores = {"Alice": 90, "Bob": 85}

// Safe access
match scores.get("Alice") {
    some(score) { print(score.to_string()) }  // "90"
    none { print("Student not found") }
}

// Direct access
auto bobScore = scores["Bob"]  // 85

// Map entries are immutable; reassign to update
scores["Alice"] = 95  // Updates existing key
```

#### Set Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Returns the number of elements |
| `.is_empty()` | `bool` | Returns `true` if set is empty |
| `.add(T item)` | `void` | Adds an item to the set |
| `.contains(T item)` | `bool` | Returns `true` if item exists in set |
| `.remove(T item)` | `bool` | Removes item and returns `true` if it existed |
| `.to_string()` | `string` | Returns a string representation of the set |

```mux
auto tags = {"urgent", "important", "review"}
print(tags.size().to_string())  // "3"

// Add and check membership
tags.add("priority")
if tags.contains("urgent") {
    print("Has urgent tag")
}

// Remove item
if tags.remove("review") {
    print("Removed review")
}

// Set operators
auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}
auto diff = set1 - set2          // {1, 2}
auto common = set1 / set2        // {3}
```

**Design Note:** Collections use consistent method naming across all types. Safe access via `.get()` returns `optional<T>`, while direct access with `[]` provides unchecked access with runtime bounds checking.

### 11.1 Technical Design: Nested Collections

Mux's collections (`list`, `map`, `set`) can contain any `Value`, enabling arbitrary nesting.

#### Collection Implementations

| Collection | Rust Type | Use Case |
|------------|-----------|----------|
| `list<T>` | `Vec<Value>` | Contiguous array, indexed access |
| `map<K,V>` | `BTreeMap<Value, Value>` | Key-value pairs, sorted keys |
| `set<T>` | `BTreeSet<Value>` | Unique elements, membership test |

#### Why BTreeMap/BTreeSet?

Unlike HashMap/HashSet, BTree variants provide:
- **Deterministic iteration order**: Always the same order
- **Ordered operations**: First/last element, range queries
- **Reproducible output**: `to_string()` produces consistent results

#### Nested Example

```mux
auto nested = [
    {"name": "Alice", "scores": [95, 87, 92]},
    {"name": "Bob", "scores": [78, 85, 90]}
]
// Structure: list<map<string, list<int> | string>>
```

The type system tracks nesting through:
1. **Parser**: Creates nested `TypeNode` structures
2. **Semantic Analyzer**: Resolves to `Type::List(Type::Map(...))`
3. **Code Generator**: Creates appropriate LLVM types

#### Reference Counting in Collections

Collections are RC-allocated and contain RC-allocated values. When freed:
1. Collection's refcount reaches zero
2. Collection's `Vec<Value>` is dropped
3. Each contained `Value` has its refcount decremented
4. Nested collections are freed recursively

---

## 12. Error Handling

### 12.1 `result<T, E>`

```
func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}

// Usage with inference
auto result = divide(10, 2)  // inferred as result<int, string>
match result {
    ok(value) {
        auto message = "result: " + value  // local inference
        print(message)
    }
    err(error) {
        print("Error: " + error)
    }
    _ {
        print("Unexpected result")  // wildcard for completeness
    }
}

// Ignoring error details when not needed
match result {
    ok(value) {
        print("Success: " + value)
    }
    err(_) {
        print("some error occurred")  // error details ignored
    }
}
```

#### result Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_ok()` | `bool` | Returns `true` if the result is an ok variant |
| `.is_err()` | `bool` | Returns `true` if the result is an err variant |
| `.to_string()` | `string` | String representation |

```mux
result<int, string> res1 = ok(42)
result<int, string> res2 = err("error")

print(res1.is_ok().to_string())   // true
print(res1.is_err().to_string())  // false
print(res2.is_ok().to_string())   // false
print(res2.is_err().to_string())  // true
```

### 12.2 `optional<T>`

```
func findEven(list<int> xs) returns optional<int> {
    for x in xs {
        if x % 2 == 0 {
            return some(x)
        }
    }
    return none
}

// Usage with inference
optional<int> maybeEven = findEven([1, 3, 4, 7])  // inferred as optional<int>

match maybeEven {
    some(value) {
        print("Found even: " + value)
    }
    none {
        print("No even number found")
    }
    _ {
        print("Unexpected optional state")
    }
}

// Ignoring the wrapped value when you just care about presence
match maybeEven {
    some(_) {
        print("Got a value")  // don't care what the value is
    }
    none {
        print("Got nothing")
    }
}
```

#### optional Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_some()` | `bool` | Returns `true` if the optional contains a value |
| `.is_none()` | `bool` | Returns `true` if the optional is empty |
| `.to_string()` | `string` | String representation |

```mux
optional<int> opt1 = some(42)
optional<int> opt2 = none

print(opt1.is_some().to_string())  // true
print(opt1.is_none().to_string())  // false
print(opt2.is_some().to_string())  // false
print(opt2.is_none().to_string())  // true
```

Use `match` to unpack results and optionals. Use `_` to ignore unused values in patterns.

### 12.3 Technical Design: result and optional

Both `result<T, E>` and `optional<T>` use a uniform runtime representation.

#### Memory Layout

```rust
pub struct result<T, E> {
    discriminant: i32,    // 0 = ok, 1 = err
    data: *mut T,        // pointer to value
}

pub struct optional<T> {
    discriminant: i32,    // 0 = none, 1 = some
    data: *mut T,        // pointer to value
}
```

Same layout enables generic code to work with either type.

#### Runtime Behavior

**Discriminant**: Determines which variant is active
**Data pointer**: Points to the contained value (boxed like all other values)

```mux
auto opt = some(42)      // discriminant=1, data=box(42)
auto res = ok("error")   // discriminant=0, data=box("error")
```

#### Why This Design?

- **Single runtime representation**: Collections can store either
- **No enum overhead**: No runtime enum tag beyond discriminant
- **Error propagation**: Easy to implement with match statements
- **Interop**: optional and result can wrap the same types

---

## 13. Memory Model

- **Reference-counted** runtime; deterministic memory management with no manual `free`
- All objects and collections live on the heap
- Primitives passed by value, objects by reference

### 13.1 Technical Design: Reference Counting

Mux uses **atomic reference counting** for deterministic memory management. Every heap-allocated value is prefixed with a reference count header.

#### Memory Layout

```
┌──────────────────┬─────────────┐
│   RefHeader      │    Value    │
│ ref_count: u64   │  (payload)  │
└──────────────────┴─────────────┘
    ^
    Allocation pointer
```

The `RefHeader` uses `AtomicUsize` for thread-safe atomic operations. The `Value` payload contains the actual data.

#### Reference Count Operations

**Increment (`mux_rc_inc`)**: Called when creating a new reference:
- Assigning to a new variable
- Passing as a function argument
- Adding to a collection

**Decrement (`mux_rc_dec`)**: Called when a reference goes out of scope:
- Variable assignment is overwritten
- Function returns (cleanup of local variables)

When `mux_rc_dec` returns `true`, the refcount reached zero and memory is freed automatically.

#### Scope-Based Tracking

The compiler generates cleanup code using a **scope stack**:

1. **Enter scope** -> `push_rc_scope()` (function entry, if-block, loop-body, match-arm)
2. **Track variable** -> `track_rc_variable(name, alloca)` for each RC-allocated variable
3. **Exit scope** -> `generate_all_scopes_cleanup()` iterates through all scopes in reverse order

This ensures proper cleanup order and handles early returns.

---

## 14. References

Mux uses references for safe memory access and manipulation:

- `&T` denotes a reference to type `T`
- `&expr` creates a reference to `expr`
- References are automatically dereferenced when used
- No pointer arithmetic is allowed
- References are non-nullable by default
- Use `Option<&T>` for nullable references

```mux
// Basic reference usage
int x = 10
auto r = &x      // r is of type &int
print("ref value: " + (*r).to_string())  // 10 - explicit dereference with *

*r = 20          // Changes x to 20 via dereference
print("val after ref update: " + (*r).to_string())  // 20
print("x is now: " + x.to_string())  // 20

// References to list elements
auto numbers = [1, 2, 3, 4, 5]
auto first = &numbers[0]  // &int
print("first element: " + (*first).to_string())  // 1

// Function taking a reference
func update(&int ref) returns void {
    *ref = *ref + 1  // Must explicitly dereference to modify
}

update(&x)
print("val after update: " + x.to_string())  // 21
```

**Reference Syntax:**
- Create reference: `&variable` or `&expression`
- Dereference: `*reference` (required for both reading and writing)
- Pass to functions: `func(&int ref)` declares parameter, `update(&x)` passes reference
- References to references: Not supported

**Design Note:** Unlike some languages with automatic dereferencing, Mux requires explicit `*` for all reference operations. This makes memory access patterns explicit and prevents accidental mutation bugs.

---

## 15. Modules & Imports

```
import math
import std.math
import shapes.circle as circle

// Usage with inference
float pi = math.PI         // type inferred from math module
float root = math.sqrt(9.0)
auto c = circle.new(5.0)  // type inferred from constructor

// Import with unused alias for completeness
import utils.logger as _  // imported but not directly used in this scope
```

- Python-style imports only
- Module paths map directly to file paths
- Imported symbols can be used with type inference
- Use `_` alias when importing for side effects only
- Standard library modules are imported as `import std.<module>` and used as `<module>.<item>`

### 15.1 Technical Design: Module System

Mux uses Python-style module imports with compile-time resolution.

#### Import Resolution

```mux
import math          // math.mux in same directory
import shapes.circle // shapes/circle.mux
import std.math      // stdlib math module
```

File paths map to module paths:
- `import foo` -> `foo.mux`
- `import shapes.circle` -> `shapes/circle.mux`
- `import std.math` -> stdlib `math` module namespace (`math.*`)

#### Name Mangling for Imported Functions

Functions from imported modules use mangled names:

```mux
// math.mux
func fibonacci(int n) returns int { ... }

// main.mux
import math
auto result = math.fibonacci(10)
```

Generates: `math!fibonacci` (not `fibonacci`)

This prevents conflicts when multiple modules define functions with the same name.

#### Top-Level Statements

Top-level statements in modules become a module initialization function:

```mux
// config.mux
const int MAX_USERS = 100
auto initialized = false

func init() returns void {
    initialized = true
}
```

The compiler generates:
```llvm
define void @config.init() { ... }
```

And calls it before `main()` executes.

#### Module Dependencies

The compiler:
1. Parses all imports
2. Builds dependency graph
3. Processes modules in topological order
4. Generates initialization functions for each module

---

## 15. Type Inference Guidelines

### 16.1 When to Use `auto`

**Recommended:**

- Local variables with obvious initialization
- Complex generic types that are clear from context
- Temporary variables in calculations
- Iterator variables in loops

### 16.2 Inference Limitations

```
// These require explicit types due to ambiguity
list<int> empty = []           // empty collection needs explicit type
auto empty = list<int>()       // or explicit constructor

result<int, string> pending    // uninitialized variables need explicit type

// Generic instantiation may need explicit types
Stack[int] stack = Stack[int]()      // explicit generic parameter
auto pairs = zip<int, string>(numbers, names)  // when inference is ambiguous
```

### 16.3 Using Underscore Effectively

```
// Good uses of underscore
func process(int data, string _) { }  // ignore second parameter
for _ in range(0, 10) { }            // ignore loop counter
match result { ok(_) { } }           // ignore success value

// Avoid overusing underscore when names would help readability
// Less clear:
func calculate(int _, int _, float _) returns float { }

// Better:
func calculate(int width, int height, float _) returns float { }
```

---

## 16. Example Program

```
import math

const float PI = 3.14159  // inferred as float

enum MaybeValue<T> { 
    some(T) 
    none 
}

interface Shape {
    func area() returns float
}

class Circle is Shape {
    float r  // explicit type required for fields
    
    func area() returns float { 
        return PI * r * r 
    }
}

// Generic utility function
func map<T, U>(list<T> items, func(T) returns U transform) returns list<U> {
    auto result = list<U>()
    for item in items {
        result.push_back(transform(item))
    }
    return result
}

func main() returns void {
    auto shapes = [Circle.new(2.0), Circle.new(3.5)]  // inferred as list<Circle>
    
    for shape in shapes {
        float area = shape.area()  // inferred as float
        string message = "Area: " + area  // inferred as str
        print(message)
    }
    
    // Working with Results and inference
    auto results = list<result<float, string>>()
    for shape in shapes {
        auto areaResult = ok(shape.area())  // inferred as result<float, string>
        results.push_back(areaResult)
    }
    
    // Using generics with inference and lambdas
    auto areas = map(shapes, func(Shape s) {
        return s.area()  // inferred as list<float>
    })
    
    auto descriptions = map(areas, func(string a) {
        return "Area: " + a  // inferred as list<string>
    })
    
    // Pattern matching with underscore
    for result in results {
        match result {
            ok(value) {
                print("Success: " + value)
            }
            err(_) {
                print("Error occurred")  // don't care about error details
            }
        }
    }
}
```

---

## 17. Standard Library

The Mux standard library includes `assert`, `math`, `io`, `random`, and `datetime`.

Import styles:

```mux
import std                    // use std.assert, std.math, std.io, std.random, std.datetime
import std.assert              // use assert.*
import std.math               // use math.*
import std.io                 // use io.*
import std.random             // use random.*
import std.datetime           // use datetime.*
import std.(math, random as r)
import std.*                  // flat import of stdlib items
```

### 17.1 assert

`assert` provides test assertions that panic immediately on failure with descriptive error messages.

- `assert.assert_true(bool condition) -> void` - Panics if false
- `assert.assert_false(bool condition) -> void` - Panics if true
- `assert.assert(bool condition, string message) -> void` - Panics with custom message if false
- `assert.assert_eq(T actual, T expected) -> void` - Panics if values differ (generic)
- `assert.assert_ne(T actual, T expected) -> void` - Panics if values equal (generic)
- `assert.assert_some(optional<T> value) -> void` - Panics if none
- `assert.assert_none(optional<T> value) -> void` - Panics if some
- `assert.assert_ok(result<T, E> value) -> void` - Panics if err
- `assert.assert_err(result<T, E> value) -> void` - Panics if ok

### 17.2 math

`math` provides floating-point constants and functions.

- Constants: `math.pi`, `math.e`
- Unary functions: `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `ln`, `log2`, `log10`, `exp`, `abs`, `floor`, `ceil`, `round`
- Binary functions: `atan2`, `log`, `min`, `max`, `hypot`, `pow`

### 17.3 io

`io` provides filesystem and path operations with explicit error handling via `result<T, string>`.

- File operations: `read_file`, `write_file`, `exists`, `remove`, `mkdir`, `listdir`
- Path operations: `is_file`, `is_dir`, `join`, `basename`, `dirname`

### 17.4 random

`random` provides pseudorandom generation:

- `random.seed(int seed) -> void`
- `random.next_int() -> int`
- `random.next_range(int min, int max) -> int`
- `random.next_float() -> float`
- `random.next_bool() -> bool`

### 17.5 datetime

`datetime` provides Unix-timestamp based date and time helpers.

- `datetime.now() -> result<int, string>` (seconds since Unix epoch, UTC)
- `datetime.now_millis() -> result<int, string>` (milliseconds since Unix epoch, UTC)
- `datetime.year(int ts) -> result<int, string>`
- `datetime.month(int ts) -> result<int, string>`
- `datetime.day(int ts) -> result<int, string>`
- `datetime.hour(int ts) -> result<int, string>`
- `datetime.minute(int ts) -> result<int, string>`
- `datetime.second(int ts) -> result<int, string>`
- `datetime.weekday(int ts) -> result<int, string>` where `0=Sun ... 6=Sat`
- `datetime.format(int ts, string pattern) -> result<string, string>` (UTC)
- `datetime.format_local(int ts, string pattern) -> result<string, string>` (local timezone)
- `datetime.sleep(int seconds) -> result<void, string>` (blocking at call site)
- `datetime.sleep_millis(int milliseconds) -> result<void, string>` (blocking at call site)

Format patterns use chrono `strftime` tokens, for example:
- `%A` full weekday name
- `%a` abbreviated weekday name
- `%B` full month name
- `%b` abbreviated month name
- `%Y-%m-%d %H:%M:%S`

---

## Project File Structure

```
mux-lang/
├── mux-compiler/
│   ├── src/
│   │   ├── ast/
│   │   │   ├── types.rs
│   │   │   ├── nodes.rs
│   │   │   ├── literals.rs
│   │   │   ├── operators.rs
│   │   │   ├── patterns.rs
│   │   │   ├── error.rs
│   │   │   └── mod.rs
│   │   ├── codegen/
│   │   │   ├── mod.rs
│   │   │   ├── expressions.rs
│   │   │   ├── statements.rs
│   │   │   ├── functions.rs
│   │   │   ├── methods.rs
│   │   │   ├── classes.rs
│   │   │   ├── constructors.rs
│   │   │   ├── operators.rs
│   │   │   ├── generics.rs
│   │   │   ├── types.rs
│   │   │   ├── memory.rs
│   │   │   └── runtime.rs
│   │   ├── semantics/
│   │   │   ├── mod.rs
│   │   │   ├── types.rs
│   │   │   ├── symbol_table.rs
│   │   │   ├── unifier.rs
│   │   │   ├── format.rs
│   │   │   └── error.rs
│   │   ├── lexer/
│   │   │   ├── mod.rs
│   │   │   ├── token.rs
│   │   │   ├── span.rs
│   │   │   └── error.rs
│   │   ├── parser/
│   │   │   ├── mod.rs
│   │   │   └── error.rs
│   │   ├── diagnostic/
│   │   │   ├── mod.rs
│   │   │   ├── emitter.rs
│   │   │   ├── files.rs
│   │   │   └── styles.rs
│   │   ├── module_resolver.rs
│   │   ├── source.rs
│   │   ├── lib.rs
│   │   └── main.rs
│   └── tests/
│       ├── lexer_integration.rs
│       ├── parser_integration.rs
│       ├── semantics_integration.rs
│       ├── executable_integration.rs
│       └── snapshots/
│
├── mux-runtime/
│   ├── src/
│   │   ├── lib.rs
│   │   ├── object.rs
│   │   ├── refcount.rs
│   │   ├── boxing.rs
│   │   ├── bool.rs
│   │   ├── int.rs
│   │   ├── float.rs
│   │   ├── string.rs
│   │   ├── list.rs
│   │   ├── map.rs
│   │   ├── set.rs
│   │   ├── optional.rs
│   │   ├── result.rs
│   │   ├── io.rs
│   │   ├── math.rs
│   │   └── std.rs
│   └── Cargo.toml
│
├── test_scripts/
│   ├── error_cases/
│   │   ├── *.mux
│   └── *.mux
│
├── Cargo.toml
├── Cargo.lock
└── AGENTS.md
```

---

## 18. License

Mux is licensed under the MIT license.

---
