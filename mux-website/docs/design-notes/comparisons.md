# Language Comparisons

This page shows how Mux approaches common programming tasks compared to Rust, Go, Python, and TypeScript. The goal is clarity, not criticism, each language makes different tradeoffs.
<br/>
**Why TypeScript?** TypeScript is included to show the difference between "static types that exist only at compile time" versus "static types that exist at runtime." Many developers know TypeScript as their "typed language," so seeing what you lose without runtime type information helps explain Mux's design decisions.

## Type Conversions

### Mux: Explicit Only

```mux title="mux_explicit.mux"
auto x = 42
auto y = 3.14

// ERROR: No implicit conversion
// auto sum = x + y

// Must be explicit
auto sum = x.to_float() + y  // 45.14
```

**Philosophy:** No surprises. You always know what type you're working with.

### Python: Implicit

```python title="python_implicit.py"
x = 42
y = 3.14
sum = x + y  # 45.14 (int becomes float automatically)
```

**Tradeoff:** Convenient but can lead to unexpected behavior with complex types.

### Rust: Explicit

```rust title="rust_explicit.rs"
let x: i32 = 42;
let y: f64 = 3.14;
// let sum = x + y;  // ERROR

let sum = (x as f64) + y;  // 45.14
```

**Similarity:** Like Mux, Rust requires explicit conversions for safety.

### Go: Explicit

```go title="go_explicit.go"
x := 42
y := 3.14
// sum := x + y  // ERROR

sum := float64(x) + y  // 45.14
```

**Similarity:** Go also requires explicit conversions, using C-style casts.

### TypeScript: Compile-Time Types Only

```typescript title="typescript_types.ts"
let x: number = 42;
let y: number = 3.14;
let sum = x + y;  // 45.14 (both are 'number' type)
```

**Tradeoff:** TypeScript's unified `number` type is convenient but can hide precision issues. Type information is erased at runtime, no generics, pattern matching, or type-safe enums exist at runtime.

---

## Error Handling

### Mux: Result Type

```mux title="mux_result.mux"
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

match divide(10, 2) {
    Ok(result) { print(result.to_string()) }
    Err(error) { print("Error: " + error) }
}
```

**Philosophy:** Errors are values. Compiler enforces handling.

### Rust: Result Type

```rust title="rust_result.rs"
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        return Err("division by zero".to_string());
    }
    Ok(a / b)
}

match divide(10, 2) {
    Ok(result) => println!("{}", result),
    Err(error) => println!("Error: {}", error),
}
```

**Similarity:** Nearly identical approach. Mux was directly inspired by Rust here.

### Go: Explicit Error Returns

```go title="go_errors.go"
func divide(a, b int) (int, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

result, err := divide(10, 2)
if err != nil {
    fmt.Println("Error:", err)
} else {
    fmt.Println(result)
}
```

**Tradeoff:** More verbose, easier to accidentally ignore errors.

### Python: Exceptions

```python title="python_exceptions.py"
def divide(a, b):
    if b == 0:
        raise ValueError("division by zero")
    return a / b

try:
    result = divide(10, 2)
    print(result)
except ValueError as e:
    print(f"Error: {e}")
```

**Tradeoff:** Can forget to handle exceptions. No compile-time checking.

### TypeScript: Exceptions (Runtime Types Only)

```typescript title="typescript_exceptions.ts"
function divide(a: number, b: number): number {
    if (b === 0) {
        throw new Error("division by zero");
    }
    return a / b;
}

try {
    const result = divide(10, 2);
    console.log(result);
} catch (e) {
    console.log("Error:", e);
}
```

**Tradeoff:** Similar to Python. No compile-time enforcement. TypeScript types are erased at runtime, the compiler cannot help you catch missing error handling.

---

## Memory Management

### Mux: Reference Counting

```mux title="mux_memory.mux"
auto data = [1, 2, 3, 4, 5]
// Reference count = 1

auto data2 = data
// Reference count = 2

// When data and data2 go out of scope,
// memory is automatically freed
```

**Philosophy:** Simple and automatic. Reference counting provides memory safety without a borrow checker.

**Tradeoff:** Reference counting has some runtime overhead. Mux avoids creating reference cycles through careful API design.

### Rust: Ownership + Borrow Checker

```rust title="rust_memory.rs"
let data = vec![1, 2, 3, 4, 5];
// Owned by 'data'

let data2 = &data;  // Borrowed
// Both can exist, but data2 is read-only

// When data goes out of scope, memory is freed
// No runtime overhead
```

**Tradeoff:** Zero runtime cost comes with a steeper learning curve. Rust's borrow checker is a powerful tool but requires significant investment to master.

### Go: Garbage Collection

```go title="go_memory.go"
data := []int{1, 2, 3, 4, 5}
// Managed by GC

data2 := data
// Both reference same memory
```

**Tradeoff:** Easy to use, but GC pauses can be unpredictable.

### Python: Reference Counting + GC

```python title="python_memory.py"
data = [1, 2, 3, 4, 5]
# Reference counted

data2 = data
# Reference count increases
```

**Similarity:** Like Mux, but Python also has a cycle-detecting GC.

### TypeScript: Garbage Collection (V8/Node.js)

```typescript title="typescript_memory.ts"
let data = [1, 2, 3, 4, 5];
// Managed by V8's GC, no compile-time guarantees

let data2 = data;
```

**Tradeoff:** Very easy to use, but GC behavior varies by runtime and there's no compile-time memory safety.

---

## Generics

### Mux: Monomorphization

```mux title="mux_generics.mux"
func identity<T>(T value) returns T {
    return value
}

auto x = identity(42)        // Generates identity$$int
auto y = identity("hello")   // Generates identity$$string
```

**Philosophy:** Zero runtime cost. Specialized code for each type.

**Tradeoff:** Larger binary size, slower compilation.

### Rust: Monomorphization

```rust title="rust_generics.rs"
fn identity<T>(value: T) -> T {
    value
}

let x = identity(42);        // Monomorphized to i32
let y = identity("hello");   // Monomorphized to &str
```

**Similarity:** Exact same approach as Mux.

### Go: Generics (Recent)

```go title="go_generics.go"
func identity[T any](value T) T {
    return value
}

x := identity(42)
y := identity("hello")
```

**Note:** Go added generics in 1.18. Implementation may use monomorphization or runtime dispatch depending on the case.

### Python: Duck Typing

```python title="python_duck_typing.py"
def identity(value):
    return value

x = identity(42)
y = identity("hello")
```

**Tradeoff:** No compile-time checking. Errors happen at runtime.

### TypeScript: Type Erasure (No Runtime Types)

```typescript title="typescript_generics.ts"
function identity<T>(value: T): T {
    return value;
}

let x = identity(42);
let y = identity("hello");
```

**Tradeoff:** Generics exist only at compile time. At runtime, both are just `any` type, there is no type information to enable pattern matching or type-safe operations.

---

## Pattern Matching

### Mux: Match with Guards

```mux title="mux_pattern_matching.mux"
match value {
    Some(x) if x > 10 { print("Large: " + x.to_string()) }
    Some(x) { print("Small: " + x.to_string()) }
    None { print("No value") }
}
```

**Philosophy:** Exhaustive, compiler-checked, guards for complex conditions.

### Rust: Match

```rust title="rust_pattern_matching.rs"
match value {
    Some(x) if x > 10 => println!("Large: {}", x),
    Some(x) => println!("Small: {}", x),
    None => println!("No value"),
}
```

**Similarity:** Nearly identical to Mux.

### Go: Switch

```go title="go_switch.go"
switch {
case value != nil && *value > 10:
    fmt.Println("Large:", *value)
case value != nil:
    fmt.Println("Small:", *value)
default:
    fmt.Println("No value")
}
```

**Tradeoff:** More verbose, no exhaustiveness checking.

### Python: Match (3.10+)

```python title="python_pattern_matching.py"
match value:
    case x if x > 10:
        print(f"Large: {x}")
    case x:
        print(f"Small: {x}")
    case None:
        print("No value")
```

**Similarity:** Similar structure, but Python's match is runtime-only.

### TypeScript: No Runtime Pattern Matching

TypeScript doesn't have pattern matching. You'd use if/else or switch:

```typescript title="typescript_conditionals.ts"
if (value !== null && value > 10) {
    console.log(`Large: ${value}`);
} else if (value !== null) {
    console.log(`Small: ${value}`);
} else {
    console.log("No value");
}
```

**Tradeoff:** Without runtime type information, exhaustiveness checking is impossible. You can't know at runtime if you've covered all cases.

---

## Summary Table

| Feature | Mux | Rust | Go | Python | TypeScript |
|---------|-----|------|----|----|-----------|
| **Type Safety** | Strong, static | Strong, static | Strong, static | Dynamic | Static (compile-time only) |
| **Type Conversions** | Explicit | Explicit | Explicit | Implicit | Explicit (erased at runtime) |
| **Error Handling** | Result type | Result type | Error values | Exceptions | Exceptions (no compile-time checks) |
| **Memory Management** | Reference counting | Ownership | GC | Ref count + GC | GC (no compile-time safety) |
| **Generics** | Monomorphization | Monomorphization | Monomorphization/dispatch | Duck typing | Type erasure (no runtime types) |
| **Pattern Matching** | Yes, with guards | Yes, with guards | Switch only | Yes (3.10+) | No (no runtime types) |
| **Learning Curve** | Low-Medium | High | Low | Low | Low-Medium |
| **Performance** | Medium-High | Highest | High | Low | Medium (runtime dependent) |

**Next:** Learn about [Mux's design philosophy](./philosophy.md)
