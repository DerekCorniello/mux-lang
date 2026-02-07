# Type System

Mux uses a strong, static type system with explicit typing and local type inference.

## Type Safety

Mux enforces **strict static typing** with **no implicit type conversions**. All type conversions must be explicit.

```mux
// ERROR: implicit conversion not allowed
auto x = 1 + 1.0           // Cannot add int and float

// Valid: explicit conversion
auto x = 1 + (1.0).to_int()   // 2
auto y = (1).to_float() + 1.0 // 2.0
```

## Primitive Types

### Integer Type (`int`)

- 64-bit signed integer
- Range: -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807

### Float Type (`float`)

- 64-bit IEEE-754 floating point
- Range: ~±1.7E308 with ~15 decimal digits of precision

### Boolean Type (`bool`)

- Two values: `true` and `false`
- Represents logical states

### Character Type (`char`)

- Single Unicode code point
- Encoded as UTF-8 internally
- Range: U+0000 to U+10FFFF

### String Type (`string`)

- UTF-8 encoded sequence of characters
- Immutable
- Variable length

### Void Type (`void`)

- Used for functions that return no value
- Cannot be used as a variable type

## Composite Types

### Optional Type (`Optional<T>`)

Represents a value that may or may not exist:

```mux
Optional<int> maybeValue = Some(42)
Optional<string> empty = None
```

Variants: `Some(T)` and `None`

### Result Type (`Result<T, E>`)

Represents either success with a value or failure with an error:

```mux
Result<int, string> success = Ok(42)
Result<int, string> failure = Err("error message")
```

Variants: `Ok(T)` and `Err(E)`

### List Type (`list<T>`)

Homogeneous collection of elements:

```mux
list<int> numbers = [1, 2, 3, 4, 5]
list<string> names = ["Alice", "Bob"]
```

### Map Type (`map<K, V>`)

Key-value pairs with unique keys:

```mux
map<string, int> scores = {"Alice": 90, "Bob": 85}
```

### Set Type (`set<T>`)

Unique elements with membership testing:

```mux
set<int> unique_numbers = {1, 2, 3, 4, 5}
```

## Reference Types

### Reference (`&T`)

Reference to a value:

```mux
int x = 42
auto r = &x    // Type: &int
*r = 100       // x is now 100
```

## User-Defined Types

### Enums (Tagged Unions)

```mux
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}
```

### Classes

```mux
class Circle is Drawable {
    float radius

    func area() returns float {
        return 3.14159 * radius * radius
    }
}
```

### Interfaces

```mux
interface Drawable {
    func draw() returns void
}
```

## Type Conversions

All type conversions in Mux are **explicit**. No implicit conversions exist between any types.

### Numeric Conversions

| From | To | Method | Notes |
|------|-----|--------|-------|
| `int` | `float` | `.to_float()` | |
| `int` | `string` | `.to_string()` | |
| `float` | `int` | `.to_int()` | Truncates |
| `float` | `string` | `.to_string()` | |
| `int` | `int` | `.to_int()` | Identity |
| `float` | `float` | `.to_float()` | Identity |

### Boolean Conversions

| From | To | Method | Notes |
|------|-----|--------|-------|
| `bool` | `int` | `.to_int()` | true=1, false=0 |
| `bool` | `float` | `.to_float()` | true=1.0, false=0.0 |
| `bool` | `string` | `.to_string()` | "true" or "false" |

### Character Conversions

| From | To | Method | Notes |
|------|-----|--------|-------|
| `char` | `string` | `.to_string()` | |
| `char` | `int` | `.to_int()` | Only '0'-'9' digits |

### String Conversions

| From | To | Method | Returns |
|------|-----|--------|---------|
| `string` | `int` | `.to_int()` | `Result<int, string>` |
| `string` | `float` | `.to_float()` | `Result<float, string>` |
| `string` | `string` | `.to_string()` | Identity |

### Invalid Conversions

The following are compile-time errors:

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
```

## Type Inference

Mux supports local type inference with the `auto` keyword.

### Where Inference Works

```mux
// Variable initialization
auto x = 42                    // inferred as int
auto pi = 3.14159              // inferred as float
auto name = "Mux"              // inferred as string

// Return statements
func get_number() returns int {
    auto n = 42  // inferred as int
    return n
}

// Function return types (cannot infer)
func make_int(int x) returns int {
    return x  // return type is int, x is int
}

// Collection literals
auto numbers = [1, 2, 3]       // inferred as list<int>
auto names = ["a", "b"]        // inferred as list<string>

// Pattern matching
match result {
    Ok(value) { auto v = value }  // inferred from Ok(T)
    Err(_) { }
}
```

### Where Inference Does NOT Work

```mux
// Empty collections need explicit type
list<int> empty = []           // ERROR with auto
auto empty = list<int>()       // Valid with explicit constructor

// Uninitialized variables need explicit type
auto uninit        // ERROR: cannot infer type
int uninitialized  // Valid

// Function parameters must be explicit
func process(auto item) returns void { }  // ERROR
func process(int item) returns void { }    // Valid

// Generic instantiation may need explicit type
Stack<int> stack = Stack<int>.new()  // Explicit for clarity
```

## Generics

Mux supports generic types and functions with interface bounds.

### Generic Functions

```mux
func identity<T>(T value) returns T {
    return value
}

auto a = identity(42)           // identity$$int
auto b = identity("hello")      // identity$$string
```

### Generic Classes

```mux
class Stack<T> {
    list<T> items

    func push(T item) returns void {
        items.push_back(item)
    }

    func pop() returns Optional<T> {
        if items.is_empty() { return None }
        return items.pop_back()
    }
}

auto int_stack = Stack<int>.new()
auto string_stack = Stack<string>.new()
```

### Type Constraints

```mux
// Single bound
func max<T is Comparable>(T a, T b) returns T {
    if a > b { return a }
    return b
}

// Multiple bounds (AND semantics)
func combine<T is Add & Stringable>(T a, T b) returns string {
    return (a.add(b)).to_string()
}

// Explicit type instantiation
auto result = max<int>(3, 7)
```

## Built-in Interfaces

Mux provides built-in interfaces for common operations:

| Interface | Methods | Types Supporting |
|-----------|---------|-----------------|
| `Stringable` | `to_string() -> string` | int, float, bool, char, string, collections, enums, classes |
| `Add` | `add(Self) -> Self` | int, float, string |
| `Sub` | `sub(Self) -> Self` | int, float |
| `Mul` | `mul(Self) -> Self` | int, float |
| `Div` | `div(Self) -> Self` | int, float |
| `Arithmetic` | `add`, `sub`, `mul`, `div` | int, float |
| `Equatable` | `eq(Self) -> bool` | All types except functions |
| `Comparable` | `cmp(Self) -> int` | int, float, string |

### Operator Interface Mapping

| Operator | Interface Method | Primitive Behavior |
|----------|-----------------|-------------------|
| `a + b` | `Add.add()` | Direct LLVM addition |
| `a - b` | `Sub.sub()` | Direct LLVM subtraction |
| `a * b` | `Mul.mul()` | Direct LLVM multiplication |
| `a / b` | `Div.div()` | Direct LLVM division |
| `a == b` | `Equatable.eq()` | Direct LLVM comparison |
| `a > b` | `Comparable.cmp()` | Comparison via cmp |

## Type Equality and Compatibility

### Nominal Typing

Mux uses nominal typing for user-defined types:

```mux
enum A { First(int) }
enum B { First(int) }

auto a = A.First(1)
auto b = B.First(1)
// a == b  // ERROR: different types
```

### Structural Compatibility for Generics

Generic types are compatible when type parameters are compatible:

```mux
func process<T is Equatable>(list<T> items) returns void { }

func main() returns void {
    auto ints = [1, 2, 3]
    auto strs = ["a", "b", "c"]
    process(ints)   // OK: int is Equatable
    process(strs)    // OK: string is Equatable
}
```

## Type Representations

The compiler maintains three distinct type representations:

```
TypeNode (AST) ──semantic analysis──► Type ──code generation──► LLVM Type
     │                                      │
  Source location                      Semantic analysis
  Pretty printing                      Type resolution
```

### TypeNode

Used in the AST for parsing and error reporting:

- Source file location
- Pretty-printed representation
- Generic type parameters

### Type

Used for semantic analysis:

- Resolved type information
- Interface implementations
- Generic instantiation

### LLVM Type

Used for code generation:

- `BasicTypeEnum` for value types
- `PointerType` for references
- `StructType` for enums and classes

## Monomorphization

Mux uses compile-time monomorphization for generics:

```mux
func identity<T>(T value) returns T {
    return value
}

auto a = identity(42)        // Generates: identity$$int
auto b = identity("hello")    // Generates: identity$$string
```

The compiler:
1. Infers concrete types from function arguments
2. Generates unique names for specialized functions
3. Substitutes type parameters with concrete types
4. Caches generated code to avoid regeneration

## See Also

- [Lexical Structure](./lexical-structure.md) - Type literals in source
- [Grammar](./grammar.md) - Type syntax
- [Operators](./operators.md) - Type operations
- [Generics](../language-guide/generics.md) - Generic programming guide
