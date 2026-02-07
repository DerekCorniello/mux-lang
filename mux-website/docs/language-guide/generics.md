# Generics

Mux supports generics with type parameters and interface bounds, similar to Go and Rust.

## Generic Functions

Functions can be generic over type parameters:

```mux title="generic_identity.mux"
// Simple generic function
func identity<T>(T value) returns T {
    return value
}

// Usage
auto a = identity<int>(42)
auto b = identity<string>("hello")
```

### Type Constraints

Use the `is` keyword to constrain type parameters to specific interfaces:

```mux title="generic_constraints.mux"
// Generic function with Comparable bound
func max<T is Comparable>(T a, T b) returns T {
    if a > b {
        return a
    }
    return b
}

// Generic function with Stringable bound
func greet<T is Stringable>(T value) returns string {
    return "Hello, " + value.to_string()
}

// Generic function with Add bound
func add<T is Add>(T a, T b) returns T {
    return a.add(b)
}

// Usage
auto max_int = max<int>(3, 7)           // T = int
auto max_float = max<float>(3.14, 2.71) // T = float
auto greeting = greet<string>("World")  // T = string
```

### Multiple Type Parameters

```mux title="multiple_type_params.mux"
func pair<T, U>(T first, U second) returns Pair<T, U> {
    auto p = Pair<T, U>.new()
    p.first = first
    p.second = second
    return p
}

auto result = pair<int, string>(42, "answer")
```

### Multiple Bounds (AND Semantics)

Type parameters can have multiple constraints:

```mux title="multiple_bounds.mux"
// Type must implement BOTH Add AND Stringable
func combine<T is Add & Stringable>(T a, T b) returns string {
    return (a.add(b)).to_string()
}

// Only types implementing both interfaces can be used
auto result = combine<int>(5, 3)  // "8"
```

## Generic Classes

Classes can be generic over type parameters:

```mux title="generic_classes.mux"
class Stack<T> {
    list<T> items
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns Optional<T> {
        if self.items.is_empty() {
            return None.new()
        }
        return self.items.pop_back()
    }
    
    func size() returns int {
        return self.items.size()
    }
}

// Usage
auto int_stack = Stack<int>.new()
int_stack.push(1)
int_stack.push(2)
int_stack.push(3)

auto string_stack = Stack<string>.new()
string_stack.push("hello")
string_stack.push("world")
```

### Generic Classes with Multiple Type Parameters

```mux title="generic_pair.mux"
class Pair<T, U> {
    T first
    U second
    
    func swap() returns Pair<U, T> {
        auto swapped = Pair<U, T>.new()
        swapped.first = self.second
        swapped.second = self.first
        return swapped
    }
    
    common func from(T a, U b) returns Pair<T, U> {
        auto pair = Pair<T, U>.new()
        pair.first = a
        pair.second = b
        return pair
    }
}

// Usage
auto pair = Pair<int, string>.from(42, "answer")
auto reversed = pair.swap()  // Pair<string, int>
```

## Built-in Interfaces

Mux provides built-in interfaces for common operations:

| Interface | Methods | Description |
|-----------|---------|-------------|
| `Stringable` | `to_string() -> string` | Types that can be converted to string |
| `Arithmetic` | `add`, `sub`, `mul`, `div` | Types that support all arithmetic operators |
| `Equatable` | `eq(Self) -> bool` | Types that support `==` and `!=` operators |
| `Comparable` | `cmp(Self) -> int` | Types that support `<`, `<=`, `>`, `>=` operators |

### Operator Mapping

- `a + b` uses `Add.add()` when type doesn't natively support `+`
- `a > b` uses `Comparable.cmp()` returning -1, 0, or 1
- `a == b` uses `Equatable.eq()`

### Primitives and Interfaces

| Type | Implements |
|------|-----------|
| `int` | `Stringable`, `Add`, `Sub`, `Mul`, `Div`, `Arithmetic`, `Equatable`, `Comparable` |
| `float` | `Stringable`, `Add`, `Sub`, `Mul`, `Div`, `Arithmetic`, `Equatable`, `Comparable` |
| `string` | `Stringable`, `Add`, `Equatable`, `Comparable` |
| `bool` | `Stringable`, `Equatable` |

## Implementing Interfaces for Custom Types

```mux title="custom_interfaces.mux"
interface Add {
    func add(Self) returns Self
}

class Point {
    int x
    int y
    
    func add(Point other) returns Point {
        auto result = Point.new()
        result.x = self.x + other.x
        result.y = self.y + other.y
        return result
    }
}

// Now Point can be used with generic functions requiring Add
func sum_points<T is Add>(list<T> points) returns T {
    auto result = points[0]
    for i in range(1, points.size()) {
        result = result.add(points[i])
    }
    return result
}

auto points = [Point.new(), Point.new(), Point.new()]
auto total = sum_points<Point>(points)
```

## Generic Functions with Collections

```mux title="generic_collections_funcs.mux"
// Generic map function
func map<T, U>(list<T> items, func(T) returns U transform) returns list<U> {
    auto result = list<U>()
    for item in items {
        result.push_back(transform(item))
    }
    return result
}

// Generic filter function
func filter<T>(list<T> items, func(T) returns bool predicate) returns list<T> {
    auto result = list<T>()
    for item in items {
        if predicate(item) {
            result.push_back(item)
        }
    }
    return result
}

// Usage
auto numbers = [1, 2, 3, 4, 5]
auto doubled = map<int, int>(numbers, func(int n) returns int {
    return n * 2
})

auto evens = filter<int>(numbers, func(int n) returns bool {
    return n % 2 == 0
})
```

## Monomorphization

Mux uses **compile-time monomorphization** for generics - specialized code is generated for each type instantiation:

```mux title="monomorphization.mux"
func identity<T>(T value) returns T {
    return value
}

auto a = identity<int>(42)        // Generates: identity$$int
auto b = identity<string>("hello") // Generates: identity$$string
```

### How Monomorphization Works

1. **Type inference**: Determine concrete types from function arguments
2. **Name generation**: Create unique identifier: `FunctionName$$Type1$$Type2$$`
3. **Type substitution**: Replace type parameters with concrete types
4. **Code generation**: Emit specialized function body
5. **Caching**: Store generated methods to avoid regeneration

### Benefits

- **Zero runtime cost**: No boxing, no vtables, no type checks
- **Static dispatch**: Methods resolved at compile time
- **LLVM optimization**: Each specialization can be fully optimized

### Tradeoff

- **Increased intermediate code size and compilation time**: One copy per type combination

## Type Inference in Generics

Type parameters can often be inferred from context:

```mux title="type_inference_generics.mux"
func identity<T is Stringable>(T value) returns T {
    return value.to_string()
}

// Explicit type parameter
auto a = identity<int>(42)

// Type inferred from argument
auto b = identity(42)  // T is a int
auto c = identity("hello")  // T is a string
```

However, when ambiguous, explicit types are required:

```mux title="explicit_generic_types.mux"
// Ambiguous - need explicit types
list<int> stack = []

// Clear from context - can use auto
auto numbers = [1, 2, 3]  // list<int> inferred
```

## Generic Enums

Enums can be generic:

```mux title="generic_enums.mux"
enum Optional<T> {
    Some(T)
    None
}

enum Result<T, E> {
    Ok(T)
    Err(E)
}

// Usage
auto maybeInt = Some.new(42)              // Optional<int>
auto success = Ok.new(100)                 // Result<int, E>
auto failure = Err.new("error message")    // Result<T, string>
```

See [Enums](./enums.md) and [Error Handling](./error-handling.md) for more details.

## Constraints and Bounds

### Single Constraint

```mux title="single_constraint.mux"
func process<T is Stringable>(list<T> items) returns void {
    for item in items {
        print(item.to_string())
    }
}
```

### Multiple Constraints (AND)

```mux title="multiple_constraints.mux"
func max<T is Comparable & Stringable>(T a, T b) returns string {
    if a > b {
        return a.to_string()
    }
    return b.to_string()
}
```

Type `T` must implement **all** specified interfaces.

## Best Practices

1. **Use type constraints** - Specify what operations are needed
2. **Prefer interface bounds over concrete types** - More flexible
3. **Leverage monomorphization** - No runtime overhead for generics
4. **Use descriptive type parameter names** - `T`, `U`, `K`, `V` for simple cases
5. **Explicit types when ambiguous** - Helps readability
6. **Keep generic functions simple** - Complex logic harder to debug
7. **Document constraints** - Make requirements clear

## See Also

- [Functions](./functions.md) - Generic functions
- [Classes](./classes.md) - Generic classes
- [Enums](./enums.md) - Generic enums
- [Error Handling](./error-handling.md) - Optional&lt;T&gt; and Result&lt;T, E&gt;
