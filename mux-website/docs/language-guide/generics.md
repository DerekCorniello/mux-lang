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
// Type must implement BOTH Stringable AND Hashable
func print_if_hashable<T is Stringable & Hashable>(T value) returns string {
    return value.to_string()
}

// Only types implementing both interfaces can be used
auto result = print_if_hashable<int>(42)  // "42"
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

| Interface | Description |
|-----------|-------------|
| `Stringable` | Types that can be converted to string (via `.to_string()` method) |
| `Equatable` | Types that support `==` and `!=` operators |
| `Comparable` | Types that support `<`, `<=`, `>`, `>=` operators |
| `Hashable` | Types that can be used as keys in sets and maps |

### Operator Mapping

- `a + b` - Works for types with native support (int, float, string, list, map, set)
- `a > b` - Works for types with native support (int, float, string, char)
- `a == b` - Works for all types

### Primitives and Interfaces

| Type | Implements |
|------|-----------|
| `int` | `Stringable`, `Equatable`, `Comparable`, `Hashable` |
| `float` | `Stringable`, `Equatable`, `Comparable`, `Hashable` |
| `string` | `Stringable`, `Equatable`, `Comparable`, `Hashable` |
| `bool` | `Stringable`, `Equatable`, `Hashable` |
| `char` | `Stringable`, `Equatable`, `Comparable`, `Hashable` |

## Implementing Interfaces for Custom Types

Custom types can implement interfaces to work with generic functions:

```mux title="custom_equatable.mux"
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

func compare<T is Equatable>(T a, T b) returns bool {
    return a.eq(b)
}

auto p1 = Point.new()
auto p2 = Point.new()
auto result = compare(p1, p2)
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
