# Functions

Mux functions use the `func` keyword with explicit parameter types and return type declarations in the form of:
<br/>`func <function_name>(<parameter_types> <parameters>) returns <return_type> { ... }`

## Basic Function Definition

```mux title="functions.mux"
func add(int a, int b) returns int {
    return a + b
}

func greet(string name) returns void {
    print("Hello, " + name)
}

// Function with no parameters
func getCurrentTime() returns int {
    return 12345  // placeholder implementation
}
```

- Keyword `func` declares a function
- Parameter list with explicit types (no type inference with `auto` allowed)
- `returns` clause specifies return type (explicit, no inference)
- Body enclosed in `{...}` with no semicolons
- Local variables within functions can use `auto` inference

## Default Parameters

```mux title="default_params.mux"
func greet(string name, int times = 1) returns void {
    for int i in range(0, times) {
        print("Hello, " + name)
    }
}

// Call with default
greet("Alice")          // Uses times = 1

// Call with explicit value
greet("Bob", 3)         // Prints greeting 3 times
```

## Return Types

All functions must explicitly declare their return type:

```mux title="return_types.mux"
// Returns a value
func square(int n) returns int {
    return n * n
}

// Returns nothing
func printSquare(int n) returns void {
    auto result = n * n
    print(result.to_string())
}

// Returns complex types
func processData() returns map<string, int> {
    map<string, int> results = {"processed": 100, "skipped": 5}
    auto total = results["processed"] + results["skipped"]
    results["total"] = total
    return results
}

// Returns result for fallible operations
func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}

// Returns optional for nullable values
func findFirst(list<int> items) returns optional<int> {
    if items.is_empty() {
        return none
    }
    return some(items[0])
}
```

## Unused Parameters

Use `_` for parameters you don't need:

```mux title="unused_params.mux"
// Second parameter ignored
func processFirst(int first, int _) returns int {
    return first * 2
}

// Callback with unused parameters
func callback(string event, int timestamp, string _) returns void {
    print("Event: " + event + " at " + timestamp.to_string())
}
```

This is useful when implementing interfaces or callbacks that require specific signatures.

## Lambdas and Closures

Mux supports anonymous functions (lambdas) using block syntax:

```mux title="lambdas.mux"
// Block-form lambda with explicit types
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
    return first * 2
}
```

### Using Lambdas with Collections

```mux title="lambda_collections.mux"
func filter(list<int> nums, func(int) returns bool cond) returns list<int> {
    list<int> out = []
    for int n in nums {
        if cond(n) {
            out.push_back(n)
        }
    }
    return out
}

// Usage
auto numbers = [1, 2, 3, 4, 5, 6]
auto evens = filter(numbers, func(int n) returns bool {
    return n % 2 == 0
})
```

## Generic Functions

Functions can be generic (see [generics and interfaces](./generics.md)) over type parameters:

```mux title="generic_functions.mux"
// Generic function with type constraint
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
auto max_int = max<int>(3, 7)
auto max_float = max<float>(3.14, 2.71)
```

## Built-in Functions

Mux provides essential built-in functions available without imports:

### Output

**`print(string message) -> void`** - Outputs to stdout with newline:

```mux title="print_example.mux"
print("Hello, World!")
auto x = 42
print("Value: " + x.to_string())
```

### Input

**`read_line() -> string`** - Reads a line from stdin:

```mux title="read_line_example.mux"
print("Enter your name: ")
auto name = read_line()
print("Hello, " + name)
```

### Utility

**`range(int start, int end) -> list<int>`** - Creates a list of integers, inclusive of start and exclusive of end:

```mux title="range_example.mux"
// Generate indices for iteration
for int i in range(0, 5) {
    print(i.to_string())  // Prints 0, 1, 2, 3, 4
}

// Create a list of numbers
auto numbers = range(10, 15)  // [10, 11, 12, 13, 14]
```

**Design Note:** `range()` is the primary way to create numeric sequences, as Mux does not support C-style `for (int i = 0; i < n; i++)` loops.

## Function Calling

```mux title="function_calls.mux"
// Basic call
auto sum = add(5, 3)

// Chaining method calls
auto result = myString.to_int().to_string()

// Nested calls
auto doubled = square(add(2, 3))
```

## Best Practices

1. **Explicit types for parameters and return values** - Makes function signatures clear
2. **Use `auto` for local variables** - Reduces verbosity when types are obvious
3. **Return `result<T, E>` for fallible operations** - Better than panicking
4. **Return `optional<T>` for nullable values** - Explicit absence handling
5. **Use `_` for truly unused parameters** - But prefer descriptive names when helpful
6. **Keep functions small and focused** - Single responsibility

## See Also

- [Generics](./generics.md) - Generic functions and type constraints
- [Error Handling](./error-handling.md) - Using result and optional
- [Variables](./variables.md) - Type inference with `auto`
- [Control Flow](./control-flow.md) - If/else, loops, and match
