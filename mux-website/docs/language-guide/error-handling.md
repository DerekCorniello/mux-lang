# Error Handling

Mux uses explicit error handling through the `result&lt;T, E&gt;` and `optional&lt;T&gt;` types, avoiding exceptions entirely.

## result&lt;T, E&gt;

The `result` type represents operations that can succeed with a value of type `T` or fail with an error of type `E`.

`E` must implement the `Error` interface:

```mux title="error_interface.mux"
interface Error {
    func message() returns string
}
```

`string` implements `Error`, so existing `result<T, string>` code continues to work.

### Basic Usage

```mux title="result_basic.mux"
func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}

auto result = divide(10, 2)

match result {
    ok(value) {
        print("result: " + value.to_string())  // "result: 5"
    }
    err(error) {
        print("Error: " + error)
    }
}
```

### result Variants

```mux title="result_variants.mux"
enum result<T, E> {
    ok(T)      // Success case with value
    err(E)     // Error case with error value
}
```

### Creating result Values

```mux title="creating_results.mux"
// Success
auto success = ok(42)                    // result<int, E>
auto success2 = ok("completed")          // result<string, E>

// Failure
auto failure = err("something went wrong")  // result<T, string>

// Explicit typing when needed
result<int, string> result = ok(100)
```

### result Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_ok()` | `bool` | Returns `true` if the result is an ok variant |
| `.is_err()` | `bool` | Returns `true` if the result is an err variant |
| `.to_string()` | `string` | String representation |

```mux title="result_methods.mux"
result<int, string> res1 = ok(42)
result<int, string> res2 = err("error")

print(res1.is_ok().to_string())   // true
print(res1.is_err().to_string())  // false
print(res2.is_ok().to_string())   // false
print(res2.is_err().to_string())  // true
```

### Pattern Matching Results

```mux title="pattern_matching_results.mux"
func parse_int(string s) returns result<int, string> {
    auto result = s.to_int()  // returns result<int, string>
    return result
}

auto parsed = parse_int("42")

match parsed {
    ok(value) {
        auto message = "Parsed: " + value.to_string()
        print(message)
    }
    err(error) {
        print("Parse error: " + error)
    }
}
```

### Ignoring Error Details

Use `_` when you don't need the error value:

```mux title="ignoring_error_details.mux"
match result {
    ok(value) {
        print("Success: " + value.to_string())
    }
    err(_) {
        print("some error occurred")  // don't care about details
    }
}
```

## optional&lt;T&gt;

The `optional` type represents values that may or may not exist.

### Basic Usage

```mux title="optional_basic.mux"
func findEven(list<int> xs) returns optional<int> {
    for x in xs {
        if x % 2 == 0 {
            return some(x)
        }
    }
    return none
}

auto maybeEven = findEven([1, 3, 4, 7])

match maybeEven {
    some(value) {
        print("Found even: " + value.to_string())  // "Found even: 4"
    }
    none {
        print("No even number found")
    }
}
```

### optional Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_some()` | `bool` | Returns `true` if the optional contains a value |
| `.is_none()` | `bool` | Returns `true` if the optional is empty |
| `.to_string()` | `string` | String representation |

```mux title="optional_methods.mux"
optional<int> opt1 = some(42)
optional<int> opt2 = none

print(opt1.is_some().to_string())  // true
print(opt1.is_none().to_string())  // false
print(opt2.is_some().to_string())  // false
print(opt2.is_none().to_string())  // true
```

### result Methods

```mux title="optional_variants.mux"
enum optional<T> {
    some(T)    // Value present
    none       // Value absent
}
```

### Creating optional Values

```mux title="creating_optionals.mux"
// With value
auto present = some(42)           // optional<int>
auto present2 = some("hello")     // optional<string>

// Without value
auto absent = none                // optional<T> (generic)

// Explicit typing
optional<int> maybeNumber = some(100)
optional<string> maybeText = none
```

### Safe Collection Access

Collections return `optional<T>` for safe access:

```mux title="safe_collection_access.mux"
auto nums = [10, 20, 30]

// Safe access with .get()
match nums.get(0) {
    some(first) {
        print("First: " + first.to_string())  // "First: 10"
    }
    none {
        print("Index out of bounds")
    }
}

// Out of bounds
match nums.get(100) {
    some(value) {
        print("Found: " + value.to_string())
    }
    none {
        print("Index out of bounds")  // This prints
    }
}
```

### Map Lookups

```mux title="map_lookups.mux"
auto scores = {"Alice": 90, "Bob": 85}

match scores.get("Alice") {
    some(score) {
        print("Alice's score: " + score.to_string())
    }
    none {
        print("Student not found")
    }
}

match scores.get("Charlie") {
    some(score) {
        print("Score: " + score.to_string())
    }
    none {
        print("Charlie not found")  // This prints
    }
}
```

### Ignoring the Value

Use `_` when you only care about presence/absence:

```mux title="ignoring_values.mux"
match maybeValue {
    some(_) {
        print("Got a value")  // don't care what it is
    }
    none {
        print("Got nothing")
    }
}
```

## Combining result and optional

### optional of result

```mux title="optional_of_result.mux"
func tryParse(optional<string> maybeStr) returns optional<result<int, string>> {
    match maybeStr {
        some(s) {
            return some(s.to_int())  // result<int, string>
        }
        none {
            return none
        }
    }
}
```

### result of optional

```mux title="result_of_optional.mux"
func getRequired(map<string, int> data, string key) returns result<int, string> {
    match data.get(key) {
        some(value) {
            return ok(value)
        }
        none {
            return err("Key '" + key + "' not found")
        }
    }
}
```

## Error Propagation Patterns

### Early Returns

```mux title="early_returns.mux"
func processData(string input) returns result<int, string> {
    // Validate input
    if input == "" {
        return err("empty input")
    }
    
    // Parse input
    auto parsed = input.to_int()
    match parsed {
        ok(value) {
            // Continue processing
            if value < 0 {
                return err("negative values not allowed")
            }
            return ok(value * 2)
        }
        err(msg) {
            return err("parse error: " + msg)
        }
    }
}
```

### Nested Matching

```mux title="nested_matching.mux"
func complexOperation() returns result<string, string> {
    auto step1 = firstOperation()
    
    match step1 {
        ok(value1) {
            auto step2 = secondOperation(value1)
            
            match step2 {
                ok(value2) {
                    return ok(value2)
                }
                err(err2) {
                    return err("step2 failed: " + err2)
                }
            }
        }
        err(err1) {
            return err("step1 failed: " + err1)
        }
    }
}
```

## Fallible Type Conversions

String and char parsing return `result` because they can fail:

```mux title="fallible_conversions.mux"
// String to int
auto num_str = "42"
auto result = num_str.to_int()  // result<int, string>

match result {
    ok(value) {
        print("Parsed: " + value.to_string())
    }
    err(error) {
        print("Parse error: " + error)
    }
}

// String to float
auto float_str = "3.14159"
auto float_result = float_str.to_float()  // result<float, string>

// Char to digit (only works for '0'-'9')
auto digit_char = '5'
auto digit_result = digit_char.to_int()  // result<int, string>

match digit_result {
    ok(digit) { print(digit.to_string()) }  // "5"
    err(msg) { print(msg) }
}

// Non-digit character
auto letter = 'A'
auto letter_result = letter.to_int()

match letter_result {
    ok(_) { print("Unexpected success") }
    err(msg) { print(msg) }  // "Character is not a digit (0-9)"
}
```

## Technical Implementation

### Memory Layout

Both types use a uniform runtime representation:

```rust title="memory_layout.rs"
pub struct result<T, E> {
    discriminant: i32,    // 0 = ok, 1 = err
    data: *mut T,        // pointer to value
}

pub struct optional<T> {
    discriminant: i32,    // 0 = none, 1 = some
    data: *mut T,        // pointer to value
}
```

**Benefits:**
- **Single runtime representation**: Collections can store either
- **No enum overhead**: No runtime enum tag beyond discriminant
- **Easy error propagation**: Simple with match statements
- **Interop**: optional and result can wrap the same types

## Comparison with Other Languages

### vs Rust

Very similar:

```rust title="rust_comparison.rs"
// Rust
fn divide(a: i32, b: i32) -> result<i32, String> {
    if b == 0 {
        err("division by zero".to_string())
    } else {
        ok(a / b)
    }
}

// Mux
func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}
```

Differences:
- Mux uses explicit `return` statements
- Rust has `?` operator for error propagation (Mux doesn't)

### vs Go

Similar philosophy, different syntax:

```go title="go_comparison.go"
// Go
func divide(a, b int) (int, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

// Mux
func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}
```

Mux advantage:
- Type system enforces error handling
- Cannot ignore errors without explicit match

### vs Exceptions (Java/Python)

Different paradigm:

```python title="python_comparison.py"
# Python
def divide(a, b):
    if b == 0:
        raise ValueError("division by zero")
    return a / b

# Mux
func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}
```

Mux advantages:
- Errors visible in function signature
- Cannot forget to handle errors
- No runtime exceptions
- No try/catch blocks

## Best Practices

1. **Return result for fallible operations** - Parse failures, I/O operations, validation
2. **Return optional for nullable values** - Collection access, lookups, searches
3. **Match exhaustively** - Handle both success and error cases
4. **Use descriptive error messages** - Include context in error strings
5. **Early returns for errors** - Reduces nesting
6. **Use `_` for ignored values** - Makes intent explicit
7. **Don't overuse wildcards** - Match specific cases when possible
8. **Document error conditions** - What errors can a function return?
9. **Chain operations explicitly** - No `?` operator, use match
10. **Prefer result over panicking** - Explicit > implicit

## Common Patterns

### Validation

```mux title="validation_pattern.mux"
func validateAge(int age) returns result<int, string> {
    if age < 0 {
        return err("age cannot be negative")
    }
    if age > 150 {
        return err("age too large")
    }
    return ok(age)
}
```

### Lookup with Default

```mux title="lookup_default.mux"
func getOrDefault(map<string, int> data, string key, int default) returns int {
    match data.get(key) {
        some(value) { return value }
        none { return default }
    }
}
```

### Transform result

```mux title="transform_result.mux"
func doubleIfEven(int n) returns result<int, string> {
    if n % 2 == 0 {
        return ok(n * 2)
    }
    return err("number is not even")
}

func processNumber(int n) returns result<string, string> {
    match doubleIfEven(n) {
        ok(doubled) {
            return ok("Doubled: " + doubled.to_string())
        }
        err(msg) {
            return err(msg)
        }
    }
}
```

## See Also

- [Enums](./enums.md) - result and optional as tagged unions
- [Control Flow](./control-flow.md) - Pattern matching with match
- [Types](./types.md) - Fallible type conversions
- [Collections](./collections.md) - Safe collection access with optional
