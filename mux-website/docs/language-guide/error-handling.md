# Error Handling

Mux uses explicit error handling through the `Result&lt;T, E&gt;` and `Optional&lt;T&gt;` types, avoiding exceptions entirely.

## Result&lt;T, E&gt;

The `Result` type represents operations that can succeed with a value of type `T` or fail with an error of type `E`.

### Basic Usage

```mux title="result_basic.mux"
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

auto result = divide(10, 2)

match result {
    Ok(value) {
        print("Result: " + value.to_string())  // "Result: 5"
    }
    Err(error) {
        print("Error: " + error)
    }
}
```

### Result Variants

```mux title="result_variants.mux"
enum Result<T, E> {
    Ok(T)      // Success case with value
    Err(E)     // Error case with error value
}
```

### Creating Result Values

```mux title="creating_results.mux"
// Success
auto success = Ok(42)                    // Result<int, E>
auto success2 = Ok("completed")          // Result<string, E>

// Failure
auto failure = Err("something went wrong")  // Result<T, string>
auto failure2 = Err(404)                    // Result<T, int>

// Explicit typing when needed
Result<int, string> result = Ok(100)
```

### Result Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_ok()` | `bool` | Returns `true` if the Result is an Ok variant |
| `.is_err()` | `bool` | Returns `true` if the Result is an Err variant |
| `.to_string()` | `string` | String representation |

```mux title="result_methods.mux"
Result<int, string> res1 = Ok(42)
Result<int, string> res2 = Err("error")

print(res1.is_ok().to_string())   // true
print(res1.is_err().to_string())  // false
print(res2.is_ok().to_string())   // false
print(res2.is_err().to_string())  // true
```

### Pattern Matching Results

```mux title="pattern_matching_results.mux"
func parse_int(string s) returns Result<int, string> {
    auto result = s.to_int()  // returns Result<int, string>
    return result
}

auto parsed = parse_int("42")

match parsed {
    Ok(value) {
        auto message = "Parsed: " + value.to_string()
        print(message)
    }
    Err(error) {
        print("Parse error: " + error)
    }
}
```

### Ignoring Error Details

Use `_` when you don't need the error value:

```mux title="ignoring_error_details.mux"
match result {
    Ok(value) {
        print("Success: " + value.to_string())
    }
    Err(_) {
        print("Some error occurred")  // don't care about details
    }
}
```

## Optional&lt;T&gt;

The `Optional` type represents values that may or may not exist.

### Basic Usage

```mux title="optional_basic.mux"
func findEven(list<int> xs) returns Optional<int> {
    for x in xs {
        if x % 2 == 0 {
            return Some(x)
        }
    }
    return None
}

auto maybeEven = findEven([1, 3, 4, 7])

match maybeEven {
    Some(value) {
        print("Found even: " + value.to_string())  // "Found even: 4"
    }
    None {
        print("No even number found")
    }
}
```

### Optional Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_some()` | `bool` | Returns `true` if the Optional contains a value |
| `.is_none()` | `bool` | Returns `true` if the Optional is empty |
| `.to_string()` | `string` | String representation |

```mux title="optional_methods.mux"
Optional<int> opt1 = Some(42)
Optional<int> opt2 = None

print(opt1.is_some().to_string())  // true
print(opt1.is_none().to_string())  // false
print(opt2.is_some().to_string())  // false
print(opt2.is_none().to_string())  // true
```

### Result Methods

```mux title="optional_variants.mux"
enum Optional<T> {
    Some(T)    // Value present
    None       // Value absent
}
```

### Creating Optional Values

```mux title="creating_optionals.mux"
// With value
auto present = Some(42)           // Optional<int>
auto present2 = Some("hello")     // Optional<string>

// Without value
auto absent = None                // Optional<T> (generic)

// Explicit typing
Optional<int> maybeNumber = Some(100)
Optional<string> maybeText = None
```

### Safe Collection Access

Collections return `Optional<T>` for safe access:

```mux title="safe_collection_access.mux"
auto nums = [10, 20, 30]

// Safe access with .get()
match nums.get(0) {
    Some(first) {
        print("First: " + first.to_string())  // "First: 10"
    }
    None {
        print("Index out of bounds")
    }
}

// Out of bounds
match nums.get(100) {
    Some(value) {
        print("Found: " + value.to_string())
    }
    None {
        print("Index out of bounds")  // This prints
    }
}
```

### Map Lookups

```mux title="map_lookups.mux"
auto scores = {"Alice": 90, "Bob": 85}

match scores.get("Alice") {
    Some(score) {
        print("Alice's score: " + score.to_string())
    }
    None {
        print("Student not found")
    }
}

match scores.get("Charlie") {
    Some(score) {
        print("Score: " + score.to_string())
    }
    None {
        print("Charlie not found")  // This prints
    }
}
```

### Ignoring the Value

Use `_` when you only care about presence/absence:

```mux title="ignoring_values.mux"
match maybeValue {
    Some(_) {
        print("Got a value")  // don't care what it is
    }
    None {
        print("Got nothing")
    }
}
```

## Combining Result and Optional

### Optional of Result

```mux title="optional_of_result.mux"
func tryParse(Optional<string> maybeStr) returns Optional<Result<int, string>> {
    match maybeStr {
        Some(s) {
            return Some(s.to_int())  // Result<int, string>
        }
        None {
            return None
        }
    }
}
```

### Result of Optional

```mux title="result_of_optional.mux"
func getRequired(map<string, int> data, string key) returns Result<int, string> {
    match data.get(key) {
        Some(value) {
            return Ok(value)
        }
        None {
            return Err("Key '" + key + "' not found")
        }
    }
}
```

## Error Propagation Patterns

### Early Returns

```mux title="early_returns.mux"
func processData(string input) returns Result<int, string> {
    // Validate input
    if input == "" {
        return Err("empty input")
    }
    
    // Parse input
    auto parsed = input.to_int()
    match parsed {
        Ok(value) {
            // Continue processing
            if value < 0 {
                return Err("negative values not allowed")
            }
            return Ok(value * 2)
        }
        Err(msg) {
            return Err("parse error: " + msg)
        }
    }
}
```

### Nested Matching

```mux title="nested_matching.mux"
func complexOperation() returns Result<string, string> {
    auto step1 = firstOperation()
    
    match step1 {
        Ok(value1) {
            auto step2 = secondOperation(value1)
            
            match step2 {
                Ok(value2) {
                    return Ok(value2)
                }
                Err(err2) {
                    return Err("step2 failed: " + err2)
                }
            }
        }
        Err(err1) {
            return Err("step1 failed: " + err1)
        }
    }
}
```

## Fallible Type Conversions

String and char parsing return `Result` because they can fail:

```mux title="fallible_conversions.mux"
// String to int
auto num_str = "42"
auto result = num_str.to_int()  // Result<int, string>

match result {
    Ok(value) {
        print("Parsed: " + value.to_string())
    }
    Err(error) {
        print("Parse error: " + error)
    }
}

// String to float
auto float_str = "3.14159"
auto float_result = float_str.to_float()  // Result<float, string>

// Char to digit (only works for '0'-'9')
auto digit_char = '5'
auto digit_result = digit_char.to_int()  // Result<int, string>

match digit_result {
    Ok(digit) { print(digit.to_string()) }  // "5"
    Err(msg) { print(msg) }
}

// Non-digit character
auto letter = 'A'
auto letter_result = letter.to_int()

match letter_result {
    Ok(_) { print("Unexpected success") }
    Err(msg) { print(msg) }  // "Character is not a digit (0-9)"
}
```

## Technical Implementation

### Memory Layout

Both types use a uniform runtime representation:

```rust title="memory_layout.rs"
pub struct Result<T, E> {
    discriminant: i32,    // 0 = Ok, 1 = Err
    data: *mut T,        // pointer to value
}

pub struct Optional<T> {
    discriminant: i32,    // 0 = None, 1 = Some
    data: *mut T,        // pointer to value
}
```

**Benefits:**
- **Single runtime representation**: Collections can store either
- **No enum overhead**: No runtime enum tag beyond discriminant
- **Easy error propagation**: Simple with match statements
- **Interop**: Optional and Result can wrap the same types

## Comparison with Other Languages

### vs Rust

Very similar:

```rust title="rust_comparison.rs"
// Rust
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        Err("division by zero".to_string())
    } else {
        Ok(a / b)
    }
}

// Mux
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
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
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
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
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}
```

Mux advantages:
- Errors visible in function signature
- Cannot forget to handle errors
- No runtime exceptions
- No try/catch blocks

## Best Practices

1. **Return Result for fallible operations** - Parse failures, I/O operations, validation
2. **Return Optional for nullable values** - Collection access, lookups, searches
3. **Match exhaustively** - Handle both success and error cases
4. **Use descriptive error messages** - Include context in error strings
5. **Early returns for errors** - Reduces nesting
6. **Use `_` for ignored values** - Makes intent explicit
7. **Don't overuse wildcards** - Match specific cases when possible
8. **Document error conditions** - What errors can a function return?
9. **Chain operations explicitly** - No `?` operator, use match
10. **Prefer Result over panicking** - Explicit > implicit

## Common Patterns

### Validation

```mux title="validation_pattern.mux"
func validateAge(int age) returns Result<int, string> {
    if age < 0 {
        return Err("age cannot be negative")
    }
    if age > 150 {
        return Err("age too large")
    }
    return Ok(age)
}
```

### Lookup with Default

```mux title="lookup_default.mux"
func getOrDefault(map<string, int> data, string key, int default) returns int {
    match data.get(key) {
        Some(value) { return value }
        None { return default }
    }
}
```

### Transform Result

```mux title="transform_result.mux"
func doubleIfEven(int n) returns Result<int, string> {
    if n % 2 == 0 {
        return Ok(n * 2)
    }
    return Err("number is not even")
}

func processNumber(int n) returns Result<string, string> {
    match doubleIfEven(n) {
        Ok(doubled) {
            return Ok("Doubled: " + doubled.to_string())
        }
        Err(msg) {
            return Err(msg)
        }
    }
}
```

## See Also

- [Enums](./enums.md) - Result and Optional as tagged unions
- [Control Flow](./control-flow.md) - Pattern matching with match
- [Types](./types.md) - Fallible type conversions
- [Collections](./collections.md) - Safe collection access with Optional
