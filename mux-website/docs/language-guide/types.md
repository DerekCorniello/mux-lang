# Types

Mux uses **strict static typing** with **NO implicit type conversions**. All type conversions must be explicit using conversion methods.

## Primitive Types

```mux
int      // 64-bit signed integer
float    // 64-bit IEEE-754
bool     // true | false
char     // Unicode code point
string   // UTF-8 sequence
```

## Type Conversions

### Numeric Conversions

```mux
// Integer conversions
auto x = 42
auto x_float = x.to_float()     // int -> float
auto x_str = x.to_string()      // int -> string
auto x_same = x.to_int()        // int -> int (identity)

// Float conversions
auto pi = 3.14
auto pi_int = pi.to_int()       // float -> int (truncates: 3)
auto pi_str = pi.to_string()    // float -> string
auto pi_same = pi.to_float()    // float -> float (identity)

// Boolean conversions
auto flag = true
auto flag_int = flag.to_int()   // bool -> int (true=1, false=0)
auto flag_float = flag.to_float() // bool -> float (true=1.0, false=0.0)
auto flag_str = flag.to_string() // bool -> string ("true" or "false")

// Char conversions
auto ch = 'A'
auto ch_str = ch.to_string()    // char -> string

// Method calls on literals require parentheses
auto num = (3).to_string()      // Valid
auto val = (42).to_float()      // Valid
// auto bad = 3.to_string()     // ERROR: parsed as float 3.0
```

### String Parsing (Fallible Conversions)

String and char parsing methods return `Result<T, string>` because they can fail:

```mux
// String to number (returns Result)
auto num_str = "42"
auto result = num_str.to_int()
match result {
    Ok(value) {
        print("Parsed: " + value.to_string())  // "Parsed: 42"
    }
    Err(error) {
        print("Parse error: " + error)
    }
}

// String to float
auto float_str = "3.14159"
auto float_result = float_str.to_float()
match float_result {
    Ok(value) { print(value.to_string()) }
    Err(msg) { print("Error: " + msg) }
}

// Char to digit (only works for '0'-'9')
auto digit_char = '5'
auto digit_result = digit_char.to_int()
match digit_result {
    Ok(digit) { print(digit.to_string()) }  // "5"
    Err(msg) { print(msg) }
}
```

### No Implicit Conversions

The following operations are **compile-time errors**:

```mux
// Type mismatches in binary operations
auto bad1 = 1 + 1.0        // ERROR: cannot add int and float
auto bad2 = "hello" + 3    // ERROR: cannot add string and int
auto bad3 = true + false   // ERROR: cannot add bool and bool

// Type mismatches in comparisons
auto bad4 = 1 < 1.0        // ERROR: cannot compare int and float
auto bad5 = "a" == 1       // ERROR: cannot compare string and int

// Correct usage requires explicit conversion
auto good1 = 1 + (1.0).to_int()           // OK: 2
auto good2 = "hello" + (3).to_string()    // OK: "hello3"
auto good3 = 1.to_float() < 1.0           // OK: true
```

## Conversion Methods Reference

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
| `char` | `.to_int()` | `Result<int, string>` | Digit value for '0'-'9' only |
| `string` | `.to_string()` | `string` | Identity function |
| `string` | `.to_int()` | `Result<int, string>` | Parses string as integer |
| `string` | `.to_float()` | `Result<float, string>` | Parses string as float |

## Composite Types

```mux
Optional<T>        // Represents a value that may or may not exist
Result<T, E>       // Represents success (T) or error (E)
list<T>            // Ordered collection
map<K, V>          // Key-value pairs
set<T>             // Unique elements
```

## References

Mux supports references for safe memory access:

```mux
// Basic reference usage
int x = 10
auto r = &x      // r is of type &int
print("ref value: " + (*r).to_string())  // 10 - explicit dereference with *

*r = 20          // Changes x to 20 via dereference
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

## Technical Implementation

<!-- TODO: Add more details about the Value enum implementation and boxing strategy once runtime details are finalized -->

Mux uses a unified `Value` enum to represent all runtime values, enabling uniform handling in collections and generics. All primitives are boxed into `*mut Value` pointers for consistent memory management.

## See Also

- [Variables](./variables.md) - Variable declarations and constants
- [Collections](./collections.md) - Lists, maps, and sets
- [Error Handling](./error-handling.md) - Using Result and Optional
