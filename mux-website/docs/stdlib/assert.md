# Assert Module

The `assert` module provides assertion functions for testing and validation. All assertion functions panic immediately on failure with descriptive error messages.

## Import

```mux
import std.assert
```

## Basic Assertions

| Function | Description |
|----------|-------------|
| `assert.assert_true(bool condition) returns void` | Panics if condition is false |
| `assert.assert_false(bool condition) returns void` | Panics if condition is true |
| `assert.assert(bool condition, string message) returns void` | Panics with custom message if condition is false |

## Equality Assertions

| Function | Description |
|----------|-------------|
| `assert.assert_eq(T actual, T expected) returns void` | Panics if values are not equal |
| `assert.assert_ne(T actual, T expected) returns void` | Panics if values are equal |

These functions are generic and work with any type `T`.

## optional Assertions

| Function | Description |
|----------|-------------|
| `assert.assert_some(optional<T> value) returns void` | Panics if value is none |
| `assert.assert_none(optional<T> value) returns void` | Panics if value is some |

## result Assertions

| Function | Description |
|----------|-------------|
| `assert.assert_ok(result<T, E> value) returns void` | Panics if value is err |
| `assert.assert_err(result<T, E> value) returns void` | Panics if value is ok |

## Example

```mux title="assert_example.mux"
import std.assert

func main() returns void {
    assert.assert_true(true)
    assert.assert_false(false)
    
    assert.assert_eq(5, 5)
    assert.assert_ne(3, 4)
    
    assert.assert(true, "this should pass")
    
    auto opt_some = some(42)
    assert.assert_some(opt_some)
    
    auto opt_none = none
    assert.assert_none(opt_none)
    
    auto ok_val = ok(100)
    assert.assert_ok(ok_val)
    
    auto err_val = err("error message")
    assert.assert_err(err_val)
    
    print("All assertions passed!")
}
```

## Error Messages

When an assertion fails, the program panics with a descriptive message:

```mux
assert.assert_eq(1, 2)  // Panics: "Assertion failed: expected 2, got 1"
assert.assert_true(false)  // Panics: "Assertion failed: expected true, got false"
assert.assert_none(some(42))  // Panics: "Assertion failed: expected none, got some(42)"
```
