# Memory Model

Mux uses reference counting for deterministic memory management. This document describes the memory model in detail.

## Overview

Mux provides **automatic memory management** through **atomic reference counting (RC)**:

- No manual `free` or `delete`
- No garbage collector pauses
- Deterministic cleanup when references go out of scope
- Thread-safe reference count operations

## Memory Layout

### Heap Allocation

All non-primitive values are heap-allocated

### Value Types

| Type | Storage | Memory Management |
|------|---------|-------------------|
| `int`, `float`, `bool`, `char` | Inline (boxed in `Value` enum) | Reference counted |
| `string` | Heap-allocated UTF-8 | Reference counted |
| `list<T>` | Heap-allocated vector | Reference counted |
| `map<K,V>` | Heap-allocated BTreeMap | Reference counted |
| `tuple<K,V>` | Heap-allocated object | Reference counted |
| `set<T>` | Heap-allocated BTreeSet | Reference counted |
| `optional<T>`, `result<T,E>` | Boxed enum | Reference counted |
| Class instances | Heap-allocated object | Reference counted |
| References (`&T`) | Pointer | Not RC'd (borrowed) |

## Reference Counting Operations

### Increment (`mux_rc_inc the reference count when`)

Increments creating a new reference:

```mux
auto a = some_function()      // Function returns new object
// ref_count incremented

auto b = a                     // New reference to same object
// ref_count incremented again

pass_to_function(a)           // Passed as argument
// ref_count incremented
```

**When increment happens:**
- Variable assignment
- Function argument passing
- Adding to collection
- Returning from function

### Decrement (`mux_rc_dec`)

Decrements the reference count when a reference goes out of scope:

```mux
func example() returns void {
    auto obj = create_object()  // ref_count = 1
    // ... use obj ...
}  // obj goes out of scope, ref_count decremented to 0, object freed
```

**When decrement happens:**
- Variable assignment (old value)
- Function return (local variables)
- Scope exit
- Collection element removal

### Cleanup

When `mux_rc_dec` returns `true`, the refcount reached zero and memory is freed:

1. For classes with destructors: call destructor
2. Free the allocation

## Scope-Based Tracking

The compiler generates cleanup code using a scope stack:

1. **Enter scope** -> `push_rc_scope()` (function entry, if-block, loop-body, match-arm)
2. **Track variable** -> `track_rc_variable(name, alloca)` for each RC-allocated variable
3. **Exit scope** -> `generate_all_scopes_cleanup()` iterates through all scopes in reverse order

### Example: Scope Cleanup

```mux
func process() returns void {
    auto a = create_obj()    // Track: a
    if condition {
        auto b = create_obj()  // Track: a, b
        return  // Cleanup b, then a
    }
    // b cleaned up here if condition false
    auto c = create_obj()    // Track: a, c
    // Cleanup c, then a
}
// Cleanup a
```

### Early Returns

```mux
func early_return(bool flag) returns void {
    auto resource = acquire()

    if flag {
        cleanup(resource)
        return  // Resource cleaned up before return
    }

    use(resource)
    // Resource cleaned up here
}
```

## Collections and Memory

Collections contain RC-allocated values. When freed:

1. Collection's refcount reaches zero
2. Collection's container (`Vec<Value>`, etc.) is dropped
3. Each contained `Value` has its refcount decremented
4. Nested collections are freed recursively

### Nested Collections

```mux
auto nested = [
    {"name": "Alice", "scores": [95, 87, 92]},
    {"name": "Bob", "scores": [78, 85, 90]}
]
```

Cleanup order:
1. Outer list refcount -> 0
2. Drop outer list
3. Each map's refcount -> 0
4. Drop each map
5. Each inner list's refcount -> 0
6. Drop inner lists
7. All strings freed

## Value Semantics

### Reference vs Value

```mux
auto original = Circle.new(5.0)
auto copy = original      // Both point to same object
copy.radius = 10          // original.radius is also 10!

auto primitive = 42
auto primitive_copy = primitive
primitive_copy = 100      // original is still 42
```

## References (`&T`)

References provide non-owning pointers to values:

```mux
int x = 10
auto r = &x       // r: &int, points to x
*r = 20           // x is now 20
```

### Reference Rules

- References are non-nullable
- No reference arithmetic
- References do not affect reference counts
- optional references: `optional<&T>`

```mux
func update(&int ref) returns void {
    *ref = *ref + 1  // Must dereference to read or write
}
```

## Memory Safety

### No Use-After-Free

Memory is only freed when all references are gone:

```mux
auto obj = create()
auto ref = obj
// ...
obj = none  // ref still valid
// ...
use(ref)    // Safe: refcount > 0
```

### No Double-Free

Reference counting prevents double-free:

```mux
auto a = create()
auto b = a
// Both point to same allocation
// Only freed when both a and b go out of scope
```

## Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| Allocation | O(1) amortized |
| Clone (increment) | O(1) atomic |
| Drop (decrement) | O(1) atomic |
| Clone collection | O(n) copy all elements |
| Clone map/set | O(n log n) insert all elements |

### Thread Safety

Reference count operations use atomic operations:

```rust
// mux_rc_inc
fn rc_inc(ptr: *mut Value) {
    let header = get_ref_header(ptr);
    header.ref_count.fetch_add(1, Ordering::AcqRel);
}

// mux_rc_dec
fn rc_dec(ptr: *mut Value) -> bool {
    let header = get_ref_header(ptr);
    if header.ref_count.fetch_sub(1, Ordering::AcqRel) == 1 {
        // Last reference - free memory
        free(ptr);
        true
    } else {
        false
    }
}
```

## See Also

- [Statements](./statements.md) - Statement memory semantics
- [Expressions](./expressions.md) - Expression value categories
- [Classes](../language-guide/classes.md) - Class memory behavior
