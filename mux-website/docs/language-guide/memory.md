# Memory Management

Mux uses **automatic reference counting** (RC) for deterministic memory management without manual allocation or garbage collection.

## Overview

Mux's memory model provides:

- **No manual `free` or `delete`** - Memory cleaned up automatically
- **Deterministic cleanup** - Objects freed when reference count reaches zero
- **No garbage collection pauses** - Predictable performance
- **Heap allocation** - Objects and collections live on the heap
- **Value semantics for primitives** - Primitives passed by value

## Memory Safety

### No Null Pointers

Mux has no null pointers. Use `Optional<T>` instead:

```mux title="no_null_pointers.mux"
// No null
Optional<Circle> maybeCircle = None

// Must explicitly handle absence
match maybeCircle {
    Some(circle) {
        print(circle.radius.to_string())
    }
    None {
        print("No circle")
    }
}
```

### No Manual Memory Management

Cannot manually free memory or create dangling pointers:

```mux title="no_manual_memory.mux"
auto circle = Circle.new(5.0)
// No way to call free/delete
// Memory automatically freed when circle goes out of scope
```

### No Use-After-Free

Reference counting prevents use-after-free:

```mux title="no_use_after_free.mux"
auto list1 = [1, 2, 3]
auto list2 = list1    // refcount = 2

// Even if list1 goes out of scope, list2 is still valid
// Memory not freed until refcount = 0
```

## Reference Counting Basics

Every heap-allocated value has a reference count that tracks how many references point to it:

```mux title="reference_counting.mux"
// Create object (refcount = 1)
auto circle1 = Circle.new(5.0)

// Create another reference (refcount = 2)
auto circle2 = circle1

// circle2 goes out of scope (refcount = 1)
// circle1 goes out of scope (refcount = 0, memory freed)
```

## Memory Layout

All heap-allocated values use a reference counting header. The `RefHeader` uses `AtomicUsize` for thread-safe atomic operations.

## Automatic Cleanup

### Scope-Based Cleanup

Variables are cleaned up when they go out of scope:

```mux title="scope_cleanup.mux"
func example() returns void {
    auto nums = [1, 2, 3]      // Allocated (refcount = 1)
    
    if true {
        auto temp = nums       // refcount = 2
        print(temp.size().to_string())
    }  // temp goes out of scope (refcount = 1)
    
    print(nums.size().to_string())
}  // nums goes out of scope (refcount = 0, freed)
```

### Early Returns

Cleanup happens even with early returns:

```mux title="early_return_cleanup.mux"
func process(int value) returns Result<int, string> {
    auto data = [1, 2, 3, 4, 5]  // Allocated
    
    if value < 0 {
        return Err("negative")    // data cleaned up before return
    }
    
    if value > 100 {
        return Err("too large")   // data cleaned up before return
    }
    
    return Ok(value)
}  // data cleaned up at end
```

## Reference Count Operations

### Increment (`mux_rc_inc`)

Reference count increases when:
- Creating a new reference to existing value
- Assigning to a new variable
- Passing as a function argument
- Adding to a collection

```mux title="rc_increment.mux"
auto list1 = [1, 2, 3]    // refcount = 1
auto list2 = list1        // refcount = 2 (rc_inc called)
auto list3 = list1        // refcount = 3 (rc_inc called)
```

### Decrement (`mux_rc_dec`)

Reference count decreases when:
- Variable goes out of scope
- Variable is reassigned
- Function returns (cleanup of local variables)

```mux title="rc_decrement.mux"
{
    auto data = [1, 2, 3]     // refcount = 1
    auto ref = data           // refcount = 2
}  // ref destroyed (rc_dec, refcount = 1)
   // data destroyed (rc_dec, refcount = 0, memory freed)
```

When `mux_rc_dec` returns `true`, the refcount reached zero and memory is freed automatically.

## Collections and Reference Counting

Collections are RC-allocated and contain RC-allocated values:

```mux title="collections_rc.mux"
auto nums = [1, 2, 3]         // list refcount = 1
                              // each int is boxed with refcount = 1

auto nums2 = nums             // list refcount = 2
                              // ints' refcounts unchanged (shared)
```

### Nested Collections

When a collection is freed, all contained values have their refcounts decremented:

```mux title="nested_collections_rc.mux"
auto nested = [[1, 2], [3, 4]]
// Outer list: refcount = 1
// Inner lists: refcount = 1 each
// Ints: refcount = 1 each

// When nested is freed:
// 1. Outer list refcount -> 0, freed
// 2. Inner lists refcount -> 0, freed
// 3. Ints refcount -> 0, freed
```

## Objects and Reference Counting

Class instances use reference counting:

```mux title="objects_rc.mux"
class Person {
    string name
    int age
}

auto person1 = Person.new()    // refcount = 1
person1.name = "Alice"
person1.age = 30

auto person2 = person1         // refcount = 2 (same object)

person2.age = 31
print(person1.age.to_string()) // "31" - same object
```

When all references are gone, the object is freed:

```mux title="object_cleanup.mux"
{
    auto p = Person.new()      // refcount = 1
    p.name = "Bob"
}  // p goes out of scope, refcount = 0, object freed
```

## Scope Tracking

The compiler generates cleanup code using a scope stack:

1. **Enter scope** → `push_rc_scope()` (function entry, if-block, loop-body, match-arm)
2. **Track variable** → `track_rc_variable(name, alloca)` for each RC-allocated variable
3. **Exit scope** → `generate_all_scopes_cleanup()` iterates through all scopes in reverse order

This ensures proper cleanup order and handles early returns.

## Circular References

**Warning:** Mux's reference counting cannot automatically break circular references:

```mux title="circular_reference.mux"
// CAREFUL: This could create a cycle
class Node {
    int value
    Optional<Node> next
}

auto node1 = Node.new()
auto node2 = Node.new()

node1.next = Some(node2)
node2.next = Some(node1)  // Circular reference!

// These nodes will never be freed automatically
```

**Solution:** Avoid circular structures, or break cycles manually before scope exit.

## Value vs Reference Semantics

### Primitives (Value Semantics)

Primitives are passed by value (copied):

```mux title="value_semantics.mux"
auto x = 42
auto y = x     // y is a copy
y = 100        // x is still 42
```

### Objects (Reference Semantics)

Objects are passed by reference (shared):

```mux title="reference_semantics.mux"
auto circle1 = Circle.new(5.0)
auto circle2 = circle1    // Same object, not a copy

circle2.radius = 10.0
print(circle1.radius.to_string())  // "10.0" - same object
```

### Collections (Reference Semantics)

Collections are passed by reference:

```mux title="collections_reference.mux"
auto list1 = [1, 2, 3]
auto list2 = list1    // Same list, not a copy

list2.push_back(4)
print(list1.size().to_string())  // "4" - same list
```

## References

Mux supports explicit references for passing values by reference:

```mux title="memory_refs.mux"
// Basic reference usage
int x = 10
auto r = &x      // r is of type &int
print("ref value: " + (*r).to_string())  // 10 - explicit dereference with *

*r = 20          // Changes x to 20 via dereference
print("x is now: " + x.to_string())  // 20

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

**Design Note:** Unlike some languages with automatic dereferencing, Mux requires explicit `*` for all reference operations. This makes memory access patterns explicit.

## Performance Considerations

### Atomic Operations

Reference counting uses atomic operations for thread safety:
- **Overhead**: Atomic increment/decrement on each reference change
- **Cache**: Reference count header may cause cache misses

### Compared to Garbage Collection

**Advantages:**
- Deterministic cleanup (no GC pauses)
- Predictable performance
- Lower memory overhead (no GC metadata)

**Tradeoffs:**
- Cannot break circular references automatically
- Atomic operations have cost
- Must track references carefully

### Compared to Manual Management

**Advantages:**
- No manual `free` calls
- No use-after-free bugs
- No double-free bugs

**Tradeoffs:**
- Cannot control exact deallocation time
- Reference count overhead

## See Also

- [Types](./types.md) - Value types and boxing
- [Classes](./classes.md) - Object lifecycle
- [Collections](./collections.md) - Collection memory management
- [Error Handling](./error-handling.md) - Optional and Result
