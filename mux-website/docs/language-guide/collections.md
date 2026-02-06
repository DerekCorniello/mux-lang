# Collections

Mux provides three main collection types: lists, maps, and sets, with Python-style literal syntax.

## Lists

Ordered, mutable collections of elements of the same type.

### Creating Lists

```mux
// Explicit typing
list<int> nums = [1, 2, 3, 4]
list<string> names = ["Alice", "Bob", "Charlie"]

// With type inference
auto nums = [1, 2, 3, 4]           // inferred as list<int>
auto names = ["Alice", "Bob"]      // inferred as list<string>

// Empty list requires explicit type
list<int> empty = []
auto empty2 = list<int>()

// Nested lists
list<list<int>> matrix = [[1, 2], [3, 4]]
auto matrix2 = [[1, 2], [3, 4]]    // inferred
```

### List Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Number of elements in the list |
| `.is_empty()` | `bool` | Returns `true` if list has no elements |
| `.get(int index)` | `Optional<T>` | Safe access; returns `Some(value)` or `None` if out of bounds |
| `[int index]` | `T` | Direct access; runtime error if out of bounds |
| `.push(T item)` | `void` | Appends item to the end (alias for push_back) |
| `.push_back(T item)` | `void` | Appends item to the end |
| `.pop()` | `Optional<T>` | Removes and returns last item, or `None` if empty |
| `.pop_back()` | `Optional<T>` | Removes and returns last item, or `None` if empty |
| `.to_string()` | `string` | String representation of the list |

### List Operations

```mux
auto nums = [1, 2, 3]

// Safe access with Optional
match nums.get(0) {
    Some(first) { print(first.to_string()) }  // "1"
    None { print("Index out of bounds") }
}

// Direct access (runtime error if index invalid)
auto second = nums[1]  // 2

// Mutation
nums.push_back(4)      // [1, 2, 3, 4]
nums.push(5)           // [1, 2, 3, 4, 5]

match nums.pop_back() {
    Some(last) { print(last.to_string()) }  // "5"
    None { }
}

// Inspection
print(nums.size().to_string())       // "4"
print(nums.is_empty().to_string())   // "false"

// Concatenation
auto list1 = [1, 2]
auto list2 = [3, 4]
auto combined = list1 + list2        // [1, 2, 3, 4]
```

### Iterating Lists

```mux
auto items = [10, 20, 30, 40, 50]

// For loop
for item in items {
    print(item.to_string())
}

// With indices using range
for i in range(0, items.size()) {
    auto item = items[i]
    print("Index " + i.to_string() + ": " + item.to_string())
}
```

## Maps

Key-value pairs with unique keys.

### Creating Maps

```mux
// Explicit typing
map<string, int> scores = {"Alice": 90, "Bob": 85}

// With type inference
auto ages = {"Alice": 30, "Bob": 25}  // map<string, int>

// Empty map requires explicit type
map<string, int> empty = {}
auto empty2 = map<string, int>()

// Nested maps
map<string, map<string, int>> data = {
    "user1": {"score": 100, "level": 5},
    "user2": {"score": 200, "level": 10}
}
```

### Map Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Number of key-value pairs |
| `.is_empty()` | `bool` | Returns `true` if map has no entries |
| `.get(K key)` | `Optional<V>` | Safe lookup; returns `Some(value)` or `None` if key not found |
| `[K key]` | `V` | Direct access; runtime error if key not found |
| `.put(K key, V value)` | `void` | Inserts or updates a key-value pair |
| `.contains(K key)` | `bool` | Returns `true` if key exists in map |
| `.remove(K key)` | `Optional<V>` | Removes key and returns value, or `None` if key not found |
| `.to_string()` | `string` | String representation of the map |

### Map Operations

```mux
auto scores = {"Alice": 90, "Bob": 85}

// Safe access
match scores.get("Alice") {
    Some(score) { print(score.to_string()) }  // "90"
    None { print("Student not found") }
}

// Direct access
auto bobScore = scores["Bob"]  // 85

// Update/insert
scores.put("Alice", 95)
scores["Charlie"] = 88

// Check membership
if scores.contains("Alice") {
    print("Alice found")
}

// Remove
match scores.remove("Bob") {
    Some(value) { print("Removed Bob with score: " + value.to_string()) }
    None { print("Bob not found") }
}

// Merge (latter wins on key collision)
auto map1 = {"a": 1, "b": 2}
auto map2 = {"b": 3, "c": 4}
auto merged = map1 + map2        // {"a": 1, "b": 3, "c": 4}
```

### Map Implementation

Mux uses `BTreeMap` (not `HashMap`):

**Benefits:**
- **Deterministic iteration order**: Always the same order
- **Ordered operations**: First/last element, range queries
- **Reproducible output**: `to_string()` produces consistent results

## Sets

Collections of unique elements.

### Creating Sets

```mux
// Explicit typing
set<int> numbers = {1, 2, 3, 4}
set<string> tags = {"urgent", "important", "review"}

// With type inference
auto nums = {1, 2, 3}             // set<int> (requires all same type)
auto words = {"hello", "world"}   // set<string>

// Empty set requires explicit type
set<int> empty = {}
auto empty2 = set<int>()
```

### Set Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Number of elements |
| `.is_empty()` | `bool` | Returns `true` if set is empty |
| `.add(T item)` | `void` | Adds an item to the set |
| `.contains(T item)` | `bool` | Returns `true` if item exists in set |
| `.remove(T item)` | `Optional<T>` | Removes item and returns it, or `None` if not found |
| `.to_string()` | `string` | String representation of the set |

### Set Operations

```mux
auto tags = {"urgent", "important", "review"}

print(tags.size().to_string())  // "3"

// Add and check membership
tags.add("priority")
if tags.contains("urgent") {
    print("Has urgent tag")
}

// Remove item
match tags.remove("review") {
    Some(removed) { print("Removed: " + removed) }
    None { print("Item not found") }
}

// Union
auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}
```

### Set Implementation

Mux uses `BTreeSet` for deterministic ordering.

## The `in` Operator

Test for membership/containment:

```mux
// List containment
auto nums = [1, 2, 3, 4, 5]
auto hasThree = 3 in nums     // true
auto hasTen = 10 in nums      // false

// Set containment
auto tags = {"urgent", "important"}
auto isUrgent = "urgent" in tags    // true

// String containment
auto msg = "hello world"
auto hasWorld = "world" in msg      // true
auto hasFoo = "foo" in msg          // false

// Character in string
auto hasO = 'o' in msg              // true
auto hasZ = 'z' in msg              // false
```

**Type Requirements:**
- Both operands must have compatible element types
- No implicit type conversions allowed

## Nested Collections

Collections can be arbitrarily nested:

```mux
// List of maps
auto users = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25}
]

// Map of lists
auto data = {
    "numbers": [1, 2, 3, 4, 5],
    "names": ["Alice", "Bob", "Charlie"]
}

// Complex nested structure
auto complex = {
    "users": [
        {"name": "Alice", "scores": [95, 87, 92]},
        {"name": "Bob", "scores": [78, 85, 90]}
    ]
}
```

## Collection Type Conversions

**No automatic conversions** - collections must have exact type match:

```mux
// ERROR: Type mismatch
// auto bad = [1, 2] + [3.0, 4.0]  // list<int> + list<float>

// Correct: Explicit conversion
auto ints = [1, 2]
auto floats = [3.0, 4.0]
auto ints_as_floats = [ints[0].to_float(), ints[1].to_float()]
auto combined = ints_as_floats + floats  // list<float>
```

## Generic Collections

Collections work seamlessly with generics:

```mux
func first<T>(list<T> items) returns Optional<T> {
    if items.is_empty() {
        return None.new()
    }
    return Some.new(items[0])
}

func lookup<K, V>(map<K, V> data, K key) returns Optional<V> {
    return data.get(key)
}

// Usage
auto nums = [1, 2, 3]
auto maybeFirst = first<int>(nums)

auto scores = {"Alice": 90, "Bob": 85}
auto aliceScore = lookup<string, int>(scores, "Alice")
```

## Collection Literals vs Constructors

```mux
// Literal syntax (preferred when possible)
auto nums = [1, 2, 3]
auto scores = {"Alice": 90, "Bob": 85}
auto tags = {"urgent", "important"}

// Constructor syntax (required for empty collections with auto)
auto empty_list = list<int>()
auto empty_map = map<string, int>()
auto empty_set = set<string>()

// Explicit type with literal (empty collections)
list<int> nums = []
map<string, int> scores = {}
set<string> tags = {}
```

## Best Practices

1. **Use safe access methods** - `.get()` returns `Optional<T>` to avoid runtime errors
2. **Explicit types for empty collections** - Prevents ambiguity
3. **Leverage type inference** - Use `auto` when types are obvious from literals
4. **Use the `in` operator** - Cleaner than calling `.contains()`
5. **Prefer literals over constructors** - More readable
6. **Use appropriate collection type**:
   - **List**: Ordered, indexed access, duplicates allowed
   - **Map**: Key-value pairs, unique keys
   - **Set**: Unique elements, membership testing
7. **Match on fallible operations** - Handle `None` cases explicitly

## Technical Implementation

### Uniform Value Representation

All collection elements are stored as `Value` enum:

```rust
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(OrderedFloat<f64>),
    String(String),
    List(Vec<Value>),
    Map(BTreeMap<Value, Value>),
    Set(BTreeSet<Value>),
    Optional(Option<Box<Value>>),
    Result(Result<Box<Value>, String>),
    Object(ObjectRef),
}
```

This enables:
- **Generic collections**: `list<T>` works uniformly for all types
- **Arbitrary nesting**: Any collection can contain any other collection
- **Polymorphic functions**: Same function can handle any type

### Reference Counting

Collections are RC-allocated and contain RC-allocated values. When freed:
1. Collection's refcount reaches zero
2. Collection's container is dropped
3. Each contained `Value` has its refcount decremented
4. Nested collections are freed recursively

## See Also

- [Types](./types.md) - Type system and conversions
- [Generics](./generics.md) - Generic functions with collections
- [Error Handling](./error-handling.md) - Optional&lt;T&gt; for safe access
- [Memory](./memory.md) - Reference counting for collections
