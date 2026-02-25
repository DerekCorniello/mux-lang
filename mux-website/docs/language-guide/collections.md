# Collections

Mux provides three main collection types: lists, maps, and sets, with Python-style literal syntax.

## Lists

Ordered, mutable collections of elements of the same type.

### Creating Lists

```mux title="creating_lists.mux"
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
| `.get(int index)` | `optional<T>` | Safe access; returns `some(value)` or `none` if out of bounds |
| `[int index]` | `T` | Direct access; runtime error if out of bounds |
| `.push(T item)` | `void` | Appends item to the front |
| `.push_back(T item)` | `void` | Appends item to the end |
| `.pop()` | `optional<T>` | Removes and returns first item, or `none` if empty |
| `.pop_back()` | `optional<T>` | Removes and returns last item, or `none` if empty |
| `.sort()` | `void` | Sorts the list in place (requires comparable `T`) |
| `.reverse()` | `void` | Reverses the list in place |
| `.contains(T item)` | `bool` | Returns `true` if item exists in the list |
| `.index_of(T item)` | `optional<int>` | Returns index of first match, or `none` |
| `.find(func(T) returns bool)` | `optional<T>` | Returns first element matching predicate, or `none` |
| `.filter(func(T) returns bool)` | `list<T>` | Returns elements that satisfy predicate |
| `.map(func(T) returns U)` | `list<U>` | Transforms each element into a new list |
| `.reduce(U init, func(U, T) returns U)` | `U` | Folds list into one value |
| `.to_string()` | `string` | String representation of the list |

### List Operations

```mux title="list_operations.mux"
auto nums = [1, 2, 3]

// Safe access with optional
match nums.get(0) {
    some(first) { print(first.to_string()) }  // "1"
    none { print("Index out of bounds") }
}

// Direct access (runtime error if index invalid)
auto second = nums[1]  // 2

// Mutation
nums.push_back(4)      // [1, 2, 3, 4]
nums.push(5)           // [5, 1, 2, 3, 4]

match nums.pop_back() {
    some(last) { print(last.to_string()) }  // "5"
    none { }
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

```mux title="iterating_lists.mux"
auto items = [10, 20, 30, 40, 50]

// For loop
for int item in items {
    print(item.to_string())
}

// With indices using range
for int i in range(0, items.size()) {
    auto item = items[i]
    print("Index " + i.to_string() + ": " + item.to_string())
}
```

## Maps

Key-value pairs with unique keys.

### Creating Maps

```mux title="creating_maps.mux"
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
| `.get(K key)` | `optional<V>` | Safe lookup; returns `some(value)` or `none` if key not found |
| `[K key]` | `V` | Direct access; runtime error if key not found |
| `.put(K key, V value)` | `void` | Inserts or updates a key-value pair |
| `.contains(K key)` | `bool` | Returns `true` if key exists in map |
| `.remove(K key)` | `optional<V>` | Removes key and returns value, or `none` if key not found |
| `.to_string()` | `string` | String representation of the map |
| `.get_keys()` | `list<K>` | List of keys from the map |
| `.get_values()` | `list<V>` | List of values from the map |
| `.get_pairs()` | `list<tuple<K, V>>` | List of key-value pairs |
| `.filter(func(K, V) returns bool)` | `map<K, V>` | Returns entries matching predicate |

### Map Operations

```mux title="map_operations.mux"
auto scores = {"Alice": 90, "Bob": 85}

// Safe access
match scores.get("Alice") {
    some(score) { print(score.to_string()) }  // "90"
    none { print("Student not found") }
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
    some(value) { print("Removed Bob with score: " + value.to_string()) }
    none { print("Bob not found") }
}

// Merge (latter wins on key collision)
auto map1 = {"a": 1, "b": 2}
auto map2 = {"b": 3, "c": 4}
auto merged = map1 + map2        // {"a": 1, "b": 3, "c": 4}
```

## Sets

### Creating Sets

```mux title="creating_sets.mux"
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
| `.remove(T item)` | `optional<T>` | Removes item and returns `some(item)` if it existed, or `none` |
| `.to_string()` | `string` | String representation of the set |

### Set Operations

```mux title="set_operations.mux"
auto tags = {"urgent", "important", "review"}

print(tags.size().to_string())  // "3"

// Add and check membership
tags.add("priority")
if tags.contains("urgent") {
    print("Has urgent tag")
}

// Remove item
match tags.remove("review") {
    some(removed) { print("Removed: " + removed) }
    none { print("Item not found") }
}

// Set operators
auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}
auto diff = set1 - set2          // {1, 2}
auto common = set1 / set2        // {3}
```

## The `in` Operator

Test for membership/containment:

```mux title="in_operator.mux"
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

```mux title="nested_collections.mux"
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

```mux title="collection_conversions.mux"
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

```mux title="generic_collections.mux"
func first<T>(list<T> items) returns optional<T> {
    if items.is_empty() {
        return none.new()
    }
    return some.new(items[0])
}

func lookup<K, V>(map<K, V> data, K key) returns optional<V> {
    return data.get(key)
}

// Usage
auto nums = [1, 2, 3]
auto maybeFirst = first<int>(nums)

auto scores = {"Alice": 90, "Bob": 85}
auto aliceScore = lookup<string, int>(scores, "Alice")
```

## Collection Literals vs Constructors

```mux title="collection_literals.mux"
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

1. **Use safe access methods** - `.get()` returns `optional<T>` to avoid runtime errors
2. **Explicit types for empty collections** - Prevents ambiguity
3. **Leverage type inference** - Use `auto` when types are obvious from literals
4. **Use the `in` operator** - Cleaner than calling `.contains()`
5. **Prefer literals over constructors** - More readable
6. **Use appropriate collection type**:
   - **List**: Ordered, indexed access, duplicates allowed
   - **Map**: Key-value pairs, unique keys
   - **Set**: Unique elements, membership testing
7. **Match on fallible operations** - Handle `none` cases explicitly

## See Also

- [Types](./types.md) - Type system and conversions
- [Generics](./generics.md) - Generic functions with collections
- [Error Handling](./error-handling.md) - optional&lt;T&gt; for safe access
- [Memory](./memory.md) - Reference counting for collections
