# Control Flow

Mux provides familiar control flow constructs with some unique features like pattern matching with guards.

## If / Else

Standard if-else branching with curly braces:

```mux
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}
```

### If as an Expression

In Mux, `if` can be used as an expression that returns a value:

```mux
auto message = if x > 0 { "positive" } else { "non-positive" }

auto status = if score >= 90 {
    "excellent"
} else if score >= 70 {
    "good"
} else {
    "needs improvement"
}
```

### Type Inference with If

```mux
// Type inferred from branches
auto result = if condition {
    calculateValue()
} else {
    defaultValue()
}
```

## For Loops

Mux uses range-based for loops only (no C-style `for (int i = 0; i < n; i++)`):

```mux
// Iterating over a list
auto items = [1, 2, 3, 4, 5]
for item in items {
    print(item.to_string())
}

// Using range() for numeric iteration
for i in range(0, 10) {
    print("Iteration: " + i.to_string())
}

// Iterating with type inference
for item in collection {
    auto processed = transform(item)  // item type inferred
    print(processed)
}
```

### Ignoring Loop Variables

Use `_` when you don't need the loop variable:

```mux
// Execute 10 times, don't care about index
for _ in range(0, 10) {
    doSomething()
}

// Destructuring with unused parts
for (key, _) in keyValuePairs {
    print("Key: " + key)  // value ignored
}
```

### Iterating Collections

```mux
// List iteration
auto nums = [10, 20, 30]
for n in nums {
    print(n.to_string())
}

// Map iteration (future feature)
// for (key, value) in myMap {
//     print(key + ": " + value.to_string())
// }
```

## While Loops

Standard while loops with boolean conditions:

```mux
auto count = 0
while count < 10 {
    print(count.to_string())
    count++
}

// With type inference for locals
while condition {
    auto currentTime = getCurrentTime()
    if currentTime > threshold {
        break
    }
}
```

## Break and Continue

Control loop execution:

```mux
// Break exits the loop
for i in range(0, 100) {
    if i > 10 {
        break
    }
    print(i.to_string())
}

// Continue skips to next iteration
for i in range(0, 10) {
    if i % 2 == 0 {
        continue  // skip even numbers
    }
    print(i.to_string())
}
```

**Note:** `break` and `continue` work in both `for` and `while` loops.

## Match Statements

Pattern matching with guards and destructuring:

### Basic Matching

```mux
match value {
    1 {
        print("one")
    }
    2 {
        print("two")
    }
    _ {
        print("other")  // wildcard pattern
    }
}
```

### Matching Optional

```mux
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
        print("Found even: " + value.to_string())
    }
    None {
        print("No even number found")
    }
}
```

### Matching Result

```mux
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

auto result = divide(10, 2)

match result {
    Ok(value) {
        print("Result: " + value.to_string())
    }
    Err(error) {
        print("Error: " + error)
    }
}
```

### Pattern Matching with Guards

Guards add conditional logic to patterns:

```mux
match value {
    Some(v) if v > 10 {
        print("large: " + v.to_string())
    }
    Some(v) if v > 0 {
        print("small positive: " + v.to_string())
    }
    Some(v) {
        print("non-positive: " + v.to_string())
    }
    None {
        print("no value")
    }
}
```

### Ignoring Values in Patterns

Use `_` to ignore parts of patterns you don't need:

```mux
// Ignore the wrapped value
match maybeValue {
    Some(_) {
        print("Got a value")  // don't care what it is
    }
    None {
        print("Got nothing")
    }
}

// Ignore error details
match result {
    Ok(value) {
        print("Success: " + value.to_string())
    }
    Err(_) {
        print("Some error occurred")  // don't care about details
    }
}
```

### Matching Enums

```mux
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

auto myShape = Circle.new(5.0)

match myShape {
    Circle(r) {
        print("Circle with radius: " + r.to_string())
    }
    Rectangle(w, h) {
        print("Rectangle: " + w.to_string() + "x" + h.to_string())
    }
    Square(s) {
        print("Square with size: " + s.to_string())
    }
}

// Ignoring enum data
match shape {
    Circle(_) {
        print("It's a circle")  // radius ignored
    }
    Rectangle(width, _) {
        print("Rectangle with width: " + width.to_string())  // height ignored
    }
    Square(size) {
        print("Square with size: " + size.to_string())
    }
}
```

### Wildcard Pattern

The `_` pattern matches anything:

```mux
match status {
    0 { print("success") }
    1 { print("warning") }
    2 { print("error") }
    _ { print("unknown status") }  // catch-all
}
```

## Return Statement

Exit a function early:

```mux
func findFirst(list<int> items, int target) returns Optional<int> {
    for i in range(0, items.size()) {
        if items[i] == target {
            return Some(i)  // early return
        }
    }
    return None
}

func validate(int value) returns Result<int, string> {
    if value < 0 {
        return Err("value must be positive")
    }
    if value > 100 {
        return Err("value too large")
    }
    return Ok(value)
}
```

## Combining Control Flow

```mux
func process(list<int> items) returns void {
    for item in items {
        // Skip negatives
        if item < 0 {
            continue
        }
        
        // Stop at large values
        if item > 100 {
            break
        }
        
        // Process based on value
        auto message = if item % 2 == 0 {
            "even: " + item.to_string()
        } else {
            "odd: " + item.to_string()
        }
        
        print(message)
    }
}
```

## Best Practices

1. **Use `match` for Result and Optional** - More expressive than if-else chains
2. **Prefer pattern matching with guards** - Cleaner than nested if statements
3. **Use `_` for unused values** - Makes intent explicit
4. **Early returns for error conditions** - Reduces nesting
5. **Use `range()` for numeric loops** - Idiomatic Mux
6. **Break and continue judiciously** - Can make code harder to follow if overused

## See Also

- [Error Handling](./error-handling.md) - Using Result and Optional with match
- [Enums](./enums.md) - Pattern matching with tagged unions
- [Functions](./functions.md) - Return statements and early exits
- [Variables](./variables.md) - Type inference with `auto`
