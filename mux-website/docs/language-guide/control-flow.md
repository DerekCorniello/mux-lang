# Control Flow

Mux provides familiar control flow constructs with some unique features like pattern matching with guards.

## If / Else

Standard if-else branching with curly braces:

```mux title="if_else.mux"
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

```mux title="if_expression.mux"
auto message = if x > 0 { "positive" } else { "non-positive" }

auto status = if score >= 90 {
    "excellent"
} else if score >= 70 {
    "good"
} else {
    "needs improvement"
}
```

## For Loops

Mux uses range-based for loops only (no C-style `for (int i = 0; i < n; i++)`):

```mux title="for_loops.mux"
// Iterating over a list
auto items = [1, 2, 3, 4, 5]
for int item in items {
    print(item.to_string())
}

// Using range() for numeric iteration
for int i in range(0, 10) {
    print("Iteration: " + i.to_string())
}

// Explicit typing for clarity
auto collection = ["Bob", "Alice", "Eve"]
for string name in collection {
    auto processed = create_message_to(name)
    print(processed)
}
```

### Ignoring Loop Variables

Use `_` when you don't need the loop variable:

```mux title="ignoring_loop_vars.mux"
// Execute 10 times, don't care about index
for int _ in range(0, 10) {
    doSomething()
}
```

### Iterating Collections

```mux title="iterating_collections.mux"
// List iteration
auto nums = [10, 20, 30]
for int n in nums {
    print(n.to_string())
}

auto myMap = {
    "a": 1,
    "b": 2,
    "c": 3
}
// Map iteration on keys
for key in myMap.keys() {
    print(key)
}

// Map iteration on vals
for value in myMap.keys() {
    print(value)
}

auto mySet = {"a", "b", "c"}
for char in mySet {
    print(char.to_string())
}
```

## While Loops

Standard while loops with boolean conditions:

```mux title="while_loops.mux"
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

```mux title="break_continue.mux"
// Break exits the loop
for int i in range(0, 100) {
    if i > 10 {
        break
    }
    print(i.to_string())
}

// Continue skips to next iteration
for int i in range(0, 10) {
    if i % 2 == 0 {
        continue  // skip even numbers
    }
    print(i.to_string())
}
```

## Match Statements

Pattern matching with guards and destructuring:

### Basic Matching

```mux title="basic_matching.mux"
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

This is equivalent to:

```mux title="if_else_equivalent.mux"
if value == 1 {
    print("one")
} else if value == 2 {
    print("two")
} else {
    print("other")
}
```

### Matching Optional

```mux title="matching_optional.mux"
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

```mux title="matching_result.mux"
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

```mux title="pattern_guards.mux"
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

```mux title="pattern_ignore.mux"
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

```mux title="matching_enums.mux"
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

```mux title="wildcard_pattern.mux"
match status {
    0 { print("success") }
    1 { print("warning") }
    2 { print("error") }
    _ { print("unknown status") }  // catch-all
}
```

### Exhaustive Matching

Mux requires that all possible cases are handled in a match statement. If you miss a case, the compiler will give an error:

```mux title="exhaustive_matching.mux"
match value {
    1 { print("one") }
    2 { print("two") }
    // Missing wildcard or other cases will cause a compile error!
}
```

### Match as Switch Statement

Match statements can be used as switch statements for any type, not just enums:

```mux title="switch_on_primitives.mux"
// Match on int literals (like a switch)
auto status = 200
match status {
    200 { print("OK") }
    404 { print("Not Found") }
    500 { print("Server Error") }
    _ { print("Unknown status") }
}

// Match on string literals
auto command = "start"
match command {
    "start" { print("Starting...") }
    "stop" { print("Stopping...") }
    "restart" { print("Restarting...") }
    _ { print("Unknown command") }
}

// Match on bool values
auto flag = true
match flag {
    true { print("Enabled") }
    false { print("Disabled") }
}

// Match on char literals
auto letter = 'a'
match letter {
    'a' { print("Alpha") }
    'b' { print("Bravo") }
    _ { print("Other") }
}
```

#### Variable Binding in Switch Patterns

Capture matched values in variables:

```mux title="switch_variable_binding.mux"
auto value = 42
match value {
    1 { print("one") }
    captured { print("got: " + captured.to_string()) }
    _ { print("shouldn't reach here") }
}

// With strings
auto name = "world"
match name {
    "admin" { print("Hello, admin!") }
    n { print("Hello, " + n) }
}
```

#### Constants in Switch Patterns

Use constants as match patterns:

```mux title="switch_constants.mux"
const int ADMIN = 1
const int MODERATOR = 2
auto level = 1
match level {
    ADMIN { print("admin") }
    MODERATOR { print("moderator") }
    _ { print("user") }
}
```

#### List Literal Matching

Match on list structure:

```mux title="switch_list_matching.mux"
auto nums = [1, 2, 3]
match nums {
    [] { print("empty") }
    [1] { print("single: 1") }
    [1, 2] { print("two: 1, 2") }
    [1, 2, 3] { print("three: 1, 2, 3") }
    [first, ..rest] { print("has elements") }
    _ { print("weird list") }
}
```

#### Switch with Guards

Add conditions to switch cases:

```mux title="switch_with_guards.mux"
auto score = 85
match score {
    n if n >= 90 { print("A") }
    n if n >= 80 { print("B") }
    n if n >= 70 { print("C") }
    n if n >= 60 { print("D") }
    _ { print("F") }
}
```

## Return Statement

Exit a function early:

```mux title="return_statement.mux"
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
