# Statements

This document describes the semantics of each statement type in Mux.

## Mutability

Variables are mutable by default:

```mux
auto count = 0
count = count + 1   // OK: reassignment
count++              // OK: increment
```

To make a variable immutable, use `const`:

```mux
const int MAX = 100
// MAX = 200  // ERROR: cannot reassign const
// MAX++      // ERROR: cannot modify const
```

## Constant Declaration Statements

### Rules

1. Constants must be initialized at declaration
2. Constants cannot be reassigned
3. Constants cannot be used with increment/decrement operators
4. Constants can be declared at:
   - Top-level (module scope)
   - Inside classes (class fields)
   - Inside functions (local constants)

```mux
// Module-level constant
const int MAX_RETRIES = 3

// Class constant
class Config {
    const int MAX_SIZE = 1024
}

// Local constant
func process() returns void {
    const float TAX_RATE = 0.08
    auto total = amount * TAX_RATE
}
```

## If Statements

### Semantics

1. The condition expression must evaluate to `bool`
2. Only the matched branch is executed
3. Each branch creates a new scope

```mux
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}
```

### Scope Rules

Variables declared in one branch are not accessible in other branches:

```mux
if condition {
    auto x = 1
    // x is accessible here
}
// x is NOT accessible here
```

## Match Statements

### Semantics

1. The match expression is evaluated
2. Each arm is checked in order
3. The first matching arm (with optional guard) is executed
4. Exhaustiveness is checked at compile time

### Patterns

| Pattern | Meaning | Example |
|---------|---------|---------|
| `_` | Wildcard (matches anything) | `_ { print("other") }` |
| `Variant(value)` | Destructuring enum | `some(v) { print(v) }` |
| `Identifier` | Bind to variable | `Point(x, y) { ... }` |
| `value` | Literal match | `0 { print("zero") }` |

### Examples

```mux
enum Option<T> {
    some(T)
    none
}

match maybeValue {
    some(v) if v > 10 {
        print("large: " + v)
    }
    some(v) {
        print("small: " + v)
    }
    none {
        print("no value")
    }
    _ {
        print("unexpected")
    }
}
```

### Exhaustiveness Checking

The compiler requires all cases to be covered:

```mux
enum Color { Red, Green, Blue }

auto c = Color.Red

match c {
    Red { print("red") }
    Green { print("green") }
    // ERROR: Blue not covered
}
```

### Guard Conditions

Arms can have additional `if` conditions:

```mux
match number {
    n if n < 0 { print("negative") }
    n if n == 0 { print("zero") }
    n { print("positive: " + n) }
}
```

## For Statements

### Semantics

1. The iterable expression is evaluated
2. Each element is bound to the pattern
3. The body is executed for each element

### Iteration Patterns

```mux
// Iterate over list
for int item in myList {
    print(item)
}

// Ignore elements
for int _ in range(0, 10) {
    doSomething()
}

// With explicit typing
auto numbers = [1, 2, 3]
for int n in numbers {
    // n is explicitly typed as int
}
```

### Range Iteration

```mux
// range(start, end) - exclusive end
for int i in range(0, 5) {
    print(i)  // 0, 1, 2, 3, 4
}

// Negative step not supported
// Use while loop for complex iteration
```

## While Statements

### Semantics

1. The condition is evaluated
2. If true, the body is executed and the condition is checked again
3. If false, execution continues after the loop

```mux
auto count = 5
while count > 0 {
    print(count)
    count = count - 1
}
// count is 0 here
```

### Condition Requirements

The condition must evaluate to `bool`:

```mux
auto x = 1
while x {  // ERROR: cannot convert int to bool
    x--
}
```

## Break Statements

### Semantics

1. Exits the immediately enclosing loop
2. Execution continues after the loop

```mux
for int i in range(0, 100) {
    if i == 10 {
        break  // Exit loop when i reaches 10
    }
    print(i)
}
// Execution continues here
```

## Continue Statements

### Semantics

1. Skips to the next iteration of the immediately enclosing loop
2. The loop condition is checked again

```mux
for int i in range(0, 10) {
    if i % 2 == 0 {
        continue  // Skip even numbers
    }
    print(i)  // Prints odd numbers: 1, 3, 5, 7, 9
}
```

## Return Statements

### Semantics

1. Returns control from the current function
2. If a value is provided, it becomes the function's result
3. If no value is provided, the function must have return type `void`

### Value Return

```mux
func add(int a, int b) returns int {
    return a + b
}

func max(int a, int b) returns int {
    if a > b {
        return a
    }
    return b
}
```

### Void Return

```mux
func greet(string name) returns void {
    print("Hello, " + name)
    return  // Required in void functions
}
```

### Early Returns

Returns can appear anywhere in a function:

```mux
func process(int value) returns int {
    if value < 0 {
        return 0  // Early return on invalid input
    }
    // Normal processing
    return value * 2
}
```

### Cleanup on Return

When a return executes:

1. All variables in current scope are cleaned up (RC decremented)
2. Nested scopes are cleaned up in reverse order
3. Control transfers to the caller

## See Also

- [Expressions](./expressions.md) - Expression evaluation
- [Control Flow](../language-guide/control-flow.md) - Control flow guide
