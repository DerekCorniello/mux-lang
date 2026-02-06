# Enums (Tagged Unions)

Enums in Mux are tagged unions (also called algebraic data types or sum types) that can hold different variants with associated data.

## Basic Enum Definition

```mux
enum Status {
    Pending
    Active
    Completed
}

enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}
```

**Key Points:**
- Each variant is a case of the enum
- Variants can carry associated data (or none)
- Variants are constructed using `.new()` syntax

## Creating Enum Instances

```mux
// Variant without data
auto status = Pending.new()

// Variants with data
auto circle = Circle.new(5.0)
auto rect = Rectangle.new(10.0, 20.0)
auto square = Square.new(7.5)

// Type inference
auto myShape = Circle.new(5.0)  // type inferred as Shape
```

## Pattern Matching with Enums

Use `match` to handle different enum variants:

```mux
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

auto myShape = Circle.new(5.0)

match myShape {
    Circle(r) {
        auto area = 3.1415 * r * r
        print("Circle area: " + area.to_string())
    }
    Rectangle(w, h) {
        auto area = w * h
        print("Rectangle area: " + area.to_string())
    }
    Square(s) {
        auto area = s * s
        print("Square area: " + area.to_string())
    }
}
```

### Ignoring Associated Data

Use `_` to ignore data you don't need:

```mux
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

## Pattern Matching with Guards

Add conditional logic with guards:

```mux
enum MaybeValue<T> {
    Some(T)
    None
}

auto value = Some.new(42)

match value {
    Some(v) if v > 10 {
        print("Large value: " + v.to_string())
    }
    Some(v) if v > 0 {
        print("Small positive: " + v.to_string())
    }
    Some(v) {
        print("Non-positive: " + v.to_string())
    }
    None {
        print("No value")
    }
}
```

## Generic Enums

Enums can be generic over type parameters:

```mux
enum Option<T> {
    Some(T)
    None
}

enum Result<T, E> {
    Ok(T)
    Err(E)
}

// Usage
auto maybeInt = Some.new(42)         // Option<int>
auto maybeStr = Some.new("hello")    // Option<string>
auto nothing = None.new()             // Option<T> (generic)

auto success = Ok.new(100)            // Result<int, E>
auto failure = Err.new("error msg")   // Result<T, string>
```

## Common Enum Patterns

### Optional Values

```mux
enum Optional<T> {
    Some(T)
    None
}

func findFirst(list<int> items, int target) returns Optional<int> {
    for i in range(0, items.size()) {
        if items[i] == target {
            return Some.new(i)
        }
    }
    return None.new()
}

auto result = findFirst([10, 20, 30], 20)

match result {
    Some(index) {
        print("Found at index: " + index.to_string())
    }
    None {
        print("Not found")
    }
}
```

### Result Types for Error Handling

```mux
enum Result<T, E> {
    Ok(T)
    Err(E)
}

func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err.new("division by zero")
    }
    return Ok.new(a / b)
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

See [Error Handling](./error-handling.md) for more details on Result and Optional.

### State Machines

```mux
enum Connection {
    Disconnected
    Connecting(string address)
    Connected(string address, int port)
    Failed(string error)
}

func handleConnection(Connection conn) returns void {
    match conn {
        Disconnected {
            print("Not connected")
        }
        Connecting(addr) {
            print("Connecting to: " + addr)
        }
        Connected(addr, port) {
            print("Connected to " + addr + ":" + port.to_string())
        }
        Failed(err) {
            print("Connection failed: " + err)
        }
    }
}
```

### Nested Enums

```mux
enum Message {
    Text(string content)
    Image(string url, int width, int height)
    Reply(string content, int replyToId)
}

enum Event {
    MessageReceived(Message msg)
    UserJoined(string username)
    UserLeft(string username)
}

auto event = MessageReceived.new(Text.new("Hello!"))

match event {
    MessageReceived(msg) {
        match msg {
            Text(content) {
                print("Text: " + content)
            }
            Image(url, w, h) {
                print("Image: " + url)
            }
            Reply(content, id) {
                print("Reply to " + id.to_string() + ": " + content)
            }
        }
    }
    UserJoined(name) {
        print(name + " joined")
    }
    UserLeft(name) {
        print(name + " left")
    }
}
```

## Enums in Collections

```mux
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

// List of shapes
list<Shape> shapes = [
    Circle.new(1.0),
    Rectangle.new(2.0, 3.0),
    Square.new(4.0)
]

for shape in shapes {
    match shape {
        Circle(r) {
            print("Circle: " + r.to_string())
        }
        Rectangle(w, h) {
            print("Rectangle: " + w.to_string() + "x" + h.to_string())
        }
        Square(s) {
            print("Square: " + s.to_string())
        }
    }
}
```

## Exhaustiveness Checking

Mux enforces exhaustive pattern matching - all variants must be covered:

```mux
enum Color {
    Red
    Green
    Blue
}

// ERROR: Non-exhaustive match (missing Blue)
match color {
    Red { print("red") }
    Green { print("green") }
    // Blue case missing!
}

// Correct: All variants covered
match color {
    Red { print("red") }
    Green { print("green") }
    Blue { print("blue") }
}

// Correct: Using wildcard for catch-all
match color {
    Red { print("red") }
    _ { print("other color") }  // Covers Green and Blue
}
```

## Comparison with Other Languages

### vs Rust

Similar to Rust enums:

```rust
// Rust
enum Option<T> {
    Some(T),
    None,
}

// Mux
enum Optional<T> {
    Some(T)
    None
}
```

Differences:
- Mux uses `.new()` for construction
- No semicolons or commas in Mux
- Mux uses `match` with curly braces

### vs TypeScript/JavaScript

TypeScript's discriminated unions:

```typescript
// TypeScript
type Shape =
  | { kind: 'circle'; radius: number }
  | { kind: 'rectangle'; width: number; height: number }

// Mux
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
}
```

Mux advantages:
- Compile-time exhaustiveness checking
- No manual discriminant field
- Type-safe variant construction

## Best Practices

1. **Use enums for mutually exclusive states** - Better than multiple booleans
2. **Match exhaustively** - Don't overuse wildcard patterns
3. **Use guards for additional logic** - Cleaner than nested if statements
4. **Ignore unused data with `_`** - Makes intent explicit
5. **Prefer Result over exceptions** - Explicit error handling
6. **Prefer Optional over null** - No null pointer errors
7. **Use generic enums for reusable patterns** - Option&lt;T&gt;, Result&lt;T,E&gt;

## See Also

- [Error Handling](./error-handling.md) - Result and Optional types
- [Control Flow](./control-flow.md) - Pattern matching with match
- [Generics](./generics.md) - Generic enums
- [Collections](./collections.md) - Enums in lists and maps
