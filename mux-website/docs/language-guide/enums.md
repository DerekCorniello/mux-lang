# Enums / Tagged Unions

Enums in Mux are tagged unions (also called algebraic data types or sum types) that can hold different variants with associated data.

## Basic Enum Definition

```mux title="basic_enum.mux"
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

```mux title="creating_enums.mux"
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

```mux title="pattern_matching_enums.mux"
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

```mux title="ignoring_enum_data.mux"
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

```mux title="enum_guards.mux"
enum MaybeValue<T> {
    some(T)
    none
}

auto value = some.new(42)

match value {
    some(v) if v > 10 {
        print("Large value: " + v.to_string())
    }
    some(v) if v > 0 {
        print("Small positive: " + v.to_string())
    }
    some(v) {
        print("Non-positive: " + v.to_string())
    }
    none {
        print("No value")
    }
}
```

## Generic Enums

Enums can be generic over type parameters:

```mux title="enum_generics.mux"
enum Option<T> {
    some(T)
    none
}

enum result<T, E> {
    ok(T)
    err(E)
}

// Usage
auto maybeInt = some.new(42)         // Option<int>
auto maybeStr = some.new("hello")    // Option<string>
auto nothing = none.new()             // Option<T> (generic)

auto success = ok.new(100)            // result<int, E>
auto failure = err.new("error msg")   // result<T, string>
```

## Common Enum Patterns

### optional Values

```mux title="optional_values.mux"
enum optional<T> {
    some(T)
    none
}

func findFirst(list<int> items, int target) returns optional<int> {
    for i in range(0, items.size()) {
        if items[i] == target {
            return some.new(i)
        }
    }
    return none.new()
}

auto result = findFirst([10, 20, 30], 20)

match result {
    some(index) {
        print("Found at index: " + index.to_string())
    }
    none {
        print("Not found")
    }
}
```

### result Types for Error Handling

```mux title="result_types.mux"
enum result<T, E> {
    ok(T)
    err(E)
}

func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err.new("division by zero")
    }
    return ok.new(a / b)
}

auto result = divide(10, 2)

match result {
    ok(value) {
        print("result: " + value.to_string())
    }
    err(error) {
        print("Error: " + error)
    }
}
```

See [Error Handling](./error-handling.md) for more details on result and optional.

### State Machines

```mux title="state_machines.mux"
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

```mux title="nested_enums.mux"
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

```mux title="enums_in_collections.mux"
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

for Shape shape in shapes {
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

```mux title="exhaustiveness_checking.mux"
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

## Best Practices

1. **Use enums for mutually exclusive states** - Better than multiple booleans
2. **Match exhaustively** - Don't overuse wildcard patterns
3. **Use guards for additional logic** - Cleaner than nested if statements
4. **Ignore unused data with `_`** - Makes intent explicit
5. **Prefer result over exceptions** - Explicit error handling
6. **Prefer optional over null** - No null pointer errors
7. **Use generic enums for reusable patterns** - Option&lt;T&gt;, result&lt;T, E&gt;

## See Also

- [Error Handling](./error-handling.md) - result and optional types
- [Control Flow](./control-flow.md) - Pattern matching with match
- [Generics](./generics.md) - Generic enums
- [Collections](./collections.md) - Enums in lists and maps
