# Examples

This page provides quick reference examples for common Mux patterns.

## Hello World

```mux title="hello.mux"
func main() returns void {
    print("Hello, World!")
}
```

## Variables and Types

```mux title="variables.mux"
func main() returns void {
    // Explicit typing
    int age = 25
    float pi = 3.14159
    bool active = true
    string name = "Mux"

    // Type inference with auto
    auto count = 42
    auto price = 19.99
    auto greeting = "Hello"

    print("Age: " + age.to_string())
}
```

## Functions

```mux title="functions.mux"
func greet(string name, int times = 1) returns void {
    for int i in range(0, times) {
        print("Hello, " + name + "!")
    }
}

func add(int a, int b) returns int {
    return a + b
}

func main() returns void {
    greet("World", 3)
    auto sum = add(5, 10)
    print("Sum: " + sum.to_string())
}
```

## Control Flow

```mux title="control-flow.mux"
func main() returns void {
    auto num = 15

    // If-else
    if num > 0 {
        print("positive")
    } else if num < 0 {
        print("negative")
    } else {
        print("zero")
    }

    // Match expression
    match num {
        0 { print("zero") }
        n if n % 2 == 0 { print("even: " + n.to_string()) }
        _ { print("odd") }
    }

    // For loop
    for int i in range(1, 4) {
        print("Count: " + i.to_string())
    }

    // While loop
    auto n = 3
    while n > 0 {
        print("Countdown: " + n.to_string())
        n--
    }
}
```

## Collections

```mux title="collections.mux"
func main() returns void {
    // Lists
    auto numbers = [1, 2, 3, 4, 5]
    auto first = numbers[0]
    numbers.push_back(6)

    // Maps
    auto scores = {"Alice": 90, "Bob": 85}
    auto aliceScore = scores.get("Alice")

    // Sets
    auto unique = {1, 2, 3, 2, 1}  // {1, 2, 3}
}
```

## Enums and Pattern Matching

```mux title="enums.mux"
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
}

func area(Shape s) returns float {
    match s {
        Circle(r) { return 3.14159 * r * r }
        Rectangle(w, h) { return w * h }
    }
}

func main() returns void {
    auto c = Shape.Circle(5.0)
    print("Area: " + area(c).to_string())
}
```

## Classes

```mux title="classes.mux"
class Counter {
    int value

    func new(int start) returns void {
        self.value = start
    }

    func increment() returns void {
        self.value++
    }

    func get_value() returns int {
        return self.value
    }
}

func main() returns void {
    auto c = Counter.new(0)
    c.increment()
    c.increment()
    print("Count: " + c.get_value().to_string())
}
```

## Generics

```mux title="generics.mux"
func identity<T>(T value) returns T {
    return value
}

class Box<T> {
    T item

    func new(T item) returns void {
        self.item = item
    }

    func get() returns T {
        return self.item
    }
}

func main() returns void {
    auto intBox = Box<int>.new(42)
    auto strBox = Box<string>.new("Mux")

    print("Int: " + intBox.get().to_string())
    print("String: " + strBox.get())
}
```

## Error Handling

```mux title="error-handling.mux"
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

func find_item(list<int> items, int target) returns Optional<int> {
    for item in items {
        if item == target {
            return Some(item)
        }
    }
    return None
}

func main() returns void {
    match divide(10, 2) {
        Ok(result) { print("Result: " + result.to_string()) }
        Err(msg) { print("Error: " + msg) }
    }

    match find_item([1, 2, 3, 4], 3) {
        Some(val) { print("Found: " + val.to_string()) }
        None { print("Not found") }
    }
}
```

## References

```mux title="references.mux"
func increment(&int ref) returns void {
    *ref = *ref + 1
}

func main() returns void {
    auto num = 10
    increment(&num)
    print("Incremented: " + num.to_string())
}
```

## Modules

```mux title="math.mux"
const float PI = 3.14159

func circle_area(float r) returns float {
    return PI * r * r
}
```

```mux title="main.mux"
import math

func main() returns void {
    auto area = math.circle_area(5.0)
    print("Circle area: " + area.to_string())
}
```

## Complete Program

```mux title="complete.mux"
import std

enum Result<T, E> {
    Ok(T)
    Err(E)
}

class Stack<T> {
    list<T> items

    func new() returns void {
        self.items = []
    }

    func push(T item) returns void {
        self.items.push_back(item)
    }

    func pop() returns Optional<T> {
        return self.items.pop_back()
    }

    func size() returns int {
        return self.items.size()
    }
}

func main() returns void {
    auto stack = Stack<int>.new()
    stack.push(1)
    stack.push(2)
    stack.push(3)

    print("Stack size: " + stack.size().to_string())

    match stack.pop() {
        Some(val) { print("Popped: " + val.to_string()) }
        None { print("Empty") }
    }
}
```

## See Also

- [Quick Start Guide](../getting-started/quick-start.md)
- [Language Guide Index](./overview.md) - All language guide topics
- [Reference](../reference/overview.md) - Formal language specification
