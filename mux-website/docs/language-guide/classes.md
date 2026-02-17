# Classes

Mux provides object-oriented programming through classes and interfaces (traits).

## Basic Class Definition

```mux title="basic_class.mux"
class Circle {
    float radius  // explicit type required for fields
    
    func area() returns float {
        const float PI = 3.1415
        return PI * self.radius * self.radius
    }
    
    func circumference() returns float {
        const float PI = 3.1415
        return 2.0 * PI * self.radius
    }
}
```

**Key Points:**
- Fields must have explicit types (no `auto` inference)
- Methods use `self` to access instance fields
- Methods follow same rules as regular functions

## Class Instantiation

Classes use the `.new()` method pattern:

```mux title="class_instantiation.mux"
// Basic instantiation
auto circle = Circle.new()

// With constructor arguments (if constructor is defined)
auto circle2 = Circle.new(5.0)

// Accessing fields and methods
circle.radius = 10.0
auto area = circle.area()
print("Area: " + area.to_string())
```

**Design Note:** Mux uses explicit `.new()` rather than direct constructor calls to distinguish class instantiation from function calls and enum variant construction. The `.new()` method will *always* instantiate a new object with all default "zero" values for fields, and then you can set fields afterward. This is a simple and consistent pattern for object creation.

The "Mux" style for a constructors is to use a `common` (see [below](./classes.md#static-methods-with-common)) method that creates and initializes the object, rather than defining a special constructor syntax. This allows for more flexible object creation patterns while keeping the core class definition simple.

## Interfaces (Traits)

Interfaces define required methods that classes must implement:

```mux title="interfaces.mux"
interface Drawable {
    func draw() returns void
}

interface Measurable {
    func area() returns float
    func perimeter() returns float
}
```

## Implementing Interfaces

Use the `is` keyword to implement interfaces:

```mux title="implementing_interfaces.mux"
class Circle is Drawable, Measurable {
    float radius
    
    func draw() returns void {
        auto message = "Circle radius=" + self.radius.to_string()
        print(message)
    }
    
    func area() returns float {
        const float PI = 3.1415
        return PI * self.radius * self.radius
    }
    
    func perimeter() returns float {
        const float PI = 3.1415
        return 2.0 * PI * self.radius
    }
}

class Rectangle is Drawable, Measurable {
    float width
    float height
    
    func draw() returns void {
        print("Rectangle " + self.width.to_string() + "x" + self.height.to_string())
    }
    
    func area() returns float {
        return self.width * self.height
    }
    
    func perimeter() returns float {
        return 2.0 * (self.width + self.height)
    }
}
```

**Note:** Use `is` instead of `implements` (like Java). Multiple interfaces separated by commas.

## Methods

### Instance Methods

Access instance data via `self`:

```mux title="instance_methods.mux"
class Counter {
    int value
    
    func increment() returns void {
        self.value = self.value + 1
    }
    
    func reset() returns void {
        self.value = 0
    }
    
    func get() returns int {
        return self.value
    }
}

auto counter = Counter.new()
counter.increment()
counter.increment()
print(counter.get().to_string())  // "2"
```

### Methods with Unused Parameters

```mux title="class_unused_params.mux"
class Config {
    string name
    
    func update(string newName, string _) returns void {
        self.name = newName  // second parameter ignored
    }
}
```

## Static Methods with `common`

The `common` keyword declares static (class-level) methods:

```mux title="static_methods.mux"
class Stack<T> {
    list<T> items
    
    // Instance method - operates on self
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    // Static method - no self, called on class
    common func who_am_i() returns string {
        return "I'm a Stack!"
    }
    
    // Factory pattern - creates instances
    common func from(list<T> init_list) returns Stack<T> {
        auto new_stack = Stack<T>.new()
        new_stack.items = init_list
        return new_stack
    }
}

// Calling static methods
print(Stack.who_am_i())                    // "I'm a Stack!"
auto s = Stack<int>.from([1, 2, 3])        // Factory method

// Calling instance methods
auto stack = Stack<int>.new()              // creates a new, empty `items` in the stack
stack.push(42)
```

### `common` vs `const`

| Keyword | Purpose | Usage |
|---------|---------|-------|
| `common` | Static methods and factory functions | `ClassName.method()` |
| `const` | Immutable constants | `const int MAX = 100` |

**Key Differences:**
- **Instance methods** (no keyword) operate on `self` and require an instance
- **Static methods** (`common`) have no `self` and are called on the class
- **Const fields** are immutable instance/class fields, not methods
- Static methods cannot access instance fields (no `self` context)

## Constants in Classes

Classes can have constant (immutable) fields:

```mux title="class_constants.mux"
class Config {
    const int MAX_RETRIES
    int current_retry
    
    func increment() returns void {
        self.current_retry++  // OK - mutable field
        // self.MAX_RETRIES++  // ERROR: Cannot modify const field
    }
}

auto cfg = Config.new()
cfg.current_retry = 1  // OK - mutable field
// cfg.MAX_RETRIES = 5  // ERROR: Cannot assign to const field
```

**Const Enforcement:**
- Cannot reassign: `self.MAX_RETRIES = value` -> ERROR
- Cannot increment/decrement: `self.MAX_RETRIES++` -> ERROR
- Use `const` for fields that shouldn't change after initialization

## Generic Classes

Classes can be generic over type parameters:

```mux title="class_generics.mux"
class Pair<T, U> {
    T first
    U second
    
    func swap() returns Pair<U, T> {
        return Pair<U, T>.new(self.second, self.first)
    }
    
    common func from(T a, U b) returns Pair<T, U> {
        auto pair = Pair<T, U>.new()
        pair.first = a
        pair.second = b
        return pair
    }
}

// Usage
auto pair = Pair<int, string>.new()
pair.first = 42
pair.second = "answer"

auto reversed = pair.swap()  // Pair<string, int>

// Using factory method
auto pair2 = Pair<string, int>.from("key", 100)
```

See [Generics](./generics.md) for more details.

## Interface Dispatch (Static)

Mux uses **static dispatch** for interfaces - no runtime vtable lookup:

```mux title="static_dispatch.mux"
func drawAll(list<Drawable> shapes) returns void {
    for Shape shape in shapes {
        shape.draw()  // Resolved at compile time
    }
}
```

**Why Static Dispatch?**
- **Zero cost**: No pointer indirection, direct function calls
- **Inlining**: LLVM can inline interface methods
- **Optimization**: Better branch prediction, no indirect jumps

The tradeoff: interfaces cannot be added to types from other modules (no "extension traits").

## Best Practices

1. **Fields must be explicitly typed** - No `auto` for class fields
2. **Use interfaces for polymorphism** - Define common behavior
3. **Use `common` for factory methods** - Create instances with pre-populated data
4. **Keep classes focused** - Single responsibility principle
5. **Use `const` for immutable fields** - Prevent accidental modification
6. **Leverage generic classes** - Reusable data structures
7. **Prefer static dispatch** - Better performance than dynamic dispatch

## See Also

- [Generics](./generics.md) - Generic classes and type parameters
- [Interfaces](./generics.md#built-in-interfaces) - Built-in interfaces for common operations
- [Memory](./memory.md) - Reference counting and object lifecycle
- [Functions](./functions.md) - Method definitions
