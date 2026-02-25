# Expressions

This document describes expression evaluation rules in Mux.

## Expression Classification

Expressions are categorized by their evaluation behavior:

| Category | Description | Examples |
|----------|-------------|----------|
| **Value expressions** | Produce a value | Literals, variables, operators |
| **Place expressions** | Denote storage locations | Variables, array elements, fields |
| **Void expressions** | Produce no value | some function calls |

## Primary Expressions

### Literals

```mux
42              // Integer literal (type: int, value: 42)
3.14159         // Float literal (type: float, value: 3.14159)
"hello"         // String literal (type: string, value: "hello")
'a'             // Char literal (type: char, value: 'a')
true            // Boolean literal (type: bool, value: true)
false           // Boolean literal (type: bool, value: false)
none            // none literal (type: optional<T>.none)
```

### Identifiers

```mux
auto x = 42     // x is an identifier expression
print(x)        // x evaluates to its current value
x = 100         // x is a place expression (assignment target)
```

### Parenthesized Expressions

```mux
auto x = (1 + 2) * 3    // x = 9
auto y = (result)       // y has same type as result
```

## Tuple Literals

Tuples are fixed size pairs with exactly two elements:

```mux
auto pair = (1, "one")
auto nested = ((1, 2), ("a", "b"))
```

Access tuple fields with `.left` and `.right`:

```mux
print(pair.left.to_string())
print(pair.right.to_string())
```

## List Literals

```mux
auto nums = [1, 2, 3]           // list<int>
auto empty = []                 // ERROR: needs explicit type
auto typed = list<int>.new()    // Empty list with type
auto mixed = [1, "two", 3.0]    // Mixed types allowed
```

### Type Inference for Lists

The element type is inferred from the contents:

```mux
auto nums = [1, 2, 3]           // list<int>
auto strs = ["a", "b"]          // list<string>
auto floats = [1.0, 2.0]        // list<float>
auto objects = [Circle.new(1)]  // list<Circle>
```

Empty lists require explicit type annotation:

```mux
list<int> empty = []            // Valid: explicit type
auto empty = list<int>.new()    // Valid: explicit constructor
auto bad = []                   // ERROR: cannot infer type
```

## Map Literals

```mux
auto scores = {"Alice": 90, "Bob": 85}
auto nested = {"name": "Mux", "version": 1}
auto complex = {"key": [1, 2, 3]}
```

### Type Inference

```mux
auto scores = {"Alice": 90}      // map<string, int>
auto mixed = {"a": 1, "b": "x"}  // map<string, Value>
```

## Set Literals

```mux
auto tags = {"urgent", "important", "review"}
```

### Type Inference

```mux
auto nums = {1, 2, 3}           // set<int>
auto strs = {"a", "b", "c"}     // set<string>
```

## Lambda Expressions

```mux
auto square = func(int n) returns int {
    return n * n
}

auto add = func(int a, int b) {
    return a + b
}

// Type: func(int, int) -> int
```

### Lambda Capture

Lambdas capture variables from enclosing scope:

```mux
auto multiplier = 10
auto times10 = func(int x) returns int {
    return x * multiplier  // Captures multiplier
}
```

## Enum Instantiation

```mux
enum Option<T> {
    some(T)
    none
}

auto opt = Option<int>.some(42)
auto none = Option<int>.none
auto inferred = some(42)  // Type inferred from context
```

## Class Instantiation

```mux
class Circle {
    float radius

    func new(float r) returns void {
        self.radius = r
    }
}

auto c = Circle.new(5.0)
```

### Generic Classes

```mux
class Stack<T> {
    list<T> items
}

auto int_stack = Stack<int>.new()
auto str_stack = Stack<string>.new()
```

## Member Access

```mux
auto r = circle.radius           // Field access
auto len = str.size()            // Method call
auto elem = list[0]              // Array access
```

### Field Access

```mux
class Point {
    int x
    int y
}

auto p = Point.new()
auto x_coord = p.x  // Read field
p.x = 10            // Write to field
```

### Method Call

```mux
auto result = numbers.size()     // Method call
auto upper = "hello".to_upper()  // Method on literal
```

## Array/List Access

```mux
auto first = myList[0]           // Read element
myList[0] = 42                   // Write element

// Out of bounds is runtime error
// auto bad = myList[1000]        // Runtime error
```

### Safe Access with `.get()`

```mux
auto first = myList.get(0)       // optional<int>
match myList.get(0) {
    some(value) { print(value) }
    none { print("out of bounds") }
}
```

## Range Expressions

```mux
auto nums = range(0, 10)  // list<int>: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

## Conditional Expressions

Mux does not have a ternary `? :` operator. Use `if`/`else` as a statement:

```mux
// Not valid Mux:
// auto x = condition ? 1 : 2

// Instead:
auto x = if condition {
    1
} else {
    2
}
```

## Expression Statements

Expressions can be used as statements when side effects are desired:

```mux
some_function()           // Function call
x++                       // Increment
x += 5                    // Compound assignment
print("message")          // Print for side effect
```

## Type of Expressions

The compiler determines the type of each expression:

| Expression | Type |
|------------|------|
| Integer literal | `int` |
| Float literal | `float` |
| String literal | `string` |
| Boolean literal | `bool` |
| Tuple literal | `tuple<T, U>` |
| List literal | `list<T>` (inferred) |
| Identifier | Declared or inferred type |
| `a + b` | Type of `a` and `b` (must match) |
| `a == b` | `bool` |
| `a && b` | `bool` |
| `!a` | `bool` |
| `range(a, b)` | `list<int>` |
| Lambda | `func(...) -> ...` |

## See Also

- [Operators](./operators.md) - Operator precedence
