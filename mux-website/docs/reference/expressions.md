# Expressions

This document describes expression evaluation rules in Mux.

## Expression Classification

Expressions are categorized by their evaluation behavior:

| Category | Description | Examples |
|----------|-------------|----------|
| **Value expressions** | Produce a value | Literals, variables, operators |
| **Place expressions** | Denote storage locations | Variables, array elements, fields |
| **Void expressions** | Produce no value | Some function calls |

## Primary Expressions

### Literals

```mux
42              // Integer literal (type: int, value: 42)
3.14159         // Float literal (type: float, value: 3.14159)
"hello"         // String literal (type: string, value: "hello")
'a'             // Char literal (type: char, value: 'a')
true            // Boolean literal (type: bool, value: true)
false           // Boolean literal (type: bool, value: false)
None            // None literal (type: Optional<T>.None)
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

## List Literals

```mux
auto nums = [1, 2, 3]           // list<int>
auto empty = []                 // ERROR: needs explicit type
auto typed = list<int>()        // Empty list with type
auto mixed = [1, "two", 3.0]   // Mixed types allowed
```

### Type Inference for Lists

The element type is inferred from the contents:

```mux
auto nums = [1, 2, 3]           // list<int>
auto strs = ["a", "b"]          // list<string>
auto floats = [1.0, 2.0]        // list<float>
auto objects = [Circle.new(1)]   // list<Circle>
```

Empty lists require explicit type annotation:

```mux
list<int> empty = []            // Valid: explicit type
auto empty = list<int>()        // Valid: explicit constructor
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
auto scores = {"Alice": 90}     // map<string, int>
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
    Some(T)
    None
}

auto opt = Option<int>.Some(42)
auto none = Option<int>.None
auto inferred = Some(42)  // Type inferred from context
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
auto len = str.length()          // Method call
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
auto first = myList.get(0)       // Optional<int>
match myList.get(0) {
    Some(value) { print(value) }
    None { print("out of bounds") }
}
```

## Arithmetic Expressions

```mux
auto sum = a + b
auto diff = a - b
auto product = a * b
auto quotient = a / b
auto remainder = a % b
auto power = a ** b
```

### Evaluation Order

Left operand is always evaluated before right operand:

```mux
auto i = 0
auto x = i++ + i++   // x = 0, i = 2
```

## Logical Expressions

### Short-Circuit Evaluation

```mux
// AND: b only evaluated if a is true
auto result = a && b

// OR: b only evaluated if a is false
auto result = a || b
```

### Truthiness

Only `bool` values are allowed in logical expressions:

```mux
auto x = 1
// auto y = x && x  // ERROR: int cannot be used with &&
```

## Comparison Expressions

```mux
auto equal = a == b
auto not_equal = a != b
auto less = a < b
auto less_equal = a <= b
auto greater = a > b
auto greater_equal = a >= b
```

### String Comparison

String comparison is lexicographic (Unicode codepoint order):

```mux
auto a = "abc"
auto b = "abd"
auto result = a < b  // true
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
if condition {
    auto x = 1
} else {
    auto x = 2
}
```

## Expression Statements

Expressions can be used as statements when side effects are desired:

```mux
some_function()          // Function call
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
| List literal | `list<T>` (inferred) |
| Identifier | Declared or inferred type |
| `a + b` | Type of `a` and `b` (must match) |
| `a == b` | `bool` |
| `a && b` | `bool` |
| `!a` | `bool` |
| `range(a, b)` | `list<int>` |
| Lambda | `func(...) -> ...` |

## Constant Folding

The compiler performs constant folding on simple expressions:

```mux
auto x = 1 + 2 + 3    // Optimized to 6 at compile time
auto y = 2 ** 10      // Optimized to 1024
auto z = "hello" + "world"  // Optimized to "helloworld"
```

## Value Category

### L-values (Place Expressions)

Can appear on the left side of assignment:

```mux
x = 42           // x is an l-value
a[i] = 10        // array element is an l-value
obj.field = 5    // field is an l-value
*r = 10          // dereferenced pointer is an l-value
```

### R-values (Value Expressions)

Produce values but cannot be assigned to:

```mux
42              // Literal r-value
a + b           // Arithmetic r-value
f()             // Function call r-value (unless returns reference)
```

## Evaluation Order Guarantees

1. **Left-to-right evaluation** for most binary operators
2. **Short-circuit evaluation** for `&&` and `||`
3. **Function arguments** evaluated before the function body executes
4. **No unspecified evaluation order** within expressions

## Side Effects

An expression has a side effect if it modifies state:

- Assignment expressions
- Increment/decrement expressions
- Function calls that modify state
- Method calls that modify state

## See Also

- [Grammar](./grammar.md) - Expression syntax
- [Operators](./operators.md) - Operator precedence
- [Type System](./type-system.md) - Type rules
