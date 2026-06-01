---
id: examples
title: Code Examples
description: Curated collection of Mux code examples organized by category
---

# Mux Code Examples

A curated collection of Mux code examples organized by category. These examples demonstrate Mux's core features with runnable, commented code.

:::tip
All examples on this page demonstrate real Mux language features. Feel free to copy them to the playground and modify them!
:::

import CodeBlock from '@theme/CodeBlock';

## Getting Started

### Hello World

Every Mux program starts with a `main` function. This simple example demonstrates how to print text to the console.

```mux title="hello.mux"
func main() returns void {
    print("Hello, World!")
}
```

### Variables and Types

Mux has explicit typing with type inference. Variables can be declared with explicit types or using `auto` for inferred types. Constants are declared with `const`.

```mux title="variables.mux"
const int MAX = 100
int explicit = 42
auto inferred = 4.89
auto name = "Mux"

bool flag = true

print("MAX: " + MAX.to_string())
print("explicit: " + explicit.to_string())
print("inferred: " + inferred.to_string())
print("name: " + name)
print("flag: " + flag.to_string())
```

### Functions

Functions in Mux can have required and optional parameters. Default values are specified directly in the parameter list. Functions return values using the `return` keyword.

```mux title="functions.mux"
func add(int a, int b) returns int {
    return a + b
}

func greet(string name, int times = 1) returns void {
    for int i in range(0, times) {
        print("Hello " + name)
    }
}

func create_message(string greeting, string name = "friend", string punctuation = "!") returns string {
    return greeting + " " + name + punctuation
}

print("add(3, 4) = " + add(3, 4).to_string())
greet("World", 2)
print(create_message("Hey", "Charlie", "?"))
```

## Control Flow

### Conditionals

Mux supports `if/else` statements with optional `else if` chains. Boolean expressions work with comparison operators.

```mux title="conditionals.mux"
int x = 5
if x > 0 {
    print("x is positive")
} else if x < 0 {
    print("x is negative")
} else {
    print("x is zero")
}

if x > 3 && x < 10 {
    print("x is between 3 and 10")
}
```

### Loops

Mux supports `for` loops with `range()` and `while` loops. The `for` loop can iterate over a range with optional step.

```mux title="loops.mux"
for int i in range(0, 5) {
    print("i = " + i.to_string())
}

int count = 0
while count < 3 {
    print("count = " + count.to_string())
    count = count + 1
}
```

### Pattern Matching

The `match` expression is Mux's powerful pattern matching tool. It works with enums, optionals, and custom types.

```mux title="pattern_matching.mux"
func describe_optional(optional<int> opt) returns void {
    match opt {
        some(value) {
            print("Got value: " + value.to_string())
        }
        none {
            print("Got nothing")
        }
    }
}

describe_optional(some(42))
describe_optional(none)
```

## Collections

### Lists

Lists are ordered, growable collections. They support indexing, push/pop operations, and various utility methods.

```mux title="lists.mux"
list<int> nums = [10, 20, 30, 40, 50]

print("Initial list: " + nums.to_string())
print("First element: " + nums[0].to_string())

nums.push_back(60)
print("After push: " + nums.to_string())

auto last = nums.pop_back()
print("Popped: " + last.to_string())
print("List size: " + nums.size().to_string())
```

### Maps

Maps store key-value pairs. Keys and values can be any type. Access values using bracket notation or the `get()` method for safe access.

```mux title="maps.mux"
map<string, int> scores = {"Alice": 90, "Bob": 85}

print("Alice's score: " + scores["Alice"].to_string())

scores.put("Charlie", 95)
print("After put: " + scores.to_string())

auto charlies_score = scores.get("Charlie")
match charlies_score {
    some(v) { print("Charlie's score: " + v.to_string()) }
    none { print("Charlie not found") }
}

print("All keys: " + scores.get_keys().to_string())
```

### Sets

Sets are unordered collections of unique elements. They provide fast membership testing with `contains()`.

```mux title="sets.mux"
set<string> tags = {"urgent", "important"}

print("Tags: " + tags.to_string())
print("Has 'urgent': " + tags.contains("urgent").to_string())
print("Has 'review': " + tags.contains("review").to_string())

tags.add("review")
print("After add: " + tags.to_string())

auto removed = tags.remove("important")
print("Removed 'important': " + removed.to_string())
```

## Types

### Enums

Enums define tagged unions with data variants. Each variant can hold different types of data.

```mux title="enums.mux"
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

func area(Shape shape) returns float {
    match shape {
        Circle(r) { return 3.14159 * r * r }
        Rectangle(w, h) { return w * h }
        Square(s) { return s * s }
    }
}

auto circle = Shape.Circle(5.0)
auto rect = Shape.Rect(4.0, 6.0)

print("Circle area: " + area(circle).to_string())
print("Rectangle area: " + area(rect).to_string())
```

### Classes

Classes encapsulate data and behavior. They can implement interfaces and contain methods with access to instance data via `self`.

```mux title="classes.mux"
class Counter {
    int value

    common func from_start(int start) returns Counter {
        Counter c = Counter.new()
        c.value = start
        return c
    }
    
    func increment() returns void {
        self.value = self.value + 1
    }
    
    func get_value() returns int {
        return self.value
    }
}

auto counter = Counter.from_start(0)
counter.increment()
counter.increment()
print("Count: " + counter.get_value().to_string())
```

### Generic Classes

Generics allow you to write flexible, reusable code that works with any type while maintaining type safety.

```mux title="generic_stack.mux"
class Stack<T> {
    list<T> items

    common func from_list(list<T> source) returns Stack<T> {
        Stack<T> s = Stack<T>.new()
        s.items = source
        return s
    }
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns optional<T> {
        if self.items.is_empty() { return none }
        return self.items.pop_back()
    }
}

auto int_stack = Stack<int>.from_list([])
int_stack.push(42)
int_stack.push(99)

match int_stack.pop() {
    some(v) { print("Popped: " + v.to_string()) }
    none { print("Empty") }
}
```

## Error Handling

### Result Types

Result types represent either a successful value (`ok`) or an error (`err`). Pattern matching is used to handle both cases.

```mux title="results.mux"
func divide(int a, int b) returns result<int, string> {
    if b == 0 { return err("Cannot divide by zero") }
    return ok(a / b)
}

func safe_divide(int a, int b) returns void {
    match divide(a, b) {
        ok(value) {
            print("Result: " + value.to_string())
        }
        err(message) {
            print("Error: " + message)
        }
    }
}

safe_divide(10, 2)
safe_divide(10, 0)
```

### Optional Types

Optional types represent a value that might not exist. `some(value)` contains a value, `none` indicates absence.

```mux title="optionals.mux"
func find_first_even(list<int> nums) returns optional<int> {
    for int n in nums {
        if n % 2 == 0 { return some(n) }
    }
    return none
}

match find_first_even([1, 3, 5, 6, 7]) {
    some(n) { print("First even: " + n.to_string()) }
    none { print("No even numbers") }
}

match find_first_even([1, 3, 5, 7]) {
    some(n) { print("First even: " + n.to_string()) }
    none { print("No even numbers") }
}
```

## Operators

### Arithmetic

Mux supports standard arithmetic operators plus exponentiation.

```mux title="arithmetic.mux"
int a = 10
int b = 3

print("10 + 3 = " + (a + b).to_string())
print("10 - 3 = " + (a - b).to_string())
print("10 * 3 = " + (a * b).to_string())
print("10 / 3 = " + (a / b).to_string())
print("10 % 3 = " + (a % b).to_string())

print("2 ** 3 = " + (2 ** 3).to_string())
print("2.0 ** 3.0 = " + (2.0 ** 3.0).to_string())
```

### Logical Operators

Boolean operators include `!` (not), `&&` (and), and `||` (or). They support short-circuit evaluation.

```mux title="logical.mux"
bool is_valid = true
bool is_enabled = false

if is_valid && !is_enabled {
    print("Valid and not enabled")
}

if is_valid || is_enabled {
    print("Valid or enabled (or both)")
}

if !is_enabled {
    print("Not enabled")
}
```

### Comparison Operators

Compare values using standard comparison operators. All comparisons return boolean values.

```mux title="comparisons.mux"
int x = 5
int y = 10

print("5 == 5: " + (5 == 5).to_string())
print("5 != 10: " + (5 != 10).to_string())
print("5 < 10: " + (5 < 10).to_string())
print("5 > 10: " + (5 > 10).to_string())
print("5 <= 5: " + (5 <= 5).to_string())
print("10 >= 10: " + (10 >= 10).to_string())
```

## References and Pointers

### Reference Parameters

Functions can accept references to modify the original value or avoid copying large data.

```mux title="references.mux"
func increment(int ref n) returns void {
    n = n + 1
}

func swap(int ref a, int ref b) returns void {
    int temp = a
    a = b
    b = temp
}

int num = 5
increment(num)
print("After increment: " + num.to_string())

int x = 1
int y = 2
swap(x, y)
print("After swap: x = " + x.to_string() + ", y = " + y.to_string())
```

## Lambda Functions

Mux supports anonymous lambda functions that can be assigned to variables and passed around.

```mux title="lambdas.mux"
auto square = func(int n) returns int {
    return n * n
}

auto double = func(float n) returns float {
    return n * 2.0
}

print("square(5) = " + square(5).to_string())
print("double(3.5) = " + double(3.5).to_string())

auto nums = [1, 2, 3, 4, 5]
print("Original: " + nums.to_string())
```

---

## Try It Yourself

Want to experiment with these examples? Visit the [Mux Playground](/playground) to write and run your own code!
