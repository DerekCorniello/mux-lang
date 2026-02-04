# Mux Language Specification

By Derek Corniello

## Why Mux?

- **Simple yet powerful:** Combines Go-like minimalism with Rust-inspired safety.
- **Strong static typing:** Helps catch errors early and ensures safer code.
- **LLVM-powered:** Fast compilation and native performance.
- **Flexible memory management:** Safe defaults with potential for advanced control.
- **Extensible:** Designed to evolve with features like traits, concurrency, and a standard library.

## 1. Overview

Mux (fully "MuxLang") is a statically-typed, reference-counted language that combines:

- **Java-style explicit typing** with **local type inference**
- **Python-style collection literals**
- **Rust-style pattern-matching with guards**
- **Curly-brace syntax** and **no semicolons**
- **Minimal trait/Class model** (use `is` instead of `implements` like Java)
- **Built-in `Result<T,E>` and `Optional<T>` for error handling**

---

## 2. Lexical Structure

- **Case-sensitive** identifiers: letters, digits, `_`, not starting with a digit
- **Whitespace** (spaces, tabs, newlines) separates tokens
- **Comments**:
  - Single-line: `// comment`
  - Multi-line: `/* comment */`
- **Statement termination**: by end-of-line only (no semicolons)
- **Underscore placeholder**: `_` can be used for unused parameters, variables, or pattern matching wildcards
- **Keywords**:
  `func`, `returns`, `const`, `auto`, `class`, `interface`, `enum`, `match`, `if`, `else`, `for`, `while`, `break`, `continue`, `return`, `import`, `is`, `as`, `in`, `true`, `false`, `common`, `None`

---

## 3. Types

**Type System**: Mux uses **strict static typing** with **NO implicit type conversions**. All type conversions must be explicit using conversion methods.

### 3.1 Primitive Types

```
int      // 64-bit signed integer
float    // 64-bit IEEE-754
bool     // true | false
char     // Unicode code point
string   // UTF-8 sequence
```

### 3.2 Type Conversions

Mux requires **explicit type conversions** for all operations. There are no implicit conversions between types.

#### 3.2.1 Numeric Conversions

```mux
// Integer conversions
auto x = 42
auto x_float = x.to_float()     // int -> float
auto x_str = x.to_string()      // int -> str
auto x_same = x.to_int()        // int -> int (identity)

// Float conversions
auto pi = 3.14
auto pi_int = pi.to_int()       // float -> int (truncates: 3)
auto pi_str = pi.to_string()    // float -> str
auto pi_same = pi.to_float()    // float -> float (identity)

// Boolean conversions
auto flag = true
auto flag_int = flag.to_int()   // bool -> int (true=1, false=0)
auto flag_float = flag.to_float() // bool -> float (true=1.0, false=0.0)
auto flag_str = flag.to_string() // bool -> string ("true" or "false")

// Char conversions
auto ch = 'A'
auto ch_str = ch.to_string()    // char -> str

// Method calls on literals require parentheses
auto num = (3).to_string()      // Valid
auto val = (42).to_float()      // Valid
// auto bad = 3.to_string()     // ERROR: parsed as float 3.0
```

#### 3.2.2 String Parsing (Fallible Conversions)

String and char parsing methods return `Result<T, string>` because they can fail:

```mux
// String to number (returns Result)
auto num_str = "42"
auto result = num_str.to_int()
match result {
    Ok(value) {
        print("Parsed: " + value.to_string())  // "Parsed: 42"
    }
    Err(error) {
        print("Parse error: " + error)
    }
}

// String to float
auto float_str = "3.14159"
auto float_result = float_str.to_float()
match float_result {
    Ok(value) { print(value.to_string()) }
    Err(msg) { print("Error: " + msg) }
}

// Char to digit (only works for '0'-'9')
auto digit_char = '5'
auto digit_result = digit_char.to_int()
match digit_result {
    Ok(digit) { print(digit.to_string()) }  // "5"
    Err(msg) { print(msg) }
}

auto letter = 'A'
auto letter_result = letter.to_int()
match letter_result {
    Ok(_) { print("Unexpected success") }
    Err(msg) { print(msg) }  // "Character is not a digit (0-9)"
}
```

#### 3.2.3 No Implicit Conversions

The following operations are **compile-time errors**:

```mux
// Type mismatches in binary operations
auto bad1 = 1 + 1.0        // ERROR: cannot add int and float
auto bad2 = "hello" + 3    // ERROR: cannot add string and int
auto bad3 = true + false   // ERROR: cannot add bool and bool

// Type mismatches in comparisons
auto bad4 = 1 < 1.0        // ERROR: cannot compare int and float
auto bad5 = "a" == 1       // ERROR: cannot compare string and int

// Function argument type mismatches
func takes_string(string s) returns void { }
takes_string(123)          // ERROR: expected string, got int

// Correct usage requires explicit conversion
auto good1 = 1 + (1.0).to_int()           // OK: 2
auto good2 = "hello" + (3).to_string()    // OK: "hello3"
auto good3 = 1.to_float() < 1.0           // OK: true
auto good4 = (true).to_int() + (false).to_int()  // OK: 1
```

#### 3.2.4 Available Conversion Methods

| From Type | Method | Returns | Notes |
|-----------|--------|---------|-------|
| `int` | `.to_string()` | `string` | Converts to string representation |
| `int` | `.to_float()` | `float` | Converts to floating-point |
| `int` | `.to_int()` | `int` | Identity function |
| `float` | `.to_string()` | `string` | Converts to string representation |
| `float` | `.to_int()` | `int` | Truncates decimal part |
| `float` | `.to_float()` | `float` | Identity function |
| `bool` | `.to_string()` | `string` | Returns "true" or "false" |
| `bool` | `.to_int()` | `int` | Returns 1 or 0 |
| `bool` | `.to_float()` | `float` | Returns 1.0 or 0.0 |
| `char` | `.to_string()` | `string` | Converts char to string |
| `char` | `.to_int()` | `Result<int, string>` | Digit value for '0'-'9' only |
| `string` | `.to_string()` | `str` | Identity function |
| `string` | `.to_int()` | `Result<int, string>` | Parses string as integer |
| `string` | `.to_float()` | `Result<float, string>` | Parses string as float |

### 3.3 Built-in Functions

Mux provides essential built-in functions for output and utility operations. These are always available without imports.

#### Output Functions

**`print(string message)`** - Prints a string to standard output with a trailing newline.

```mux
print("Hello, World")  // Output: Hello, World\n
// Multiple prints produce separate lines:
print("First")
print("Second")  // First\nSecond\n
// To print without newline, use multiple arguments or string concatenation:
print("Hello, " + "World")  // Output: Hello, World\n
// Or use the runtime function directly (advanced):
// mux_print_raw("No newline") - if available in runtime
```

#### Utility Functions

**`range(int start, int end) -> list<int>`** - Returns a list of integers from `start` (inclusive) to `end` (exclusive). The result is always a `list<int>`.

```mux
// Generate indices for iteration
for i in range(0, 5) {
    print(i.to_string())  // Prints 0, 1, 2, 3, 4
}

// Create a list of numbers
auto numbers = range(10, 15)  // [10, 11, 12, 13, 14]
```

**Design Note:** `range()` is the primary way to create numeric sequences for iteration, as Mux does not support C-style `for (int i = 0; i < n; i++)` loops.

#### Input Functions

**`read_line() -> string`** - Reads a line from standard input and returns it as a string (excluding the newline).

```mux
print("Enter your name: ")
auto name = read_line()
print("Hello, " + name)
```

### 3.4 Composite Types

```
Optional<T>
Result<T, E>
list<T>
map<K,V>
```

### 3.5 Generics

Mux supports Go/Rust-style generics with type parameters and interface bounds using the `is` keyword:

```mux
// Generic function with type constraints
func max<T is Comparable>(T a, T b) returns T {
    if a > b {
        return a
    }
    return b
}

// Generic function with Stringable bound for to_string()
func greet<T is Stringable>(T value) returns string {
    return "Hello, " + value.to_string()
}

// Generic function with Add bound for + operator
func add<T is Add>(T a, T b) returns T {
    return a.add(b)
}

// Generic class
class Stack<T> {
    list<T> items
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns Optional<T> {
        if self.items.is_empty() { return None }
        return self.items.pop_back()
    }
}
```

### 3.6 Built-in Interfaces

Mux provides built-in interfaces for common operations on generic types:

| Interface | Methods | Description |
|-----------|---------|-------------|
| `Stringable` | `to_string() -> string` | Types that can be converted to string |
| `Add` | `add(Self) -> Self` | Types that support `+` operator |
| `Sub` | `sub(Self) -> Self` | Types that support `-` operator |
| `Mul` | `mul(Self) -> Self` | Types that support `*` operator |
| `Div` | `div(Self) -> Self` | Types that support `/` operator |
| `Arithmetic` | `add`, `sub`, `mul`, `div` | Types that support all arithmetic operators |
| `Equatable` | `eq(Self) -> bool` | Types that support `==` and `!=` operators |
| `Comparable` | `cmp(Self) -> int` | Types that support `<`, `<=`, `>`, `>=` operators |

**Operator Mapping:**
- `a + b` uses `Add.add()` when type doesn't natively support `+`
- `a > b` uses `Comparable.cmp()` returning -1, 0, or 1
- `a == b` uses `Equatable.eq()`

**Primitives and Interfaces:**
- `int`: Implements `Stringable`, `Add`, `Sub`, `Mul`, `Div`, `Arithmetic`, `Equatable`, `Comparable`
- `float`: Implements `Stringable`, `Add`, `Sub`, `Mul`, `Div`, `Arithmetic`, `Equatable`, `Comparable`
- `string`: Implements `Stringable`, `Add`, `Equatable`, `Comparable`
- `bool`: Implements `Stringable`, `Equatable`

**Example: Custom Type Implementing Interfaces**
```mux
interface Add {
    func add(Self) returns Self
}

class Point {
    int x
    int y
    
    func add(Point other) returns Point {
        return Point.new(self.x + other.x, self.y + other.y)
    }
}

func sum_points<T is Add>(list<T> points) returns T {
    auto result = points[0]
    for i in range(1, points.size()) {
        result = result.add(points[i])
    }
    return result
}

auto points = [Point.new(1, 2), Point.new(3, 4), Point.new(5, 6)]
auto total = sum_points(points)  // Point(9, 12)
```

### 3.7 Generic Type Constraints

```mux
// Using built-in interfaces
func process<T is Stringable>(list<T> items) returns void {
    for item in items {
        print(item.to_string())
    }
}

// Multiple bounds (AND semantics - type must implement all)
func combine<T is Add & Stringable>(T a, T b) returns string {
    return (a.add(b)).to_string()
}

// Type parameters must be explicitly specified
auto max_int = max<int>(3, 7)           // T = int, Comparable bound satisfied
auto names = ["apple", "banana"]
print(greet<string>(names[0]))          // T = string, Stringable bound satisfied
```

### 3.8 User-Defined Types

- **Structs**: simple aggregates
- **Enums**: tagged unions (see §8)
- **Classes**: with fields + methods (see §9)

---

## 4. Variable & Constant Declarations

### 4.1 Explicit Typing

```
const int MAX = 100

// Variables (explicit type required for declarations without inference)
int x = 5
bool flag = true
string name = "MuxLang"
```

### 4.2 Variable Declarations

Mux supports both explicit types and type inference with `auto`:

```
// Type inferred with 'auto'
auto x = 42          // inferred as int
auto pi = 3.14159    // inferred as float
auto name = "Mux"    // inferred as str

// Explicit type annotation
int count = 0
list<string> names = []
map<string, string | int> user = {"name": "Alice", "age": 30}

// Valid inference
auto value = someFunction()
auto numbers = [1, 2, 3]
map<string, string> userMap = {"key": "value"}

// Invalid - no initializer with 'auto'
auto x  // ERROR: cannot infer type without initializer

// Function parameters must be explicitly typed
func process(auto item) returns void { }  // ERROR
func process(int item) returns void { }   // Valid

// Unused parameter
func process(int item, int _) returns void { }  // second parameter unused
```

All declarations require either an explicit type or `auto` with an initializer; semicolons are not used.

### 4.3 Constants

Constants are immutable values that cannot be reassigned or modified after initialization:

```
// Function-level constants
func calculate() returns int {
    const int MULTIPLIER = 10
    const float TAX_RATE = 0.08
    int value = 100
    return value * MULTIPLIER
}

// Constants in classes
class Config {
    const int MAX_RETRIES
    int current_retry
    
    func increment() returns void {
        self.current_retry++  // OK - mutable field
        // self.MAX_RETRIES++  // ERROR: Cannot modify const field 'MAX_RETRIES'
    }
}

auto cfg = Config.new()
cfg.current_retry = 1  // OK - mutable field
// cfg.MAX_RETRIES = 5  // ERROR: Cannot assign to const field 'MAX_RETRIES'
```

**Const Enforcement:**
- Cannot reassign: `const_var = new_value` → ERROR
- Cannot use compound assignment: `const_var += 1` → ERROR
- Cannot increment/decrement: `const_var++` or `const_var--` → ERROR
- Applies to both identifiers and class fields
- Use `const` when you want a value that won't change after initialization

---

## 5. Functions

```
func add(int a, int b) returns int {
    return a + b
}

func greet(string name, int times = 1) returns void {
    for i in range(0, times) {
        print("Hello, " + name)
    }
}

func processData() returns map<string, int> {
    map<string, int> results = {"processed": 100, "skipped": 5}
    auto total = results["processed"] + results["skipped"]
    results["total"] = total
    return results
}

// Function with unused parameters
func callback(string event, int timestamp, string _) returns void {
    print("Event: " + event + " at " + timestamp)
    // third parameter ignored
}
```

- Keyword `func`
- Parameter list with explicit types; default values optional
- `returns` clause for return type (explicit, no inference)
- Body enclosed in `{…}`; no semicolons
- Local variables within functions can use `auto` inference
- Use `_` for unused parameters

---

## 6. Operators

### 6.1 Arithmetic Operators

Mux supports standard arithmetic operations with strict type requirements (no implicit conversions):

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `+` | Addition | `int`, `float`, `string` | `5 + 3`, `"a" + "b"` |
| `-` | Subtraction | `int`, `float` | `10 - 4` |
| `*` | Multiplication | `int`, `float` | `6 * 7` |
| `/` | Division | `int`, `float` | `15 / 3` |
| `%` | Modulo | `int`, `float` | `10 % 3` // 1 |
| `**` | Exponentiation | `int`, `float` | `2 ** 3` // 8 |

**Exponentiation Operator (`**`):**
- Right-associative: `2 ** 3 ** 2` equals `2 ** (3 ** 2)` = 512
- Higher precedence than `*` and `/`: `2 * 3 ** 2` equals `2 * 9` = 18
- Works on both `int` and `float` types

```mux
auto squared = 5 ** 2        // 25
auto cubed = 2.0 ** 3.0      // 8.0
auto complex = 2 ** 3 ** 2   // 512 (right-associative)
```

### 6.2 Increment and Decrement Operators

Mux provides postfix increment (`++`) and decrement (`--`) operators with specific design constraints:

**Design Constraints:**
- **Postfix only**: `counter++` is valid, `++counter` is not supported
- **Standalone only**: Must appear on their own statement line, not within expressions
- **Only on mutable variables**: Cannot be used on `const` declarations or literals
- **Type preservation**: Operates on `int` types only

```mux
auto counter = 0
counter++         // Valid: counter is now 1
counter--         // Valid: counter is now 0

// INVALID - cannot use in expressions:
// auto x = counter++    // ERROR: ++ cannot be used in expressions
// auto y = (counter++) + 5  // ERROR: standalone only

// INVALID - prefix not supported:
// ++counter            // ERROR: prefix increment not supported

// INVALID - cannot modify const:
const int MAX = 100
// MAX++               // ERROR: cannot modify const
```

**Rationale:** The postfix-only, standalone-only design prevents ambiguity and side-effect confusion that can occur with prefix operators or expression-embedded increments.

### 6.3 Comparison and Logical Operators

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `==` | Equality | All comparable types | `a == b` |
| `!=` | Inequality | All comparable types | `a != b` |
| `<` | Less than | `int`, `float`, `string` | `5 < 10` |
| `<=` | Less than or equal | `int`, `float`, `string` | `x <= 100` |
| `>` | Greater than | `int`, `float`, `string` | `y > 0` |
| `>=` | Greater than or equal | `int`, `float`, `string` | `age >= 18` |
| `&&` | Logical AND | `bool` | `a && b` (short-circuit) |
| `\|\|` | Logical OR | `bool` | `a \|\| b` (short-circuit) |
| `!` | Logical NOT | `bool` | `!flag` |

**Short-circuit Evaluation:**
- `&&` only evaluates right side if left is `true`
- `||` only evaluates right side if left is `false`

### 6.4 The `in` Operator

The `in` operator tests for membership/containment with strict type requirements:

| Left Operand | Right Operand | Description |
|--------------|---------------|-------------|
| `T` | `list<T>` | Check if value exists in list |
| `T` | `set<T>` | Check if value exists in set |
| `string` | `string` | Check if substring exists |
| `char` | `string` | Check if character exists in string |

**Type Constraints:**
- Both operands must have compatible element types
- No implicit type conversions allowed
- Returns `bool`

```mux
// List containment
auto nums = [1, 2, 3, 4, 5]
auto hasThree = 3 in nums     // true
auto hasTen = 10 in nums      // false

// Set containment
auto tags = {"urgent", "important"}
auto isUrgent = "urgent" in tags    // true

// String containment
auto msg = "hello world"
auto hasWorld = "world" in msg      // true
auto hasFoo = "foo" in msg          // false

// Character in string
auto hasO = 'o' in msg              // true
auto hasZ = 'z' in msg              // false

// INVALID - type mismatch:
// auto bad = "1" in nums           // ERROR: string not in list<int>
// auto bad2 = 1 in msg             // ERROR: int not in string
```

### 6.5 Collection Operators

#### Concatenation with `+`

The `+` operator is overloaded for collection types with type-specific semantics:

| Types | Operation | Result |
|-------|-----------|--------|
| `list<T> + list<T>` | Concatenation | Combined list |
| `map<K,V> + map<K,V>` | Merge | Combined map (latter overwrites former on key collision) |
| `set<T> + set<T>` | Union | Set containing all unique elements |
| `string + string` | Concatenation | Combined string |

**Type Constraints:**
- Both operands must be the exact same collection type
- No mixing of collection types (e.g., `list + set` is an error)
- No implicit element type conversions

```mux
// List concatenation
auto list1 = [1, 2]
auto list2 = [3, 4]
auto combined = list1 + list2    // [1, 2, 3, 4]

// Map merge (latter wins on key collision)
auto map1 = {"a": 1, "b": 2}
auto map2 = {"b": 3, "c": 4}     // Note: key "b" exists in both
auto merged = map1 + map2        // {"a": 1, "b": 3, "c": 4}

// Set union
auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}

// String concatenation
auto greeting = "Hello, " + "World"  // "Hello, World"

// INVALID - type mismatch:
// auto bad = [1, 2] + {3, 4}       // ERROR: cannot add list and set
// auto bad2 = [1, 2] + [3.0, 4.0]  // ERROR: list<int> + list<float>
```

---

## 7. Lambdas & Closures

```
// Block-form lambda with explicit types and return type
auto square = func(int n) returns int {
    return n * n
}

auto doubler = func(float x) returns float {
    return x * 2.0
}

// Passing lambdas to functions
auto result = apply(10, func(int x) returns int {
    return x + 5
})

// Lambda with unused parameters
auto processFirst = func(int first, int _) returns int {
    return first * 2  // second parameter ignored
}

// Block-form lambda with mixed explicit/inferred types
auto filter = func(list<int> nums, func(int) returns bool cond) returns list<int> {
    list<int> out = []
    for n in nums {
        if cond(n) {
            out.push_back(n)
        }
    }
    return out
}
```

- All lambdas use block syntax with `func(params) { ... }`
- Lambda parameters can use `auto` when type can be inferred from context
- Use `_` for unused lambda parameters
- Optional capture list in `[…]` form [needs more clarification]

---

## 8. Control Flow

### 8.1 If / Else

```
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}

// With type inference
auto message = if x > 0 { "positive" } else { "non-positive" }
```

### 8.2 Match with Guards

```
match (value) {
    Some(v) if v > 10 {
        auto msg = "large: " + v  // local inference
        print(msg)
    }
    Some(v) {
        print("small: " + v)
    }
    None {
        print("no value")
    }
    _ {
        print("unexpected case")  // wildcard pattern
    }
}
```

### 8.3 For Loops

```
for item in myList {
    auto processed = transform(item)  // type inferred
    print(processed)
}

// Iterator with inference
for item in collection {
    // item type inferred from collection element type
    process(item)
}

// Ignoring loop variables when not needed
for _ in range(0, 10) {
    doSomething()  // don't care about the index
}

// Destructuring in loops with unused parts
for (key, _) in keyValuePairs {
    print("Key: " + key)  // value ignored
}
```

### 8.4 While Loops

```
while cond {
    auto currentTime = getCurrentTime()  // local inference
    // ...
}
```

### 8.5 Break / Continue / Return

Works as in C/Java.

---

## 9. Enums (Tagged Unions)

```
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}
```

```
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

// Usage with inference
auto myShape = Circle.new(5.0)  // type inferred as Shape
list<Shape> shapes = [Circle.new(1.0), Rectangle.new(2.0, 3.0)]

// Pattern matching with unused enum data
match (shape) {
    Circle(_) {
        print("It's a circle")  // radius value ignored
    }
    Rectangle(width, _) {
        print("Rectangle with width: " + width)  // height ignored
    }
    Square(size) {
        print("Square with size: " + size)
    }
}
```

Each variant may carry data. Pattern-match with destructuring and guards. Use `_` to ignore unused enum data in patterns.

---

## 10. Classes & Traits

### 10.1 Traits (Interfaces)

```
interface Drawable {
    func draw() returns void
}
```

### 10.2 Classes with `is` Clause

```
class Circle is Drawable, ShapeLike {
    float radius  // explicit type required for fields

    func draw() returns void {
        auto message = "Circle radius=" + radius  // local inference in methods
        print(message)
    }

    func area() returns float {
        const float PI = 3.1415  // inferred as float
        return PI * radius * radius
    }
    
    // Method with unused parameters
    func resize(float newRadius, string _) returns void {
        radius = newRadius  // second parameter ignored
    }
}

// Generic class example
class Stack<T> {
    list<T> items

    func push(T item) returns void {
        items.push_back(item)
    }

    func pop() returns Optional<T> {
        if items.isEmpty() {
            return None
        }
        auto item = items.pop_back()
        return Some(item)
    }
}

// Usage with inference
auto circle = Circle.new(5.0)  // type inferred as Circle
list<Drawable> shapes = [circle]
Stack<int> intStack = Stack<int>.new()  // explicit generic instantiation with .new()
```

### 10.3 Static Methods with `common`

Mux uses the `common` keyword to declare static (class-level) methods that can be called without an instance. This is distinct from `const` which declares immutable values.

**`common` vs `const`:**

| Keyword | Purpose | Usage | Example |
|---------|---------|-------|---------|
| `common` | Static methods and factory functions | Called on the class itself, not instances | `ClassName.method()` |
| `const` | Immutable constants | Values that cannot change after initialization | `const int MAX = 100` |

```mux
class Stack<T> {
    list<T> items
    
    // Instance method - called on instances
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    // Static method - called on the class
    common func who_am_i() returns string {
        return "I'm a Stack!"
    }
    
    // Factory pattern - static method that creates instances
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
auto stack = Stack<int>.new()
stack.push(42)                             // Instance method
```

**Key Differences:**
- **Instance methods** (no keyword) operate on `self` and require an instance
- **Static methods** (`common`) have no `self` and are called on the class
- **Const fields** are immutable instance/class fields, not methods
- Static methods cannot access instance fields (no `self` context)
- Factory patterns commonly use `common func from(...)` to create instances with pre-populated data

### 10.4 Class Instantiation

Classes are instantiated using the `.new()` method pattern:

```mux
// Basic instantiation
auto circle = Circle.new()           // No constructor arguments
auto circle2 = Circle.new(5.0)       // With constructor arguments

// Generic class instantiation
auto int_stack = Stack<int>.new()
auto string_stack = Stack<string>.new()

// Using factory methods
auto prebuilt = Stack<int>.from([1, 2, 3])
auto pair = Pair<string, int>.from("key", 42)
```

**Design Note:** Mux uses explicit `.new()` rather than direct constructor calls to distinguish class instantiation from function calls and enum variant construction.

---

## 11. Collections & Literals

```
// Explicit typing
list<int> nums = [1, 2, 3, 4]
map<string, int> scores = {"Alice": 90, "Bob": 85}

// With type inference
auto nums = [1, 2, 3, 4]           // inferred as list<int>
map<string, int> scores = {"Alice": 90, "Bob": 85}
// mixed = [1, 2.5, 3]           // ERROR: conflicting types, explicit type needed

// Nested collections
list<list<int>> matrix = [[1, 2], [3, 4]]
map<string, list<int>> lookup = {"users": [1, 2, 3], "admins": [4, 5]}

// Complex nested structures
auto users = [
    {"name": "Alice", "scores": [95, 87, 92]},
    {"name": "Bob", "scores": [78, 85, 90]}
]  // inferred as list<map<string, string | list<int>>>

auto data = {
    "numbers": [1, 2, 3, 4, 5],
    "metadata": {"version": "1.0", "count": 5}
}  // inferred as map<string, list<int> | map<string, string | int>>

// Generic collections
list<Pair<int, string>> pairs = [Pair.new(1, "one"), Pair.new(2, "two")]
list<Container<int>> containers = list<Container<int>>()
```

### 11.1 Collection Methods

All collections provide a consistent API for access, mutation, and inspection.

#### List Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Returns the number of elements in the list |
| `.is_empty()` | `bool` | Returns `true` if list has no elements |
| `.get(int index)` | `Optional<T>` | Safe access; returns `Some(value)` or `None` if out of bounds |
| `[int index]` | `T` | Direct access; runtime error if out of bounds |
| `.push(T item)` | `void` | Appends item to the end of the list (alias for push_back) |
| `.push_back(T item)` | `void` | Appends item to the end of the list |
| `.pop()` | `Optional<T>` | Removes and returns last item, or `None` if empty (alias for pop_back) |
| `.pop_back()` | `Optional<T>` | Removes and returns last item, or `None` if empty |
| `.to_string()` | `string` | Returns a string representation of the list |

```mux
auto nums = [1, 2, 3]

// Safe access with Optional
match nums.get(0) {
    Some(first) { print(first.to_string()) }  // "1"
    None { print("Index out of bounds") }
}

// Direct access (runtime error if index invalid)
auto second = nums[1]  // 2

// Mutation
nums.push_back(4)      // [1, 2, 3, 4]
match nums.pop_back() {
    Some(last) { print(last.to_string()) }  // "4"
    None { }
}

// Inspection
print(nums.size().to_string())     // "3"
print(nums.is_empty().to_string())   // "false"
```

#### Map Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Returns the number of key-value pairs |
| `.is_empty()` | `bool` | Returns `true` if map has no entries |
| `.get(K key)` | `Optional<V>` | Safe lookup; returns `Some(value)` or `None` if key not found |
| `[K key]` | `V` | Direct access; runtime error if key not found |
| `.put(K key, V value)` | `void` | Inserts or updates a key-value pair |
| `.contains(K key)` | `bool` | Returns `true` if key exists in map |
| `.remove(K key)` | `Optional<V>` | Removes key and returns value, or `None` if key not found |
| `.to_string()` | `string` | Returns a string representation of the map |

```mux
auto scores = {"Alice": 90, "Bob": 85}

// Safe access
match scores.get("Alice") {
    Some(score) { print(score.to_string()) }  // "90"
    None { print("Student not found") }
}

// Direct access
auto bobScore = scores["Bob"]  // 85

// Map entries are immutable; reassign to update
scores["Alice"] = 95  // Updates existing key
```

#### Set Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.size()` | `int` | Returns the number of elements |
| `.is_empty()` | `bool` | Returns `true` if set is empty |
| `.add(T item)` | `void` | Adds an item to the set |
| `.contains(T item)` | `bool` | Returns `true` if item exists in set |
| `.remove(T item)` | `Optional<T>` | Removes item and returns it, or `None` if not found |
| `.to_string()` | `string` | Returns a string representation of the set |

```mux
auto tags = {"urgent", "important", "review"}
print(tags.size().to_string())  // "3"

// Add and check membership
tags.add("priority")
if tags.contains("urgent") {
    print("Has urgent tag")
}

// Remove item
match tags.remove("review") {
    Some(removed) { print("Removed: " + removed) }
    None { print("Item not found") }
}
```

**Design Note:** Collections use consistent method naming across all types. Safe access via `.get()` returns `Optional<T>`, while direct access with `[]` provides unchecked access with runtime bounds checking.

---

## 12. Error Handling

### 12.1 `Result<T, E>`

```
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

// Usage with inference
auto result = divide(10, 2)  // inferred as Result<int, string>
match result {
    Ok(value) {
        auto message = "Result: " + value  // local inference
        print(message)
    }
    Err(error) {
        print("Error: " + error)
    }
    _ {
        print("Unexpected result")  // wildcard for completeness
    }
}

// Ignoring error details when not needed
match result {
    Ok(value) {
        print("Success: " + value)
    }
    Err(_) {
        print("Some error occurred")  // error details ignored
    }
}
```

### 12.2 `Optional<T>`

```
func findEven(list<int> xs) returns Optional<int> {
    for x in xs {
        if x % 2 == 0 {
            return Some(x)
        }
    }
    return None
}

// Usage with inference
Optional<int> maybeEven = findEven([1, 3, 4, 7])  // inferred as Optional<int>

match maybeEven {
    Some(value) {
        print("Found even: " + value)
    }
    None {
        print("No even number found")
    }
    _ {
        print("Unexpected optional state")
    }
}

// Ignoring the wrapped value when you just care about presence
match maybeEven {
    Some(_) {
        print("Got a value")  // don't care what the value is
    }
    None {
        print("Got nothing")
    }
}
```

Use `match` to unpack results and optionals. Use `_` to ignore unused values in patterns.

---

## 13. Memory Model

- **Reference-counted** runtime; deterministic memory management with no manual `free`
- All objects and collections live on the heap
- Primitives passed by value, objects by reference

---

## 14. References

Mux uses references for safe memory access and manipulation:

- `&T` denotes a reference to type `T`
- `&expr` creates a reference to `expr`
- References are automatically dereferenced when used
- No pointer arithmetic is allowed
- References are non-nullable by default
- Use `Option<&T>` for nullable references

```mux
// Basic reference usage
int x = 10
auto r = &x      // r is of type &int
print("ref value: " + (*r).to_string())  // 10 - explicit dereference with *

*r = 20          // Changes x to 20 via dereference
print("val after ref update: " + (*r).to_string())  // 20
print("x is now: " + x.to_string())  // 20

// References to list elements
auto numbers = [1, 2, 3, 4, 5]
auto first = &numbers[0]  // &int
print("first element: " + (*first).to_string())  // 1

// Function taking a reference
func update(&int ref) returns void {
    *ref = *ref + 1  // Must explicitly dereference to modify
}

update(&x)
print("val after update: " + x.to_string())  // 21
```

**Reference Syntax:**
- Create reference: `&variable` or `&expression`
- Dereference: `*reference` (required for both reading and writing)
- Pass to functions: `func(&int ref)` declares parameter, `update(&x)` passes reference
- References to references: Not supported

**Design Note:** Unlike some languages with automatic dereferencing, Mux requires explicit `*` for all reference operations. This makes memory access patterns explicit and prevents accidental mutation bugs.

---

## 15. Modules & Imports

```
import math
import shapes.circle as circle

// Usage with inference
float pi = math.PI         // type inferred from math module
auto c = circle.new(5.0)  // type inferred from constructor

// Import with unused alias for completeness
import utils.logger as _  // imported but not directly used in this scope
```

- Python-style imports only
- Module paths map directly to file paths
- Imported symbols can be used with type inference
- Use `_` alias when importing for side effects only

---

## 15. Type Inference Guidelines

### 16.1 When to Use `auto`

**Recommended:**

- Local variables with obvious initialization
- Complex generic types that are clear from context
- Temporary variables in calculations
- Iterator variables in loops

**Not Recommended:**

- Function parameters and return types (must be explicit)
- Public class fields (must be explicit)
- When the inferred type is not obvious to readers
- Long-lived variables where explicit type aids readability

### 16.2 Inference Limitations

```
// These require explicit types due to ambiguity
list<int> empty = []           // empty collection needs explicit type
auto empty = list<int>()       // or explicit constructor

Result<int, string> pending    // uninitialized variables need explicit type

// Generic instantiation may need explicit types
Stack[int] stack = Stack[int]()      // explicit generic parameter
auto pairs = zip<int, string>(numbers, names)  // when inference is ambiguous
```

### 16.3 Using Underscore Effectively

```
// Good uses of underscore
func process(int data, string _) { }  // ignore second parameter
for _ in range(0, 10) { }            // ignore loop counter
match result { Ok(_) { } }           // ignore success value

// Avoid overusing underscore when names would help readability
// Less clear:
func calculate(int _, int _, float _) returns float { }

// Better:
func calculate(int width, int height, float _) returns float { }
```

---

## 16. Example Program

```
import math

const float PI = 3.14159  // inferred as float

enum MaybeValue<T> { 
    Some(T) 
    None 
}

interface Shape {
    func area() returns float
}

class Circle is Shape {
    float r  // explicit type required for fields
    
    func area() returns float { 
        return PI * r * r 
    }
}

// Generic utility function
func map<T, U>(list<T> items, func(T) returns U transform) returns list<U> {
    auto result = list<U>()
    for item in items {
        result.push_back(transform(item))
    }
    return result
}

func main() returns void {
    auto shapes = [Circle.new(2.0), Circle.new(3.5)]  // inferred as list<Circle>
    
    for shape in shapes {
        float area = shape.area()  // inferred as float
        string message = "Area: " + area  // inferred as str
        print(message)
    }
    
    // Working with Results and inference
    auto results = list<Result<float, string>>()
    for shape in shapes {
        auto areaResult = Ok(shape.area())  // inferred as Result<float, string>
        results.push_back(areaResult)
    }
    
    // Using generics with inference and lambdas
    auto areas = map(shapes, func(Shape s) {
        return s.area()  // inferred as list<float>
    })
    
    auto descriptions = map(areas, func(string a) {
        return "Area: " + a  // inferred as list<string>
    })
    
    // Pattern matching with underscore
    for result in results {
        match result {
            Ok(value) {
                print("Success: " + value)
            }
            Err(_) {
                print("Error occurred")  // don't care about error details
            }
        }
    }
}
```

## 17. Compiler Behavior

The Mux compiler performs type inference during the semantic analysis phase:

1. **Parse** the source code into an AST
2. **Infer** types for `auto` declarations based on initializer expressions
3. **Resolve** generic type parameters and constraints
4. **Check** type compatibility and enforce explicit typing rules
5. **Generate** code with full type information

Type inference errors are reported with suggestions for explicit typing when ambiguous. Generic type resolution follows Go's approach with clear constraint satisfaction checking.

The compiler will warn about unused variables unless they are explicitly marked with `_` or have names starting with `_`.

## 18. License

Mux is licensed under the MIT license.

---
