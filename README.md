# Mux Language Specification

By Derek Corniello

## Why Mux?

- **Simple yet powerful:** Combines Go-like minimalism with Rust-inspired safety.
- **Strong static typing:** Helps catch errors early and ensures safer code.
- **LLVM-powered:** Fast compilation and native performance.
- **Flexible memory management:** Safe defaults with potential for advanced control.
- **Extensible:** Designed to evolve with features like traits, concurrency, and a standard library.

## 1. Overview

Mux (fully "MuxLang") is a statically-typed, garbage-collected language that combines:

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
  `func`, `returns`, `const`, `let`, `class`, `interface`, `enum`, `match`, `if`, `else`, `for`, `while`, `break`, `continue`, `return`, `import`, `is`, `Some`, `None`, `Ok`, `Err`

---

## 3. Types

**Note**: Not sure that I want to/will do implicit conversions, like 7 -> 7.0, 7 -> "7", or 7 -> true

### 3.1 Primitive Types

```
int      // 64-bit signed integer
float    // 64-bit IEEE-754
bool     // true | false
char     // Unicode code point
string   // UTF-8 sequence
```

### 3.2 Composite Types

```
Optional<T>
Result<T, E>
list<T>
map<K,V>
T[n]         // fixed-size array of n elements
```

### 3.3 Generics

Mux supports Go-style generics with type parameters:

```
// Generic function with type constraints
func max[T comparable](T a, T b) returns T {
    if a > b {
        return a
    }
    return b
}

// Generic function with multiple type parameters
func zip[T, U](list<T> first, list<U> second) returns list<(T, U)> {
    let result = list<(T, U)>()
    let minLen = min(first.length(), second.length())
    for int i = 0; i < minLen; i += 1 {
        result.append((first[i], second[i]))
    }
    return result
}

// Generic struct
struct Pair[T, U] {
    T first
    U second
}

// Generic interface with type constraints
interface Container[T any] {
    func add(T item) returns void
    func get(int index) returns T
    func size() returns int
}

// Type constraints
interface Ordered {
    func compare(Self other) returns int
}

interface Numeric {
    func add(Self other) returns Self
    func multiply(Self other) returns Self
}

// Built-in constraint aliases
// comparable - types that support == and !=
// any - any type (equivalent to no constraint)
```

### 3.4 Generic Type Constraints

```
// Using built-in constraints
func sort[T comparable](list<T> items) returns void {
    // sorting implementation
}

// Using custom constraints
func sum[T Numeric](list<T> numbers) returns T {
    let result = numbers[0]
    for int i = 1; i < numbers.length(); i += 1 {
        result = result.add(numbers[i])
    }
    return result
}

// Multiple constraints
func processOrdered[T Ordered & Numeric](T value) returns T {
    // T must implement both Ordered and Numeric
    return value.multiply(value)
}

// Type inference with generics
let numbers = [1, 2, 3, 4, 5]
let maximum = max(3, 7)        // Generic T inferred as int
let pairs = zip(numbers, ["a", "b", "c"])  // T=int, U=string inferred
```

### 3.5 User-Defined Types

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

### 4.2 Local Type Inference

Mux supports local type inference using the `let` keyword:

```
// Type inferred from initializer
let x = 42          // inferred as int
let pi = 3.14159    // inferred as float
let name = "Mux"    // inferred as string
let items = [1, 2, 3]  // inferred as list<int>

// Type inference in function calls
let result = calculateDistance(point1, point2)  // type inferred from function return

// Type inference with collections
let scores = {"Alice": 95, "Bob": 87}  // inferred as map<string, int>
let coords = [1.0, 2.5, 3.7]          // inferred as list<float>

// Unused variables with underscore
let _ = expensiveCalculation()  // result ignored
let (value, _) = getTuple()    // second element ignored
```

### 4.3 Inference Rules

- `let` can only be used when an initializer is present
- The type is inferred from the right-hand side expression
- Once inferred, the variable maintains that type (no dynamic typing)
- Function parameters and return types must still be explicitly declared
- Class fields must be explicitly typed
- Use `_` for unused variables to avoid compiler warnings

```
// Valid inference
let value = someFunction()
let list = [1, 2, 3]
let map = {"key": "value"}

// Invalid - no initializer
let x  // ERROR: cannot infer type without initializer

// Invalid - function parameters must be explicit
func process(let item) returns void { }  // ERROR

// Valid - unused parameter
func process(int item, int _) returns void { }  // second parameter unused
```

All declarations require either an explicit type or `let` with an initializer; semicolons are not used.

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
    let results = {"processed": 100, "skipped": 5}  // local inference
    let total = results["processed"] + results["skipped"]
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
- Local variables within functions can use `let` inference
- Use `_` for unused parameters

---

## 6. Lambdas & Closures

```
// Block-form lambda with explicit types
let square = func(int n) {
    return n * n
}

let doubler = func(let x) {
    return x * 2  // parameter type can be inferred in some contexts
}

// Passing lambdas to functions
let result = apply(10, func(int x) {
    return x + 5
})

// Lambda with unused parameters
let processFirst = func(int first, int _) {
    return first * 2  // second parameter ignored
}

// Block-form lambda with mixed explicit/inferred types
let filter = func(list<int> nums, func(int) returns bool cond) returns list<int> {
    let out = list<int>()  // explicit constructor, or could use []
    for n in nums {
        if cond(n) {
            out.append(n)
        }
    }
    return out
}
```

- All lambdas use block syntax with `func(params) { ... }`
- Lambda parameters can use `let` when type can be inferred from context
- Use `_` for unused lambda parameters
- Optional capture list in `[…]` form [needs more clarification]

---

## 7. Control Flow

### 7.1 If / Else

```
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}

// With type inference
let message = if x > 0 { "positive" } else { "non-positive" }
```

### 7.2 Match with Guards

```
match (value) {
    Some(v) if v > 10 {
        let msg = "large: " + v  // local inference
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

// Pattern matching with underscore for unused values
match (tuple) {
    (x, _) {
        print("First element: " + x)  // second element ignored
    }
    _ {
        print("No match")
    }
}
```

### 7.3 For Loops

```
for int i = 0; i < 5; i += 1 {
    print(i)
}

for item in myList {
    let processed = transform(item)  // type inferred
    print(processed)
}

// Iterator with inference
for let item in collection {
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

### 7.4 While Loops

```
while cond {
    let currentTime = getCurrentTime()  // local inference
    // ...
}
```

### 7.5 Break / Continue / Return

Works as in C/Java.

---

## 8. Enums (Tagged Unions)

```
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

// Usage with inference
let myShape = Circle(5.0)  // type inferred as Shape
let shapes = [Circle(1.0), Rectangle(2.0, 3.0)]  // inferred as list<Shape>

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

## 9. Classes & Traits

### 9.1 Traits (Interfaces)

```
interface Drawable {
    func draw() returns void
}
```

### 9.2 Classes with `is` Clause

```
class Circle is Drawable, ShapeLike {
    float radius  // explicit type required for fields

    func draw() returns void {
        let message = "Circle radius=" + radius  // local inference in methods
        print(message)
    }

    func area() returns float {
        const let PI = 3.1415  // inferred as float
        return PI * radius * radius
    }
    
    // Method with unused parameters
    func resize(float newRadius, string _) returns void {
        radius = newRadius  // second parameter ignored
    }
}

// Generic class example
class Stack[T] {
    list<T> items

    func push(T item) returns void {
        items.append(item)
    }

    func pop() returns Optional<T> {
        if items.isEmpty() {
            return None
        }
        let item = items.removeLast()
        return Some(item)
    }
}

// Usage with inference
let circle = Circle(5.0)  // type inferred as Circle
let shapes = list<Drawable>([circle])  // explicit generic type needed
let intStack = Stack<int>()  // explicit generic instantiation
let stringStack = Stack[string]()  // alternative syntax
```

- `is TraitA, TraitB` declares implemented traits; compiler enforces required methods
- No `implements` keyword, no `@Override`
- Class fields must be explicitly typed
- Local variables in methods can use `let` inference
- Use `_` for unused method parameters

---

## 10. Collections & Literals

```
// Explicit typing
list<int> nums = [1, 2, 3, 4]
map<string, int> scores = {"Alice": 90, "Bob": 85}

// With type inference
let nums = [1, 2, 3, 4]           // inferred as list<int>
let scores = {"Alice": 90, "Bob": 85}  // inferred as map<string, int>
let mixed = [1, 2.5, 3]           // ERROR: conflicting types, explicit type needed

// Nested collections
let matrix = [[1, 2], [3, 4]]     // inferred as list<list<int>>
let lookup = {"users": [1, 2, 3], "admins": [4, 5]}  // inferred as map<string, list<int>>

// Generic collections
let pairs = [Pair(1, "one"), Pair(2, "two")]  // inferred as list<Pair<int, string>>
let containers = list<Container<int>>()  // explicit generic type declaration
```

---

## 11. Error Handling

### 11.1 `Result<T, E>`

```
func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

// Usage with inference
let result = divide(10, 2)  // inferred as Result<int, string>
match result {
    Ok(value) {
        let message = "Result: " + value  // local inference
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

### 11.2 `Optional<T>`

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
let maybeEven = findEven([1, 3, 4, 7])  // inferred as Optional<int>

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

## 12. Memory Model

- **Garbage-collected** runtime; no manual `free` or ownership semantics
- All objects and collections live on the heap
- Primitives passed by value, objects by reference

---

## 13. Pointers

Mux supports C-like pointers with type inference:

- `*T` denotes pointer to `T`
- `&expr` takes address of `expr`
- `*expr` dereferences pointer
- Pointer arithmetic allowed on arrays

```
int x = 10
let p = &x        // inferred as int*
print(*p)         // 10
*p = 20
print(x)          // 20

let arr = [1, 2, 3, 4, 5]  // inferred as int[5]
let q = arr                // inferred as int*
q += 2
print(*q)         // 3

// Using underscore with pointers when values aren't needed
let (_, secondPtr) = getPointerPair()  // first pointer ignored
```

---

## 14. Modules & Imports

```
import math
import shapes.circle as circle

// Usage with inference
let pi = math.PI         // type inferred from math module
let c = circle.new(5.0)  // type inferred from constructor

// Import with unused alias for completeness
import utils.logger as _  // imported but not directly used in this scope
```

- Python-style imports only
- Module paths map directly to file paths
- Imported symbols can be used with type inference
- Use `_` alias when importing for side effects only

---

## 15. Type Inference Guidelines

### 15.1 When to Use `let`

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

### 15.2 Inference Limitations

```
// These require explicit types due to ambiguity
list<int> empty = []           // empty collection needs explicit type
let empty = list<int>()       // or explicit constructor

Result<int, string> pending    // uninitialized variables need explicit type

// Generic instantiation may need explicit types
let stack = Stack<int>()      // explicit generic parameter
let pairs = zip<int, string>(numbers, names)  // when inference is ambiguous
```

### 15.3 Using Underscore Effectively

```
// Good uses of underscore
let (first, _) = getTuple()           // ignore second element
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

const let PI = 3.14159  // inferred as float

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
func map[T, U](list<T> items, func(T) returns U transform) returns list<U> {
    let result = list<U>()
    for item in items {
        result.append(transform(item))
    }
    return result
}

func main() returns void {
    let shapes = [Circle(2.0), Circle(3.5)]  // inferred as list<Circle>
    
    for shape in shapes {
        let area = shape.area()  // inferred as float
        let message = "Area: " + area  // inferred as string
        print(message)
    }
    
    // Working with Results and inference
    let results = list<Result<float, string>>()
    for shape in shapes {
        let areaResult = Ok(shape.area())  // inferred as Result<float, string>
        results.append(areaResult)
    }
    
    // Using generics with inference and lambdas
    let areas = map(shapes, func(let s) {
        return s.area()  // inferred as list<float>
    })
    
    let descriptions = map(areas, func(let a) {
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
2. **Infer** types for `let` declarations based on initializer expressions
3. **Resolve** generic type parameters and constraints
4. **Check** type compatibility and enforce explicit typing rules
5. **Generate** code with full type information

Type inference errors are reported with suggestions for explicit typing when ambiguous. Generic type resolution follows Go's approach with clear constraint satisfaction checking.

The compiler will warn about unused variables unless they are explicitly marked with `_` or have names starting with `_`.

## 18. License

Mux will be licensed inder the MIT license.

---

# MuxLang 16-Week Plan (MVP)

This document outlines a week-by-week schedule for the development of MuxLang's Minimum Viable Product.

| Week | Focus | Tasks | Deliverables |
|------|-------|-------|-------------|
| 1 | **Language Design** | Finalize syntax & grammar; sketch AST; plan modules | Grammar document, AST outline |
| 2 | **Lexer Implementation** | Implement lexer; test tokenization | Working lexer |
| 3 | **Parser Implementation** | Parse expressions & statements; build AST | Working parser |
| 4 | **REPL Prototype** | Minimal REPL to test parsing/evaluation; parser tests | REPL prototype, parser test cases |
| 5 | **Type System – Core Types** | Implement int, float, bool, string, arrays; AST integration | Core type checker functional |
| 6 | **Type System – Functions & Scope** | Type checking for functions, variables, scopes | Type checker integrated with function support |
| 7 | **Type System – Traits/Interfaces** | Implement `is Trait` system; write tests | Trait system functional, test suite |
| 8 | **Memory Management Design** | Decide GC vs manual; design allocator/GC; integrate AST/runtime | Memory management design doc, skeleton allocator/GC |
| 9 | **Memory Management Implementation** | Implement basic GC/manual allocator; test memory correctness | Working memory management system |
| 10 | **LLVM Setup & Expression Codegen** | Install/setup LLVM; codegen for simple expressions | LLVM backend running simple expression programs |
| 11 | **LLVM Statement & Function Codegen** | Extend codegen to statements, loops, functions | Core constructs codegen functional |
| 12 | **Integration Testing** | Combine type checker, memory system, LLVM codegen; debug integration | MVP programs running end-to-end |
| 13 | **Standard Library – Core Types** | Implement I/O, collections, string utilities | Minimal stdlib working |
| 14 | **Compiler CLI & REPL Improvements** | Implement `mux run/build`; improve REPL; debugging | Usable compiler CLI with REPL |
| 15 | **Testing & Examples** | Write example programs; test edge cases; fix remaining bugs | Test suite, example programs |
| 16 | **Polish & Documentation** | Expand stdlib, improve error reporting, finalize docs | Polished MVP, documentation, final project report |

**Notes:**
- Hours per week: ~9–15
- Buffer is included in weeks 12–16 for testing, debugging, and polishing.
- The plan ensures a gradual progression from language design to a fully integrated MVP.
