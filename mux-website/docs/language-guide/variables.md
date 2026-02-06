# Variables and Constants

Mux supports both explicit type declarations and type inference with the `auto` keyword.

## Variable Declarations

### Explicit Typing

```mux
int x = 5
bool flag = true
string name = "MuxLang"
float pi = 3.14159
```

### Type Inference with `auto`

```mux
// Type inferred from initializer
auto x = 42          // inferred as int
auto pi = 3.14159    // inferred as float
auto name = "Mux"    // inferred as string

// Explicit type annotation when needed
list<string> names = []
map<string, int> scores = {"Alice": 90, "Bob": 85}

// Valid inference
auto value = someFunction()
auto numbers = [1, 2, 3]
```

### Important Rules

```mux
// Invalid - no initializer with 'auto'
// auto x  // ERROR: cannot infer type without initializer

// Function parameters must be explicitly typed
// func process(auto item) returns void { }  // ERROR
func process(int item) returns void { }   // Valid

// Unused parameter
func process(int item, int _) returns void { }  // second parameter unused
```

All declarations require either an explicit type or `auto` with an initializer. Semicolons are not used.

## Constants

Constants are immutable values that cannot be reassigned or modified after initialization:

```mux
// Top-level constants
const int MAX = 100
const float PI = 3.14159

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
        // self.MAX_RETRIES++  // ERROR: Cannot modify const field
    }
}

auto cfg = Config.new()
cfg.current_retry = 1  // OK - mutable field
// cfg.MAX_RETRIES = 5  // ERROR: Cannot assign to const field
```

### Const Enforcement

- Cannot reassign: `const_var = new_value` → ERROR
- Cannot use compound assignment: `const_var += 1` → ERROR
- Cannot increment/decrement: `const_var++` or `const_var--` → ERROR
- Applies to both identifiers and class fields
- Use `const` when you want a value that won't change after initialization

## When to Use `auto`

### Recommended

- Local variables with obvious initialization
- Complex generic types that are clear from context
- Temporary variables in calculations
- Iterator variables in loops

### Explicit Types Recommended

```mux
// Empty collections need explicit types
list<int> empty = []           // explicit type needed
auto empty = list<int>()       // or explicit constructor

// Uninitialized variables
Result<int, string> pending    // explicit type needed

// Function parameters (required)
func process(int data) returns void { }

// Generic instantiation when ambiguous
Stack<int> stack = Stack<int>.new()
```

## Using Underscore for Unused Values

The underscore `_` is a placeholder for values you don't need:

```mux
// Unused function parameters
func process(int data, string _) returns void { }

// Unused loop counters
for _ in range(0, 10) {
    doSomething()
}

// Unused pattern match values
match result {
    Ok(_) { print("Success") }  // don't care about the value
    Err(_) { print("Error") }   // don't care about the error details
}

// Destructuring with unused parts
for (key, _) in keyValuePairs {
    print("Key: " + key)  // value ignored
}
```

**Best Practice:** Use `_` when a value is required by syntax but not needed in your code. Don't overuse it when descriptive names would improve readability.

## Variable Scope

<!-- TODO: Add detailed scoping rules once finalized -->

Variables are scoped to the block in which they are declared:

```mux
func example() returns void {
    auto x = 10  // Scoped to function
    
    if x > 5 {
        auto y = 20  // Scoped to if block
        print(y.to_string())
    }
    
    // print(y.to_string())  // ERROR: y is out of scope
}
```

## See Also

- [Types](./types.md) - Type system and conversions
- [Functions](./functions.md) - Function declarations
- [Classes](./classes.md) - Class fields and constants
