# Modules and Imports

Mux uses Python-style imports for code organization and reuse.

## Basic Import Syntax

```mux title="basic_import.mux"
import math
import shapes.circle
import utils.logger as log
import std.math
import std.datetime
```

- Python-style imports only
- Module paths map directly to file paths
- Imported symbols can be used immediately

## Standard Library Imports

The stdlib uses the `std` namespace:

```mux title="stdlib_imports.mux"
import std
import std.math
import std.io
import std.random
import std.datetime
import std.(math, random as r)
import std.*
```

- `import std` exposes module namespaces like `std.math`, `std.io`, and `std.datetime`
- `import std.<module>` imports a single stdlib module namespace
- `import std.*` performs a flat import of stdlib items into the current scope

## Module Resolution

Module paths map to file paths:

```mux title="module_resolution.mux"
import math          // math.mux in same directory
import shapes.circle // shapes/circle.mux
import lib.core.util // lib/core/util.mux
```

**File Structure:**
```
project/
├── main.mux
├── math.mux
├── shapes/
│   ├── circle.mux
│   └── rectangle.mux
└── lib/
    └── core/
        └── util.mux
```

## Using Imports

### Basic Usage

```mux title="math_module.mux"
// math.mux
func add(int a, int b) returns int {
    return a + b
}

func multiply(int a, int b) returns int {
    return a * b
}

const float PI = 3.14159
```

```mux title="main_import.mux"
// main.mux
import math

func main() returns void {
    auto sum = math.add(5, 3)
    auto product = math.multiply(4, 7)
    auto circumference = 2.0 * math.PI * 5.0
    
    print(sum.to_string())
    print(product.to_string())
}
```

### Aliased Imports

Use `as` to rename imported modules:

```mux title="aliased_imports.mux"
import shapes.circle as circle
import utils.logger as log

func main() returns void {
    auto c = circle.new(5.0)
    log.info("Created circle")
}
```

### Imports for Side Effects

Use `_` when importing only for side effects:

```mux title="side_effect_import.mux"
import utils.logger as _  // imported but not directly used
```

## Name Mangling

Functions from imported modules use mangled names to prevent conflicts:

```mux title="fibonacci_module.mux"
// math.mux
func fibonacci(int n) returns int {
    if n <= 1 {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}
```

```mux title="using_imported_func.mux"
// main.mux
import math

func main() returns void {
    auto result = math.fibonacci(10)  // Calls math_fibonacci
    print(result.to_string())
}
```

Generates LLVM function: `math_fibonacci` (not just `fibonacci`)

This prevents conflicts when multiple modules define functions with the same name.

## Module Initialization

Top-level statements in modules become initialization functions:

```mux title="module_init.mux"
// config.mux
const int MAX_USERS = 100
auto initialized = false

func initialize() returns void {
    initialized = true
    print("Config initialized")
}

// Top-level code runs on import
print("Loading config module...")
initialize()
```

The compiler:
1. Generates a module init function
2. Calls it before `main()` executes
3. Ensures each module initializes only once

## Dependency Resolution

The compiler processes modules in dependency order:

```mux title="module_a.mux"
// a.mux
import b

func useB() returns void {
    b.doSomething()
}
```

```mux title="module_b.mux"
// b.mux
import c

func doSomething() returns void {
    c.helper()
}
```

```mux title="module_c.mux"
// c.mux
func helper() returns void {
    print("helper called")
}
```

Initialization order: `c` -> `b` -> `a` -> `main`

## Module Scope

### Public by Default

All functions and types are public by default:

```mux title="public_module.mux"
// math.mux
func add(int a, int b) returns int {  // Public
    return a + b
}

const float PI = 3.14159  // Public
```

### Module-Level Variables

```mux title="module_variables.mux"
// config.mux
const string VERSION = "1.0.0"
auto request_count = 0

func increment_requests() returns void {
    request_count++
}

func get_request_count() returns int {
    return request_count
}
```

```mux title="using_module_vars.mux"
// main.mux
import config

func main() returns void {
    print("Version: " + config.VERSION)
    config.increment_requests()
    print(config.get_request_count().to_string())
}
```

## Circular Dependencies

**Warning:** Avoid circular imports:

```mux title="circular_a.mux"
// a.mux
import b  // ERROR: a imports b, b imports a

func useB() returns void {
    b.doSomething()
}
```

```mux title="circular_b.mux"
// b.mux
import a  // ERROR: circular dependency

func doSomething() returns void {
    a.useB()
}
```

**Solution:** Restructure to remove circular dependencies or extract shared code to a third module.

## Common Module Patterns

### Utility Module

```mux title="utility_module.mux"
// utils.mux
func max(int a, int b) returns int {
    if a > b {
        return a
    }
    return b
}

func min(int a, int b) returns int {
    if a < b {
        return a
    }
    return b
}

func clamp(int value, int low, int high) returns int {
    if value < low {
        return low
    }
    if value > high {
        return high
    }
    return value
}
```

### Type Module

```mux title="type_module.mux"
// types.mux
class Point {
    int x
    int y
    
    func distance_to(Point other) returns float {
        auto dx = self.x - other.x
        auto dy = self.y - other.y
        auto sum = dx * dx + dy * dy
        return sum.to_float()  // Simplified, needs sqrt
    }
}

enum Color {
    Red
    Green
    Blue
}
```

### Constants Module

```mux title="constants_module.mux"
// constants.mux
const int MAX_CONNECTIONS = 100
const float TIMEOUT_SECONDS = 30.0
const string API_VERSION = "v2"
const bool DEBUG_MODE = true
```

## Module Organization

### Flat Structure

For small projects:

```
project/
├── main.mux
├── math.mux
├── utils.mux
└── constants.mux
```

### Hierarchical Structure

For larger projects:

```
project/
├── main.mux
├── lib/
│   ├── core/
│   │   ├── math.mux
│   │   └── utils.mux
│   ├── data/
│   │   ├── parser.mux
│   │   └── validator.mux
│   └── net/
│       ├── http.mux
│       └── tcp.mux
└── config/
    └── settings.mux
```

## Import Best Practices

1. **Import at the top** - All imports at file beginning
2. **Avoid circular dependencies** - Restructure if needed
3. **Use descriptive module names** - `math` not `m`
4. **Alias long names** - `import very.long.module.path as path`
5. **Group related functions** - Put related utilities in same module
6. **One module per file** - Keep file structure simple
7. **Document public APIs** - Comment exported functions
8. **Avoid deep nesting** - Keep module hierarchy shallow

## Example Project

```
calculator/
├── main.mux
├── operations/
│   ├── basic.mux
│   └── advanced.mux
└── utils/
    └── format.mux
```

```mux title="basic_operations.mux"
// operations/basic.mux
func add(int a, int b) returns int {
    return a + b
}

func subtract(int a, int b) returns int {
    return a - b
}
```

```mux title="advanced_operations.mux"
// operations/advanced.mux
func power(int base, int exp) returns int {
    auto result = 1
    for _ in range(0, exp) {
        result = result * base
    }
    return result
}
```

```mux title="format_module.mux"
// utils/format.mux
func format_result(int value) returns string {
    return "Result: " + value.to_string()
}
```

```mux title="calculator_main.mux"
// main.mux
import operations.basic as basic
import operations.advanced as advanced
import utils.format as fmt

func main() returns void {
    auto sum = basic.add(5, 3)
    print(fmt.format_result(sum))
    
    auto powered = advanced.power(2, 10)
    print(fmt.format_result(powered))
}
```

## Technical Implementation

### Module Compilation

1. **Parse imports** - Extract all import statements
2. **Build dependency graph** - Determine module order
3. **Process modules** - Parse and analyze in topological order
4. **Generate init functions** - Create module initialization code
5. **Link modules** - Combine into final executable

### Name Mangling Details

Module functions are prefixed with their module path:

```mux title="shapes_circle.mux"
// shapes/circle.mux
func area(float radius) returns float {
    return 3.14159 * radius * radius
}
```

Generates: `shapes_circle_area` (module path becomes prefix)

## See Also

- [Functions](./functions.md) - Function definitions
- [Classes](./classes.md) - Type definitions
- [Variables](./variables.md) - Module-level constants and variables
