# Statements

This document describes the semantics of each statement type in Mux.

## Block Statements

A block statement groups statements and creates a new scope.

```mux
{
    auto x = 1
    {
        auto y = 2  // Inner scope
        // x and y are accessible
    }
    // y is not accessible here
    // x is still accessible
}
```

### Scope Rules

- Each block creates a new lexical scope
- Variables declared in a block are only accessible within that block
- Nested blocks can access variables from enclosing scopes
- Variables are destroyed when their scope exits

## Variable Declaration Statements

### Syntax

```
variable_declaration ::= IDENT ':' type '=' expression
                       | IDENT '=' expression
```

### Rules

1. The type annotation is required unless using `auto`
2. The initializer expression is required
3. The variable is mutable by default

```mux
int x = 42           // Explicit type
auto y = 42          // Type inferred as int
auto name = "Mux"    // Type inferred as string
```

### Mutability

Variables are mutable by default:

```mux
auto count = 0
count = count + 1   // OK: reassignment
count++              // OK: increment
```

To make a variable immutable, use `const`:

```mux
const int MAX = 100
// MAX = 200  // ERROR: cannot reassign const
// MAX++      // ERROR: cannot modify const
```

## Constant Declaration Statements

### Syntax

```
const_declaration ::= 'const' type IDENT '=' expression
                    | 'const' IDENT '=' expression
```

### Rules

1. Constants must be initialized at declaration
2. Constants cannot be reassigned
3. Constants cannot be used with increment/decrement operators
4. Constants can be declared at:
   - Top-level (module scope)
   - Inside classes (class fields)
   - Inside functions (local constants)

```mux
// Module-level constant
const int MAX_RETRIES = 3

// Class constant
class Config {
    const int MAX_SIZE = 1024
}

// Local constant
func process() returns void {
    const float TAX_RATE = 0.08
    auto total = amount * TAX_RATE
}
```

## Expression Statements

Any expression can be used as a statement by placing it on its own line:

```mux
42                    // Integer literal (does nothing)
"hello"               // String literal (does nothing)
some_function()       // Function call with side effects
x++                   // Increment statement
y += 5                // Compound assignment statement
```

### Side Effects

Expression statements are useful when the expression has side effects:

```mux
print("Hello")        // Output function call
mutate_value()        // Function that modifies state
data.clear()          // Method that modifies the object
```

## If Statements

### Syntax

```
if_statement ::= 'if' expression block_statement
                | 'if' expression block_statement 'else' block_statement
                | 'if' expression block_statement { 'else' 'if' expression block_statement } [ 'else' block_statement ]
```

### Semantics

1. The condition expression must evaluate to `bool`
2. Only the matched branch is executed
3. Each branch creates a new scope

```mux
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}
```

### Scope Rules

Variables declared in one branch are not accessible in other branches:

```mux
if condition {
    auto x = 1
    // x is accessible here
}
// x is NOT accessible here
```

## Match Statements

### Syntax

```
match_statement ::= 'match' '(' expression ')' '{' { match_arm } '}'

match_arm ::= pattern [ 'if' expression ] '{' { statement } '}'
```

### Semantics

1. The match expression is evaluated
2. Each arm is checked in order
3. The first matching arm (with optional guard) is executed
4. Exhaustiveness is checked at compile time

### Patterns

| Pattern | Meaning | Example |
|---------|---------|---------|
| `_` | Wildcard (matches anything) | `_ { print("other") }` |
| `Variant(value)` | Destructuring enum | `Some(v) { print(v) }` |
| `Identifier` | Bind to variable | `Point(x, y) { ... }` |
| `value` | Literal match | `0 { print("zero") }` |

### Examples

```mux
enum Option<T> {
    Some(T)
    None
}

match maybeValue {
    Some(v) if v > 10 {
        print("large: " + v)
    }
    Some(v) {
        print("small: " + v)
    }
    None {
        print("no value")
    }
    _ {
        print("unexpected")
    }
}
```

### Exhaustiveness Checking

The compiler requires all cases to be covered:

```mux
enum Color { Red, Green, Blue }

auto c = Color.Red

match c {
    Red { print("red") }
    Green { print("green") }
    // ERROR: Blue not covered
}
```

### Guard Conditions

Arms can have additional `if` conditions:

```mux
match number {
    n if n < 0 { print("negative") }
    n if n == 0 { print("zero") }
    n { print("positive: " + n) }
}
```

## For Statements

### Syntax

```
for_statement ::= 'for' pattern 'in' expression block_statement
```

### Semantics

1. The iterable expression is evaluated
2. Each element is bound to the pattern
3. The body is executed for each element

### Iteration Patterns

```mux
// Iterate over list
for int item in myList {
    print(item)
}

// Ignore elements
for int _ in range(0, 10) {
    doSomething()
}

// With explicit typing
auto numbers = [1, 2, 3]
for int n in numbers {
    // n is explicitly typed as int
}
```

### Range Iteration

```mux
// range(start, end) - exclusive end
for int i in range(0, 5) {
    print(i)  // 0, 1, 2, 3, 4
}

// Negative step not supported
// Use while loop for complex iteration
```

### Nested Iteration

```mux
for int outer in outerList {
    for int inner in innerList {
        // Both outer and inner accessible
    }
}
```

## While Statements

### Syntax

```
while_statement ::= 'while' expression block_statement
```

### Semantics

1. The condition is evaluated
2. If true, the body is executed and the condition is checked again
3. If false, execution continues after the loop

```mux
auto count = 5
while count > 0 {
    print(count)
    count = count - 1
}
// count is 0 here
```

### Condition Requirements

The condition must evaluate to `bool`:

```mux
auto x = 1
while x {  // ERROR: cannot convert int to bool
    x--
}
```

## Break Statements

### Syntax

```
break_statement ::= 'break'
```

### Semantics

1. Exits the immediately enclosing loop
2. Execution continues after the loop

```mux
for int i in range(0, 100) {
    if i == 10 {
        break  // Exit loop when i reaches 10
    }
    print(i)
}
// Execution continues here
```

## Continue Statements

### Syntax

```
continue_statement ::= 'continue'
```

### Semantics

1. Skips to the next iteration of the immediately enclosing loop
2. The loop condition is checked again

```mux
for int i in range(0, 10) {
    if i % 2 == 0 {
        continue  // Skip even numbers
    }
    print(i)  // Prints odd numbers: 1, 3, 5, 7, 9
}
```

## Return Statements

### Syntax

```
return_statement ::= 'return' [ expression ]
```

### Semantics

1. Returns control from the current function
2. If a value is provided, it becomes the function's result
3. If no value is provided, the function must have return type `void`

### Value Return

```mux
func add(int a, int b) returns int {
    return a + b
}

func max(int a, int b) returns int {
    if a > b {
        return a
    }
    return b
}
```

### Void Return

```mux
func greet(string name) returns void {
    print("Hello, " + name)
    return  // Optional in void functions
}
```

### Early Returns

Returns can appear anywhere in a function:

```mux
func process(int value) returns int {
    if value < 0 {
        return 0  // Early return on invalid input
    }
    // Normal processing
    return value * 2
}
```

### Cleanup on Return

When a return executes:

1. All variables in current scope are cleaned up (RC decremented)
2. Nested scopes are cleaned up in reverse order
3. Control transfers to the caller

## Statement Evaluation Order

Statements are executed sequentially:

```mux
auto a = 1  // First
auto b = 2  // Second
auto c = a + b  // Third (a and b are already defined)
```

Within a single statement:

- Left operand evaluated before right operand
- Function arguments evaluated before function call
- Operands of `&&` and `||` may short-circuit (not evaluated)

## See Also

- [Grammar](./grammar.md) - Statement syntax
- [Expressions](./expressions.md) - Expression evaluation
- [Control Flow](../language-guide/control-flow.md) - Control flow guide
