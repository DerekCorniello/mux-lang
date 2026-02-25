# Operators

Mux provides a comprehensive set of operators for arithmetic, comparison, logical operations, and more.

## Arithmetic Operators

Standard arithmetic operations with strict type requirements:

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `+` | Addition | `int`, `float`, `string` | `5 + 3` -> `8` |
| `-` | Subtraction | `int`, `float` | `10 - 4` -> `6` |
| `*` | Multiplication | `int`, `float` | `6 * 7` -> `42` |
| `/` | Division | `int`, `float` | `15 / 3` -> `5` |
| `%` | Modulo | `int`, `float` | `10 % 3` -> `1` |
| `**` | Exponentiation | `int`, `float` | `2 ** 3` -> `8` |

### Examples

```mux title="arithmetic_ops.mux"
// Integer arithmetic
auto sum = 5 + 3        // 8
auto diff = 10 - 4      // 6
auto prod = 6 * 7       // 42
auto quot = 15 / 3      // 5
auto rem = 10 % 3       // 1
auto pow = 2 ** 3       // 8

// Float arithmetic
auto fsum = 5.0 + 3.2   // 8.2
auto fdiff = 10.5 - 4.3 // 6.2
auto fprod = 2.5 * 4.0  // 10.0
auto fquot = 7.5 / 2.5  // 3.0

// String concatenation
auto greeting = "Hello, " + "World"  // "Hello, World"
auto name = "Age: " + (30).to_string()  // "Age: 30"
```

### Exponentiation (`**`)

- **Right-associative**: `2 ** 3 ** 2` equals `2 ** (3 ** 2)` = 512
- **Higher precedence than `*` and `/`**: `2 * 3 ** 2` equals `2 * 9` = 18
- Works on both `int` and `float` types

```mux title="exponentiation.mux"
auto squared = 5 ** 2        // 25
auto cubed = 2.0 ** 3.0      // 8.0
auto complex = 2 ** 3 ** 2   // 512 (right-associative)
```

### Type Constraints

As previously stated, **No implicit conversions**, operands must be the same type:

```mux title="type_constraints.mux"
// ERROR: Type mismatches
// auto bad1 = 1 + 1.0        // cannot add int and float
// auto bad2 = "hello" + 3    // cannot add string and int

// Correct: Explicit conversion
auto good1 = 1 + (1.0).to_int()           // 2
auto good2 = "hello" + (3).to_string()    // "hello3"
auto good3 = (1).to_float() + 1.0         // 2.0
```

## Increment and Decrement

Postfix-only operators for incrementing/decrementing:

| Operator | Description | Example |
|----------|-------------|---------|
| `++` | Increment (postfix only) | `counter++` |
| `--` | Decrement (postfix only) | `counter--` |

### Design Constraints

- **Postfix only**: `counter++` is valid, `++counter` is NOT supported
- **Standalone only**: Must appear on their own line, not within expressions
- **Only on mutable variables**: Cannot be used on `const` or literals
- **Type preservation**: Operates on `int` types only

```mux title="increment_decrement.mux"
auto counter = 0
counter++         // Valid: counter is now 1
counter--         // Valid: counter is now 0

// INVALID - cannot use in expressions:
// auto x = counter++     // ERROR: ++ cannot be used in expressions
// auto y = (counter++) + 5  // ERROR: standalone only

// INVALID - prefix not supported:
// ++counter              // ERROR: prefix increment not supported

// INVALID - cannot modify const:
const int MAX = 100
// MAX++                 // ERROR: cannot modify const
```

**Rationale:** The postfix-only, standalone-only design prevents ambiguity and side-effect confusion.

## Comparison Operators

Compare values of compatible types:

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `==` | Equality | All comparable types | `a == b` |
| `!=` | Inequality | All comparable types | `a != b` |
| `<` | Less than | `int`, `float`, `string` | `5 < 10` |
| `<=` | Less than or equal | `int`, `float`, `string` | `x <= 100` |
| `>` | Greater than | `int`, `float`, `string` | `y > 0` |
| `>=` | Greater than or equal | `int`, `float`, `string` | `age >= 18` |

### Examples

```mux title="comparison_ops.mux"
// Numeric comparisons
auto isEqual = 5 == 5       // true
auto isNotEqual = 5 != 3    // true
auto isLess = 3 < 10        // true
auto isGreater = 10 > 3     // true

// String comparisons
auto strEq = "hello" == "hello"     // true
auto strLess = "apple" < "banana"   // true (lexicographic)

// Boolean comparisons
auto boolEq = true == true          // true
auto boolNeq = true != false        // true
```

### Type Constraints

Both operands must have the same type:

```mux title="comparison_types.mux"
// ERROR: Type mismatches
// auto bad1 = 1 < 1.0        // cannot compare int and float
// auto bad2 = "a" == 1       // cannot compare string and int

// Correct: Same types
auto good1 = 1 < 2              // true
auto good2 = "a" == "a"         // true
auto good3 = (1).to_float() < 1.0   // true
```

## Logical Operators

Boolean operations with short-circuit evaluation:

| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Logical AND (short-circuit) | `a && b` |
| `\|\|` | Logical OR (short-circuit) | `a \|\| b` |
| `!` | Logical NOT | `!flag` |

### Short-Circuit Evaluation

- `&&` only evaluates right side if left is `true`
- `||` only evaluates right side if left is `false`

```mux title="short_circuit.mux"
// AND short-circuit
auto result1 = false && expensiveCheck()  // expensiveCheck() not called

// OR short-circuit
auto result2 = true || expensiveCheck()   // expensiveCheck() not called

// Chaining
auto valid = x > 0 && x < 100 && isPrime(x)

// Negation
auto isInvalid = !valid
```

### How Short-Circuiting Works

Mux uses LLVM control flow for short-circuit evaluation:

```mux title="short_circuit_codegen.mux"
auto result = a && b
```

Generates:
1. Evaluate `a`
2. If `a` is false, result is false (skip `b`)
3. If `a` is true, evaluate `b` and use its value
4. Phi node merges results from different paths

This enables:
- Performance optimization (skip unnecessary checks)
- Safe null/bounds checking patterns
- Branch prediction opportunities

## Membership Operator (`in`)

Test for membership/containment:

| Left Operand | Right Operand | Description |
|--------------|---------------|-------------|
| `T` | `list<T>` | Check if value exists in list |
| `T` | `set<T>` | Check if value exists in set |
| `string` | `string` | Check if substring exists |
| `char` | `string` | Check if character exists in string |

### Type Constraints

Both operands must have compatible element types:

```mux title="membership_types.mux"
// ERROR: Type mismatch
// auto bad = "1" in nums           // string not in list<int>
// auto bad2 = 1 in msg             // int not in string
```

## Collection Operators

### Concatenation with `+`

The `+` operator is overloaded for collection types:

| Types | Operation | Result |
|-------|-----------|--------|
| `list<T> + list<T>` | Concatenation | Combined list |
| `map<K,V> + map<K,V>` | Merge | Combined map (latter overwrites on collision) |
| `set<T> + set<T>` | Union | Set with all unique elements |
| `string + string` | Concatenation | Combined string |

### Examples

```mux title="collection_ops.mux"
// List concatenation
auto list1 = [1, 2]
auto list2 = [3, 4]
auto combined = list1 + list2    // [1, 2, 3, 4]

// Map merge (latter wins on key collision)
auto map1 = {"a": 1, "b": 2}
auto map2 = {"b": 3, "c": 4}
auto merged = map1 + map2        // {"a": 1, "b": 3, "c": 4}

// Set union
auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}

// String concatenation
auto greeting = "Hello, " + "World"  // "Hello, World"
```

### Type Constraints

Collections must be the exact same type:

```mux title="collection_types.mux"
// ERROR: Type mismatch
// auto bad = [1, 2] + {3, 4}       // cannot add list and set
// auto bad2 = [1, 2] + [3.0, 4.0]  // list<int> + list<float>
```

## Reference Operators

Create and dereference references:

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Create reference | `&variable` |
| `*` | Dereference | `*reference` |

### Examples

```mux title="reference_ops.mux"
// Create reference
int x = 10
auto r = &x      // r is of type &int

// Dereference for reading
auto value = *r  // 10

// Dereference for writing
*r = 20          // Changes x to 20

// Function with reference parameter
func update(&int ref) returns void {
    *ref = *ref + 1
}

update(&x)
print(x.to_string())  // "21"
```

## Operator Precedence

From highest to lowest precedence:

1. **Unary**: `!`, `*` (dereference), `&` (reference)
2. **Exponentiation**: `**` (right-associative)
3. **Multiplicative**: `*`, `/`, `%`
4. **Additive**: `+`, `-`
5. **Comparison**: `<`, `<=`, `>`, `>=`
6. **Equality**: `==`, `!=`
7. **Logical AND**: `&&`
8. **Logical OR**: `||`

### Examples

```mux title="precedence_example.mux"
auto result1 = 2 + 3 * 4        // 14 (not 20)
auto result2 = 2 * 3 ** 2       // 18 (not 36)
auto result3 = 10 - 5 - 2       // 3 (left-to-right)
auto result4 = 2 ** 3 ** 2      // 512 (right-associative)

// Use parentheses for clarity
auto result5 = (2 + 3) * 4      // 20
auto result6 = 2 * (3 ** 2)     // 18
```

## Operator Overloading

Operators are built-in for primitive types. Custom types can implement interfaces for equality and comparison:

| Operator | Interface |
|----------|-----------|
| `==`, `!=` | `Equatable` |
| `<`, `>`, `<=`, `>=` | `Comparable` |

### Custom Type Example

```mux title="operator_overloading.mux"
interface Equatable {
    func eq(Self) returns bool
}

class Point is Equatable {
    int x
    int y
    
    func eq(Point other) returns bool {
        return self.x == other.x && self.y == other.y
    }
}

// Now == works with Point
auto p1 = Point.new()
p1.x = 1
p1.y = 2

auto p2 = Point.new()
p2.x = 1
p2.y = 2

auto same = p1.eq(p2)  // true
```

## Assignment Operators

Simple assignment (no compound assignment):

| Operator | Description | Example |
|----------|-------------|---------|
| `=` | Assignment | `x = 10` |

**Note:** Mux does NOT support compound assignment operators like `+=`, `-=`, etc.

```mux title="assignment_ops.mux"
auto x = 10
x = x + 5     // Must write explicitly
// x += 5     // ERROR: Not supported

auto count = 0
count = count + 1  // Explicit increment
count++            // Or use postfix ++
```

## Best Practices

1. **Use parentheses for clarity** - Don't rely on precedence rules
2. **Explicit type conversions** - No implicit conversions allowed
3. **Short-circuit for efficiency** - Use `&&` and `||` wisely
4. **Use `in` for membership** - Cleaner than calling methods
5. **Prefer `++`/`--` on separate lines** - Clearer than expression-embedded
6. **Leverage operator overloading** - Implement interfaces for custom types
7. **Understand right-associativity of `**`** - Use parentheses if unsure

## See Also

- [Types](./types.md) - Type conversions and constraints
- [Generics](./generics.md) - Built-in interfaces for operators
- [Collections](./collections.md) - Collection operators
- [Functions](./functions.md) - Operator overloading with interfaces
