# Operators

This document describes all operators in Mux, including their precedence, associativity, and behavior.

## Operator Precedence

Higher precedence operators are evaluated first.

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 (highest) | `.` (member access), `()`, `[]` | Left-to-right |
| 2 | `++`, `--` (postfix) | Left-to-right |
| 3 | `**` | Right-to-left |
| 4 | `+`, `-` (unary), `!`, `&`, `*` | Right-to-left |
| 5 | `*`, `/`, `%` | Left-to-right |
| 6 | `+`, `-` (binary) | Left-to-right |
| 7 | `<<`, `>>` | Left-to-right |
| 8 | `&` | Left-to-right |
| 9 | `^` | Left-to-right |
| 10 | `\|` | Left-to-right |
| 11 | `<`, `<=`, `>`, `>=` | Left-to-right |
| 12 | `==`, `!=` | Left-to-right |
| 13 | `&&` | Left-to-right |
| 14 | `\|\|` | Left-to-right |
| 15 | `in` | Left-to-right |
| 16 | `=` | Right-to-left |
| 17 | `++`, `--` (standalone statement) | N/A |

## Arithmetic Operators

### Binary Arithmetic

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `+` | Addition | `int`, `float`, `string` | `5 + 3`, `"a" + "b"` |
| `-` | Subtraction | `int`, `float` | `10 - 4` |
| `*` | Multiplication | `int`, `float` | `6 * 7` |
| `/` | Division | `int`, `float` | `15 / 3` |
| `%` | Modulo | `int`, `float` | `10 % 3` (result: 1) |
| `**` | Exponentiation | `int`, `float` | `2 ** 3` (result: 8) |

### Unary Arithmetic

| Operator | Description | Types | Example |
|----------|-------------|-------|---------|
| `+` | Unary plus | `int`, `float` | `+5` (identity) |
| `-` | Negation | `int`, `float` | `-5` (negation) |

### Arithmetic Rules

- All arithmetic operators require both operands to have the same type
- No implicit type conversion between `int` and `float`
- Division by zero is a runtime error for `int`, returns `inf` for `float`
- Modulo with negative numbers follows C semantics

### Exponentiation Details

```mux
auto squared = 5 ** 2        // 25
auto cubed = 2.0 ** 3.0      // 8.0
auto combined = 2 ** 3 ** 2  // 512 (right-associative: 2 ** (3 ** 2))
auto precedence = 2 * 3 ** 2 // 18 (higher than *)
```

**Properties:**
- Right-associative: `a ** b ** c` = `a ** (b ** c)`
- Higher precedence than `*`, `/`, `%`

## Increment and Decrement

| Operator | Description | Restrictions |
|----------|-------------|--------------|
| `++` | Postfix increment | `int` only, standalone statement |
| `--` | Postfix decrement | `int` only, standalone statement |

### Usage Rules

```mux
auto counter = 0
counter++         // Valid: counter is now 1
counter--         // Valid: counter is now 0

// ERROR: cannot use in expressions
// auto x = counter++    // ERROR
// auto y = (counter++) + 5  // ERROR

// ERROR: prefix not supported
// ++counter            // ERROR

// ERROR: cannot modify const
const int MAX = 100
// MAX++               // ERROR
```

### Rationale

The postfix-only, standalone-only design prevents ambiguity and side-effect confusion that can occur with prefix operators or expression-embedded increments.

## Comparison Operators

| Operator | Description | Types |
|----------|-------------|-------|
| `==` | Equality | All types |
| `!=` | Inequality | All types |
| `<` | Less than | `int`, `float`, `string` |
| `<=` | Less than or equal | `int`, `float`, `string` |
| `>` | Greater than | `int`, `float`, `string` |
| `>=` | Greater than or equal | `int`, `float`, `string` |

### Comparison Rules

- Both operands must have the same type
- No implicit conversion between numeric types
- String comparison is lexicographic (Unicode codepoint order)

```mux
auto a = 1 == 1           // true
auto b = 1 == "1"         // ERROR: different types
auto c = "abc" < "abd"    // true
```

## Logical Operators

| Operator | Description | Behavior |
|----------|-------------|----------|
| `&&` | Logical AND | Short-circuit evaluation |
| `\|\|` | Logical OR | Short-circuit evaluation |
| `!` | Logical NOT | Unary negation |

### Short-Circuit Evaluation

```mux
if x > 0 && y > 0 {      // y is only evaluated if x > 0
    // ...
}

if x == 0 || y == 0 {    // y is only evaluated if x != 0
    // ...
}
```

### Implementation

The `&&` and `||` operators use LLVM control flow for short-circuit evaluation:

```
For `a && b`:
1. Evaluate a
2. If a is false, return false (b not evaluated)
3. If a is true, evaluate b and return result
```

Phi nodes merge results from different branches.

## Bitwise Operators

| Operator | Description | Types |
|----------|-------------|-------|
| `&` | Bitwise AND | `int` |
| `\|` | Bitwise OR | `int` |
| `^` | Bitwise XOR | `int` |
| `<<` | Left shift | `int` |
| `>>` | Right shift | `int` |

```mux
auto a = 5 & 3            // 1 (0101 & 0011 = 0001)
auto b = 5 | 3            // 7 (0101 | 0011 = 0111)
auto c = 5 ^ 3            // 6 (0101 ^ 0011 = 0110)
auto d = 5 << 1           // 10 (0101 << 1 = 1010)
auto e = 5 >> 1           // 2 (0101 >> 1 = 0010)
```

## Membership Operator

| Operator | Description | Types |
|----------|-------------|-------|
| `in` | Membership test | `T in list<T>`, `T in set<T>`, `string in string`, `char in string` |

```mux
// List containment
auto nums = [1, 2, 3, 4, 5]
auto hasThree = 3 in nums     // true

// Set containment
auto tags = {"urgent", "important"}
auto isUrgent = "urgent" in tags    // true

// String containment
auto msg = "hello world"
auto hasWorld = "world" in msg      // true
auto hasO = 'o' in msg              // true
```

## Collection Operators

### Concatenation with `+`

| Types | Operation | Result |
|-------|-----------|--------|
| `list<T> + list<T>` | Concatenation | Combined list |
| `map<K,V> + map<K,V>` | Merge | Combined map |
| `set<T> + set<T>` | Union | Set with all elements |
| `string + string` | Concatenation | Combined string |

```mux
auto list1 = [1, 2]
auto list2 = [3, 4]
auto combined = list1 + list2    // [1, 2, 3, 4]

auto map1 = {"a": 1, "b": 2}
auto map2 = {"b": 3, "c": 4}
auto merged = map1 + map2        // {"a": 1, "b": 3, "c": 4}

auto set1 = {1, 2, 3}
auto set2 = {3, 4, 5}
auto unioned = set1 + set2       // {1, 2, 3, 4, 5}
```

## Compound Assignment Operators

| Operator | Expansion | Types |
|----------|-----------|-------|
| `+=` | `a = a + b` | `int`, `float`, `string` |
| `-=` | `a = a - b` | `int`, `float` |
| `*=` | `a = a * b` | `int`, `float` |
| `/=` | `a = a / b` | `int`, `float` |
| `%=` | `a = a % b` | `int`, `float` |

```mux
auto x = 5
x += 3        // x = x + 3 = 8
x *= 2        // x = x * 2 = 16
```

## Reference Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Create reference | `auto r = &x` |
| `*` | Dereference | `*r = 42` |

```mux
int x = 10
auto r = &x        // r: &int
*r = 20            // x is now 20
```

## Operator Overloading

Operators map to interface methods:

| Operator | Interface | Method |
|----------|-----------|--------|
| `+` | `Add` | `add(Self) -> Self` |
| `-` | `Sub` | `sub(Self) -> Self` |
| `*` | `Mul` | `mul(Self) -> Self` |
| `/` | `Div` | `div(Self) -> Self` |
| `==` | `Equatable` | `eq(Self) -> bool` |
| `<` | `Comparable` | `cmp(Self) -> int` |

### Custom Operator Implementation

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

auto p1 = Point.new(1, 2)
auto p2 = Point.new(3, 4)
auto p3 = p1 + p2  // Point(4, 6)
```

## Operator Semantics

### Operand Evaluation Order

1. **Left operand** is always evaluated first
2. **Right operand** is evaluated before the operation
3. For short-circuit operators, the right operand may not be evaluated

### Overflow and Underflow

- `int` overflow wraps around (modular arithmetic)
- `float` overflow produces `inf` or `-inf`
- `float` underflow produces denormalized numbers or `0.0`

### Division by Zero

- `int / 0`: Runtime error
- `float / 0`: Returns `inf` or `-inf`

## See Also

- [Type System](./type-system.md) - Type requirements for operators
- [Grammar](./grammar.md) - Operator syntax
- [Expressions](./expressions.md) - Expression evaluation rules
