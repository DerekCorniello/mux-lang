# Grammar

This document provides the formal grammar for Mux using a BNF-like notation.

## Notation

The following conventions are used:

| Notation | Meaning |
|----------|---------|
| `::=` | Definition |
| `\|` | Alternative (or) |
| `[...]` | Optional |
| `{...}` | Repetition (zero or more) |
| `'...'` | Literal token |
| `IDENT` | Identifier |
| `TYPE` | Type name |
| `expr` | Expression |
| `stmt` | Statement |

## Compilation Unit

```
compilation_unit ::= { import_declaration } { top_level_declaration }
```

## Imports

```
import_declaration ::= 'import' module_path [ 'as' IDENT ]

module_path ::= IDENT { '.' IDENT }
```

```mux
import math
import shapes.circle as circle
import foo.bar.baz
```

## Top-Level Declarations

```
top_level_declaration ::= function_declaration
                        | class_declaration
                        | interface_declaration
                        | enum_declaration
                        | const_declaration
```

## Functions

```
function_declaration ::= 'func' IDENT [ type_parameters ]
                         '(' [ formal_parameters ] ')'
                         'returns' type
                         block_statement

type_parameters ::= '<' IDENT { ',' IDENT } '>'

formal_parameters ::= parameter { ',' parameter }

parameter ::= [ IDENT ':' ] type
            | IDENT
            | '_' ':' type      // Unused parameter
```

```mux
func add(int a, int b) returns int {
    return a + b
}

func greet(string name, int times = 1) returns void {
    for i in range(0, times) {
        print("Hello, " + name)
    }
}

func identity<T>(T value) returns T {
    return value
}
```

## Classes

```
class_declaration ::= 'class' IDENT [ type_parameters ]
                      [ 'is' interface_list ]
                      '{' { class_member } '}'

interface_list ::= type_name { ',' type_name }

class_member ::= field_declaration
               | method_declaration
               | static_method_declaration

field_declaration ::= IDENT ':' type

method_declaration ::= 'func' IDENT '(' [ formal_parameters ] ')'
                       [ 'returns' type ]
                       block_statement

static_method_declaration ::= 'common' 'func' IDENT
                              '(' [ formal_parameters ] ')'
                              'returns' type
                              block_statement
```

```mux
class Circle is Drawable {
    float radius

    func area() returns float {
        const float PI = 3.14159
        return PI * radius * radius
    }

    common func from_diameter(float d) returns Circle {
        auto c = Circle.new()
        c.radius = d / 2.0
        return c
    }
}
```

## Interfaces

```
interface_declaration ::= 'interface' IDENT
                          '{' { interface_method } '}'

interface_method ::= 'func' IDENT '(' [ formal_parameters ] ')'
                     [ 'returns' type ]
```

```mux
interface Drawable {
    func draw() returns void
}

interface Add {
    func add(Self) returns Self
}
```

## Enums

```
enum_declaration ::= 'enum' IDENT [ type_parameters ]
                     '{' enum_variant { ',' enum_variant } '}'

enum_variant ::= IDENT [ '(' type_list ')' ]

type_list ::= type { ',' type }
```

```mux
enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

enum Option<T> {
    Some(T)
    None
}
```

## Constants

```
const_declaration ::= 'const' type IDENT '=' expression
                    | 'const' IDENT '=' expression
```

```mux
const int MAX_RETRIES = 3
const float PI = 3.14159
```

## Statements

```
block_statement ::= '{' { statement } '}'

statement ::= block_statement
            | variable_declaration
            | const_declaration
            | expression_statement
            | if_statement
            | match_statement
            | for_statement
            | while_statement
            | break_statement
            | continue_statement
            | return_statement
```

### Variable Declaration

```
variable_declaration ::= IDENT ':' type '=' expression
                       | IDENT '=' expression
```

```mux
int x = 42
auto name = "Mux"
list<string> names = ["Alice", "Bob"]
```

### Expression Statement

```
expression_statement ::= expression
```

### If Statement

```
if_statement ::= 'if' expression block_statement
                | 'if' expression block_statement 'else' block_statement
                | 'if' expression block_statement { 'else' 'if' expression block_statement } [ 'else' block_statement ]
```

```mux
if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}
```

### Match Statement

```
match_statement ::= 'match' '(' expression ')' '{' { match_arm } '}'

match_arm ::= pattern [ 'if' expression ] '{' { statement } '}'

pattern ::= '_'                                    // Wildcard
          | IDENT                                  // Variable pattern
          | IDENT '(' [ pattern { ',' pattern } ]' )  // Enum variant pattern
          | literal                                 // Literal pattern
```

```mux
match value {
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
        print("unknown")
    }
}
```

### For Statement

```
for_statement ::= 'for' pattern 'in' expression block_statement
```

```mux
for int item in myList {
    print(item)
}

for int _ in range(0, 10) {
    doSomething()
}
```

### While Statement

```
while_statement ::= 'while' expression block_statement
```

```mux
while count > 0 {
    print(count)
    count = count - 1
}
```

### Break Statement

```
break_statement ::= 'break'
```

### Continue Statement

```
continue_statement ::= 'continue'
```

### Return Statement

```
return_statement ::= 'return' [ expression ]
```

```mux
func add(int a, int b) returns int {
    return a + b
}

func void_func() returns void {
    return
}
```

## Expressions

```
expression ::= literal
             | identifier
             | lambda_expression
             | enum_instantiation
             | class_instantiation
             | method_call
             | field_access
             | array_access
             | unary_expression
             | binary_expression
             | range_expression
             | parenthesized_expression
```

### Literals

```
literal ::= integer_literal
          | float_literal
          | string_literal
          | char_literal
          | 'true'
          | 'false'
          | 'None'
          | list_literal
          | map_literal
          | set_literal
```

### List Literal

```
list_literal ::= '[' [ expression { ',' expression } ] ']'
```

```mux
auto nums = [1, 2, 3, 4, 5]
auto empty = []
auto mixed = [1, "two", 3.0]
```

### Map Literal

```
map_literal ::= '{' [ map_entry { ',' map_entry } ] '}'

map_entry ::= expression ':' expression
```

```mux
auto scores = {"Alice": 90, "Bob": 85}
auto nested = {"name": "Mux", "version": 1}
```

### Set Literal

```
set_literal ::= '{' expression { ',' expression } '}'
```

```mux
auto tags = {"urgent", "important", "review"}
```

### Lambda Expression

```
lambda_expression ::= 'func' '(' [ formal_parameters ] ')' [ 'returns' type ] block_statement
```

```mux
auto square = func(int n) returns int {
    return n * n
}

auto add = func(int a, int b) {
    return a + b
}
```

### Enum Instantiation

```
enum_instantiation ::= IDENT '.' 'new' '(' [ expression { ',' expression } ] ')'
                     | IDENT '<' type_list '>' '.' 'new' '(' [ expression { ',' expression } ] ')'
```

```mux
auto circle = Circle.new(5.0)
auto opt = Option<int>.new(42)
```

### Class Instantiation

```
class_instantiation ::= IDENT '.' 'new' '(' [ expression { ',' expression } ] ')'
                       | IDENT '<' type_list '>' '.' 'new' '(' [ expression { ',' expression } ] ')'
```

```mux
auto c = Circle.new(5.0)
auto stack = Stack<int>.new()
```

### Method Call

```
method_call ::= expression '.' IDENT '(' [ expression { ',' expression } ] ')'
```

```mux
auto str = "hello".to_upper()
numbers.push_back(42)
```

### Field Access

```
field_access ::= expression '.' IDENT
```

```mux
auto r = circle.radius
```

### Array/List Access

```
array_access ::= '[' expression ']'
```

```mux
auto first = myList[0]
```

### Parenthesized Expression

```
parenthesized_expression ::= '(' expression ')'
```

### Unary Operators

```
unary_expression ::= '-' expression
                   | '!' expression
                   | '&' expression
                   | '*' expression
```

### Binary Operators

```
binary_expression ::= expression '+' expression
                    | expression '-' expression
                    | expression '*' expression
                    | expression '/' expression
                    | expression '%' expression
                    | expression '**' expression
                    | expression '==' expression
                    | expression '!=' expression
                    | expression '<' expression
                    | expression '<=' expression
                    | expression '>' expression
                    | expression '>=' expression
                    | expression '&&' expression
                    | expression '||' expression
                    | expression '&' expression
                    | expression '|' expression
                    | expression '^' expression
                    | expression '<<' expression
                    | expression '>>' expression
                    | expression 'in' expression
                    | expression '+=' expression
                    | expression '-=' expression
                    | expression '*=' expression
                    | expression '/=' expression
                    | expression '%=' expression
```

### Range Expression

```
range_expression ::= 'range' '(' expression ',' expression ')'
```

```mux
for int i in range(0, 10) {
    print(i)
}
```

### Increment/Decrement

```
postfix_expression ::= expression '++'
                     | expression '--'
```

```mux
auto i = 0
i++
```

## Types

```
type ::= primitive_type
       | type_name
       | generic_type
       | optional_type
       | result_type
       | reference_type
       | function_type
       | list_type
       | map_type
       | set_type
```

### Primitive Types

```
primitive_type ::= 'int'
                 | 'float'
                 | 'bool'
                 | 'char'
                 | 'string'
                 | 'void'
```

### Generic Type

```
generic_type ::= type_name '<' type { ',' type } '>'
```

### Optional Type

```
optional_type ::= 'Optional' '<' type '>'
```

### Result Type

```
result_type ::= 'Result' '<' type ',' type '>'
```

### Reference Type

```
reference_type ::= '&' type
```

### List Type

```
list_type ::= 'list' '<' type '>'
```

### Map Type

```
map_type ::= 'map' '<' type ',' type '>'
```

### Set Type

```
set_type ::= 'set' '<' type '>'
```

### Function Type

```
function_type ::= 'func' '(' [ type { ',' type } ] ')' [ 'returns' type ]
```

## Type Parameters

```
type_parameter ::= IDENT [ 'is' type_constraint ]

type_constraint ::= type_name { '&' type_name }
```

## See Also

- [Lexical Structure](./lexical-structure.md) - Tokens and keywords
- [Type System](./type-system.md) - Type rules and inference
- [Operators](./operators.md) - Operator precedence
- [Language Guide](../language-guide/index.md) - Practical examples
