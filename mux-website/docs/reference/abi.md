# ABI (Application Binary Interface)

This document describes the binary interface for Mux, including calling conventions, type representations, and interoperability.

## Warning

The ABI is not yet stable. This document describes the current implementation, which may change.

## Calling Convention

Mux uses the System V AMD64 ABI on Linux/x86-64, with the following modifications:

### Argument Passing

| Type | Passing Method |
|------|----------------|
| `int` | Register (RDI, RSI, RDX, RCX, R8, R9) or stack |
| `float` | Register (XMM0-XMM7) or stack |
| `bool` | Register (as 8-bit value) |
| `char` | Register (as 8-bit value) |
| `string` | Register (pointer to struct) |
| `list&lt;T>`, `map&lt;K,V>`, `set&lt;T>` | Register (pointer to struct) |
| `Optional&lt;T>`, `Result&lt;T,E>` | Register (pointer to struct) |
| Class instances | Register (pointer to object) |
| References (`&T`) | Register (pointer) |

### Return Values

| Type | Return Method |
|------|----------------|
| `void` | No return value |
| `int`, `float`, `bool`, `char` | Register (RAX or XMM0) |
| `string` | Pointer in RAX, length in RDX |
| `list&lt;T>`, `map&lt;K,V>`, `set&lt;T>` | Pointer to struct in RAX |
| `Optional&lt;T>`, `Result&lt;T,E>` | Pointer to struct in RAX |
| Class instances | Pointer to object in RAX |
| References (`&T`) | Pointer in RAX |

## Type Representations

### Primitive Types

| Mux Type | C Type | Size | Alignment |
|----------|--------|------|-----------|
| `void` | void | 0 | 1 |
| `int` | int64_t | 8 | 8 |
| `float` | double | 8 | 8 |
| `bool` | uint8_t | 1 | 1 |
| `char` | uint32_t | 4 | 4 |
| `string` | struct \{ void* ptr; int64_t len; \} | 16 | 8 |

### Composite Types

#### Optional&lt;T&gt;

```rust
struct Optional<T> {
    int32_t discriminant;  // 0 = None, 1 = Some
    void* data;           // Pointer to T
}
```

#### Result&lt;T, E&gt;

```rust
struct Result<T, E> {
    int32_t discriminant;  // 0 = Ok, 1 = Err
    void* data;           // Pointer to T or E
}
```

#### list&lt;T&gt;

```rust
struct list<T> {
    void** data;      // Pointer to array of Value*
    int64_t size;     // Number of elements
    int64_t capacity;  // Allocated capacity
}
```

#### map&lt;K, V&gt;

```rust
struct map<K, V> {
    void* tree;       // Pointer to BTreeMap
}
```

#### set&lt;T&gt;

```rust
struct set<T> {
    void* tree;       // Pointer to BTreeSet
}
```

## Value Representation

All runtime values are represented using the `Value` enum:

```rust
enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Value>),
    Map(BTreeMap<Value, Value>),
    Set(BTreeSet<Value>),
    Optional(Option<Box<Value>>),
    Result(Result<Box<Value>, String>),
    Object(ObjectRef),
}
```

### Boxed Values

Primitive values are boxed into `*mut Value` pointers:

```rust
┌──────────────────┬─────────────┐
│   RefHeader      │    Value    │
│ ref_count: u64   │  discriminator + data |
└──────────────────┴─────────────┘
```

## Name Mangling

### Function Names

Functions are mangled to encode their module and signature:

```
module_functionName$paramType1$paramType2$...
```

### Examples

```mux
// math.mux
func fibonacci(int n) returns int { ... }
// Mangled: math_fibonacci$$int

// main.mux
func add(int a, int b) returns int { ... }
// Mangled: main_add$$int$$int

// Generic functions
func identity<T>(T value) returns T { ... }
// identity$$int
// identity$$string
```

### Operator Overloads

Operator methods use the interface name:

```mux
class Point {
    func add(Point other) returns Point { ... }
}
// Mangled: Point_add$Point
```

## Runtime Functions

The Mux runtime provides the following functions for FFI:

### Memory Management

```c
void* mux_rc_alloc(Value value);
void mux_rc_inc(void* ptr);
bool mux_rc_dec(void* ptr);
```

### Value Operations

```c
int64_t mux_value_get_int(void* ptr);
void mux_value_set_int(void* ptr, int64_t value);
double mux_value_get_float(void* ptr);
void mux_value_set_float(void* ptr, double value);
const char* mux_value_get_string(void* ptr);
// ... and more
```

### String Operations

```c
void* mux_string_create(const char* str);
void* mux_string_concat(void* a, void* b);
int64_t mux_string_length(void* ptr);
```

### Collection Operations

```c
void* mux_list_create();
void mux_list_push(void* list, void* value);
void* mux_list_pop(void* list);
void* mux_list_get(void* list, int64_t index);
// ... and more
```

## Symbol Names

### Exported Symbols

| Symbol | Purpose |
|--------|---------|
| `mux_main` | Program entry point |
| `mux_init_module_<name>` | Module initialization |
| `mux_rc_alloc` | Allocate RC value |
| `mux_rc_inc` | Increment reference count |
| `mux_rc_dec` | Decrement reference count |
| `mux_print` | Print string to stdout |

### Object File Format

- Object files: ELF64 (Linux), Mach-O (macOS), COFF (Windows)
- Final executable: Platform-native format

## C Interoperability

### Declaring C Functions

Mux can call C functions through runtime binding:

```c
// C header (example)
int c_add(int a, int b);
```

### Limitations

- Limited FFI support in current version
- C interop requires manual binding
- Type marshaling is manual

## Future ABI Stability

The ABI will stabilize before version 1.0. Until then:

- No guarantees about binary compatibility
- Recompilation required between versions
- Internal representations may change

## See Also

- [Memory Model](./memory-model.md) - Memory layout details
- [Type System](./type-system.md) - Type representations
- [Source Code](https://github.com/derekcorniello/mux-lang) - Compiler implementation
