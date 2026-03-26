# AI Guide to Mux

A concise reference sheet for LLMs to understand Mux and help users effectively.

## Key Language Differences

### Static & Strict Typing
- **No implicit conversions** – all type conversions must be explicit (`.to_int()`, `.to_float()`, `.to_string()`).
- **No `null`** – use `optional<T>` or `none`.
- **No dynamic typing** – all types resolved at compile time.
- **No runtime reflection** – static dispatch for interfaces (no vtables).

### Memory Management
- **Reference‑counted** – automatic memory safety, no garbage collector, no manual `free`.
- **Boxed values** – all primitives are boxed into `*mut Value` pointers (transparent to users).

### Error Handling
- **`result<T, E>`** and **`optional<T>`** – explicit error handling with pattern matching.
- **No exceptions** – errors are values that must be handled.

### Generics & Polymorphism
- **Monomorphized generics** – zero‑cost, static dispatch.
- **Interfaces** – declared with `is`, implemented by classes; static dispatch (no vtables).
- **No trait objects** – cannot have heterogeneous collections of interfaces.

## Syntax Highlights

### Variables & Types
```mux
int x = 5
auto y = 3.14      // type inference
const string NAME = "Mux"
```

### Functions
```mux
func add(int a, int b) returns int { return a + b }
func greet(string name, int times = 1) returns void { ... }
```

### Classes & Interfaces
```mux
interface Drawable { func draw() returns void }
class Circle is Drawable {
    float r
    func draw() returns void { ... }
    common func from_radius(float r) returns Circle { ... }
}
auto c = Circle.new(5.0)
```

### Pattern Matching
```mux
match value {
    ok(val) { print(val.to_string()) }
    err(msg) { print("Error: " + msg) }
    none { print("No value") }
    42 { print("The answer") }
    _ { print("Wildcard") }
}
```

### Collections
```mux
list<int> nums = [1, 2, 3]
map<string, int> ages = {"Alice": 30, "Bob": 25}
set<string> tags = {"rust", "llvm"}
tuple<int, string> pair = (1, "one")
```

### Control Flow
```mux
if x > 0 { ... } else if x < 0 { ... } else { ... }
while condition { ... }
for int item in list { ... }
for (int i = 0; i < 10; i++) { ... }   // C‑style loop
```

## Standard Library Quick Reference

| Module | Purpose |
|--------|---------|
| `std.assert` | Test assertions (`assert_eq`, `assert_true`) |
| `std.math` | Math functions (`sqrt`, `sin`, `cos`, `pow`, `min`, `max`) |
| `std.io` | File I/O (`read_file`, `write_file`, `exists`) |
| `std.random` | Pseudorandom numbers (`next_int`, `next_range`) |
| `std.datetime` | Timestamps (`now`, `year`, `month`, `format`) |
| `std.sync` | Concurrency (`spawn`, `sleep`, `Mutex`, `RwLock`) |
| `std.net` | Networking (`TcpStream`, `UdpSocket`, `http.request`) |
| `std.env` | Environment variables (`env.get`) |
| `std.data.json` | JSON parsing (`parse`, `to_map`) |
| `std.data.csv` | CSV parsing |
| `std.sql` | Database connectivity (SQLite, PostgreSQL, MySQL) |
| `std.dsa` | Data structures (`stack`, `queue`, `heap`, `bintree`, `graph`, `sort`) |

## Common Patterns

- **Safe collection access** – `.get()` returns `optional<T>`, then pattern‑match.
- **Factory methods** – `common func from(…) returns ClassName`.
- **Unused parameters** – declare with `_` (e.g., `func cb(string event, int _)`).
- **Type inference** – `auto` for local variables, explicit types for function parameters and fields.
- **Range iteration** – built‑in `range(int start, int end)` function.
- **String concatenation** – `+` operator, but no implicit conversion of other types.

## Pitfalls & Constraints

- **No operator overloading** for user‑defined types.
- **No arbitrary‑precision integers** – `int` is 64‑bit signed.
- **No mutable references to immutable data** – references are mutable by default.
- **No prefix increment/decrement** – only postfix (`x++`, `x--`) and as standalone statements.
- **Generics are monomorphized** – each type instantiation creates a separate copy.
- **Static dispatch for interfaces** – cannot add interfaces to types from other modules.

## Tooling & Commands

```bash
mux build file.mux        # Compile
mux run file.mux          # Compile and run
mux format file.mux       # Format (not yet implemented)
mux try file.mux          # Quick experimentation
mux doctor                # Check dependencies
mux doctor --dev          # Check LLVM/clang for development
```

- **LLVM IR** – use `mux run -i file.mux` to inspect generated `.ll` files.
- **Testing** – use `cargo run -- test_scripts/example.mux` for single‑file tests.
- **Project structure** – `mux-compiler/` (Rust), `mux-runtime/` (C/Rust), `test_scripts/` (samples).

## Helping Users Effectively

1. **Always check type compatibility** – remind users to use explicit conversions.
2. **Encourage pattern matching** – for `result<T,E>`, `optional<T>`, and `match` expressions.
3. **Prefer `auto`** for local variables, but explicit types for function signatures.
4. **Use standard library** – suggest `std.math`, `std.io`, `std.net`, etc., when relevant.
5. **Highlight Mux‑specific features** – reference counting, boxed values, static interfaces.
6. **Point to resources** – language guide (https://mux-lang.dev), GitHub repo, examples in `test_scripts/`.