# Standard Library

The Mux standard library provides a collection of essential modules for common programming tasks. These modules are designed to be simple, efficient, and consistent with Mux's philosophy of explicit, safe code.

## Using the Standard Library

Import standard library modules using the `import` keyword:

```mux title="import_example.mux"
import std.random
import std.datetime

func main() returns void {
    auto roll = random.next_range(1, 7)
    print("You rolled: " + roll.to_string())
}
```

You can import stdlib in multiple forms:

```mux title="stdlib_import_forms.mux"
import std                    // namespace import: std.math, std.io, std.random, std.datetime
import std.math               // single module namespace
import std.datetime
import std.(math, random as r)
import std.*                  // flat import of all stdlib items
```

## Available Modules

| Module | Description | Status |
|--------|-------------|--------|
| [assert](./assert) | Test assertions that panic on failure | Available |
| [math](./math) | Mathematical functions and constants | Available |
| [io](./io) | File and path operations with `result`-based errors | Available |
| [net](./net) | TCP/UDP primitives and protocol-agnostic request/response shapes | Available |
| [random](./random) | Pseudorandom number generation | Available |
| [datetime](./datetime) | Time retrieval, formatting, and sleep helpers | Available |

## Module Stability

The standard library is actively being developed. While the core functionality is stable, APIs may change as the language evolves. Breaking changes will be documented in release notes.

## Future Modules

Planned additions include:

- `string` - String manipulation utilities
- `collections` - Additional collection types and algorithms

## Contributing

The standard library is part of the Mux language project. Contributions are welcome! See the [Contributing Guide](https://github.com/derekcorniello/mux-lang/blob/main/CONTRIBUTING.md) for guidelines.
