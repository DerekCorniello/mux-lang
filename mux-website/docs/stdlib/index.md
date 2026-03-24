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
import std                      // namespace import: std.assert, std.math, std.io, std.random, std.datetime, std.sync, std.net, std.env, std.data, std.dsa, std.sql
import std.math                 // single module namespace
import std.(math, random as r)  // qualified and selective imports with aliasing
import std.*                    // flat import of all stdlib items
```

## Available Modules

| Module | Description | Status |
|--------|-------------|--------|
| [assert](./assert) | Test assertions that panic on failure | Available |
| [math](./math) | Mathematical functions and constants | Available |
| [io](./io) | File and path operations with `result`-based errors | Available |
| [net](./net) | TCP/UDP primitives plus JSON HTTP client/server bindings | Available |
| [sql](./sql) | SQL connections, transactions, result sets, and typed SQL values | Available (SQLite) |
| [env](./env) | Environment inspection utilities (`env.get`) | Available |
| [data.json](./data-json) | JSON parsing, conversion, and serialization helpers | Available |
| [data.csv](./data-csv) | CSV parsing with optional header support | Available |
| [random](./random) | Pseudorandom number generation | Available |
| [sync](./sync) | Threading, locks, condition variables, and sleep helpers | Available |
| [datetime](./datetime) | Time retrieval, formatting, and sleep helpers | Available |
| [dsa](./dsa) | Data structures and algorithms (stack, queue, heap, bintree, graph) | Available |

## Module Stability

The standard library is published and available. Core modules are stable, and APIs may evolve as new modules are added. Any breaking changes will be documented in release notes.

## Future Modules

Planned additions include:

- `string` - String manipulation utilities

## Contributing

The standard library is part of the Mux language project. Contributions are welcome! See the [Contributing Guide](https://github.com/derekcorniello/mux-lang/blob/main/CONTRIBUTING.md) for guidelines.
