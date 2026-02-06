# Welcome to Mux

Mux is a statically-typed, reference-counted programming language that combines Go-like minimalism with Rust-inspired safety.

## Why Mux?

- **Simple yet powerful:** Combines Go-like minimalism with Rust-inspired safety
- **Strong static typing:** Helps catch errors early and ensures safer code
- **LLVM-powered:** Fast compilation and native performance
- **Flexible memory management:** Ease of use through reference counting
- **Extensible:** Designed to evolve with features like traits, concurrency, and a standard library

## Quick Example

```mux
func main() returns void {
    print("Hello, Mux!")
    
    auto numbers = [1, 2, 3, 4, 5]
    for num in numbers {
        print(num.to_string())
    }
}
```

## Getting Started

Ready to dive in? Check out the [Quick Start](./getting-started/quick-start.md) guide to install Mux and write your first program.

Or learn [Why Mux exists](./getting-started/why-mux.md) and what problems it aims to solve.

## Language Features

- **Static type system** with type inference using `auto`
- **Pattern matching** with guards for elegant control flow
- **Generics** with compile-time monomorphization
- **Error handling** with `Result<T, E>` and `Optional<T>`
- **Memory safety** through reference counting
- **No semicolons** - clean, readable syntax
- **Collection literals** for lists, maps, and sets
- **Interfaces** for polymorphism with static dispatch

## Project Status

Mux is currently in active development. The language specification, compiler, and tooling are evolving. Expect breaking changes and incomplete features as we work towards a stable release.

## Community

- [GitHub Repository](https://github.com/derekcorniello/mux-lang)
- [Contributing Guide](https://github.com/derekcorniello/mux-lang/blob/main/CONTRIBUTING.md)
- [Issues](https://github.com/derekcorniello/mux-lang/issues)

## License

Mux is licensed under the MIT license.
