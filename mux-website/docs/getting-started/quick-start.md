# Quick Start

Get up and running with Mux in just a few minutes.

## Prerequisites

Before you begin, make sure you have the following installed:

- **Rust** (1.70 or later) - [Install Rust](https://rustup.rs/)
- **LLVM** (14 or later)
- **clang** (for linking compiled programs)
- **Git**

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/derekcorniello/mux-lang
cd mux-lang
```

### 2. Build the Compiler

```bash
cargo build --release
```

The compiler will be built in `target/release/mux-compiler`.

### 3. Verify Installation

```bash
cargo run -- --version
```

## Your First Mux Program

### 1. Create a File

Create a new file called `hello.mux`:

```mux
func main() returns void {
    print("Hello, Mux!")
}
```

### 2. Run the Program

```bash
cargo run -- run hello.mux
```

You should see:
```
Hello, Mux!
```

## Next Steps

### Try More Examples

Create a file called `numbers.mux`:

```mux
func main() returns void {
    auto numbers = [1, 2, 3, 4, 5]
    
    for num in numbers {
        auto squared = num * num
        print("Square of " + num.to_string() + " is " + squared.to_string())
    }
}
```

Run it:
```bash
cargo run -- run numbers.mux
```

### Explore the Language

- Read the [Language Guide](../language-guide/index.md) to learn about Mux's features
- Learn about [Types and Variables](../language-guide/types.md)
- Understand [Variable Declarations](../language-guide/variables.md)
- Discover [Why Mux exists](./why-mux.md)

## Common Commands

```bash
# Run a Mux program
cargo run -- run <file.mux>

# Build the compiler in release mode
cargo build --release

# Run tests
cargo test

# Check code without building
cargo check
```

## Getting Help

- [Language Guide](../language-guide/index.md)
- [Why Mux?](./why-mux.md)
- [GitHub Issues](https://github.com/derekcorniello/mux-lang/issues)

## What's Next?

Now that you have Mux installed, explore the documentation:

- [Language Guide](../language-guide/index.md) - Complete language overview
- [Types and Variables](../language-guide/types.md) - Type system and conversions
- [Variable Declarations](../language-guide/variables.md) - Using auto and const
- [Design Philosophy](../design-notes/philosophy.md) - Why Mux is designed the way it is
