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

- Read the [Language Reference](./language/index.md) to learn about Mux's features
- Learn about [Types and Variables](./language/types.md)
- Understand [Variable Declarations](./language/variables.md)

<!-- TODO: Add links when these pages are created:
- Check out Examples for more code samples
- Learn about Error Handling with Result and Optional
-->

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

- [Language Reference](./language/index.md)
- [GitHub Issues](https://github.com/derekcorniello/mux-lang/issues)

<!-- TODO: Add contributing guide link when page is created -->

## What's Next?

Now that you have Mux installed, explore the documentation:

- [Language Reference](./language/index.md) - Complete language overview
- [Types and Variables](./language/types.md) - Type system and conversions
- [Variable Declarations](./language/variables.md) - Using auto and const

<!-- TODO: Add links when these pages are created:
- Functions
- Control Flow
- Classes and Interfaces
- Error Handling
-->
