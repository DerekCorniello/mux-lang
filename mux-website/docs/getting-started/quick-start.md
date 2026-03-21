# Quick Start

Get up and running with Mux in just a few minutes.

## Prerequisites

Before you begin, make sure you have the following installed:

- **For prebuilt install**: no Rust or LLVM toolchain needed
- **For source install**: Rust is required; use the bootstrap script to install LLVM 17 and clang 17

## Installation

### Option 1: Install prebuilt binaries (Recommended)

Install with the official script:

```bash title="bash"
curl -fsSL https://raw.githubusercontent.com/derekcorniello/mux-lang/main/scripts/install.sh | sh
```

Windows PowerShell:

```powershell title="powershell"
iwr -useb https://raw.githubusercontent.com/derekcorniello/mux-lang/main/scripts/install.ps1 | iex
```

### Option 2: Install from crates.io (Advanced)

If you want to build from source with cargo:

```bash title="bash"
cargo install mux-lang
```

This installs the Mux compiler to your cargo bin directory.
The runtime library is built on first use or by running `mux doctor`.

**Note:** Make sure LLVM 17 and clang are installed first for source builds.

Use bootstrap helpers for contributor source builds:

```bash title="bash"
./scripts/bootstrap-dev.sh
./scripts/dev-cargo.sh install --path mux-compiler
```

This works on Arch Linux, Debian, and Ubuntu. The bootstrap script installs LLVM 17 and clang 17 for your platform.

### Option 3: Build from Source

If you prefer to build from source, maybe to even help [contribute](https://github.com/derekcorniello/mux-lang/blob/main/CONTRIBUTING.md) to the project:

```bash title="bash"
git clone https://github.com/derekcorniello/mux-lang
cd mux-lang
cargo build --release
```

The compiler will be built in `target/release/mux-compiler`.

## Verify Installation

After installation, verify everything is working:

```bash title="bash"
mux --version
```

Use the built-in doctor command to check your setup:

```bash title="bash"
mux doctor
mux doctor --dev
```

`mux doctor` validates runtime dependencies for end users. `mux doctor --dev` validates LLVM 17 and clang for source development.

## Your First Mux Program

### 1. Create a File

Create a new file called `hello.mux`:

```mux title="hello.mux"
func main() returns void {
    print("Hello, Mux!")
}
```

### 2. Run the Program

```bash title="bash"
mux run hello.mux
```

You should see:
``` title="output"
Hello, Mux!
```

## Next Steps

### Try More Examples

Create a file called `numbers.mux`:

```mux title="numbers.mux"
func main() returns void {
    auto numbers = [1, 2, 3, 4, 5]
    
    for int num in numbers {
        auto squared = num * num
        print("Square of " + num.to_string() + " is " + squared.to_string())
    }
}
```

Run it:
```bash title="bash"
mux run numbers.mux
```

### Explore the Language

- Read the [Language Guide](../language-guide/overview.md) to learn about Mux's features
- Learn about [Types and Variables](../language-guide/types.md)
- Understand [Variable Declarations](../language-guide/variables.md)
- Discover [Why Mux exists](./why-mux.md)

## Commands and Options

```bash title="bash"
Usage: mux [OPTIONS] <COMMAND>

Commands:
  build    Compile a Mux file without running it
  run      Compile and run a Mux file
  format   Format a Mux file
  try      Try running a Mux file (for quick experimentation)
  doctor   Check system dependencies for the Mux compiler
  version  Print the Mux version
  help     Print this message or the help of the given subcommand(s)

Options:
  -o, --output <OUTPUT>  Name of the output executable
  -i, --intermediate     Emit intermediate LLVM IR (.ll)
  -h, --help             Print help
  -V, --version          Print version
```

## Getting Help

- [Language Guide](../language-guide/overview.md)
- [Why Mux?](./why-mux.md)
- [GitHub Issues](https://github.com/derekcorniello/mux-lang/issues)

## Current Limitations

Mux is actively being developed. Here are some things to be aware of:

- **No LSP (Language Server Protocol)** - Editor support is limited to basic syntax highlighting
- **No Code Formatter** - There is currently no automated code formatting tool
- **Standard Library Under Development** - The stdlib is incomplete and APIs may change
- **Breaking Changes Expected** - The language is evolving, so expect syntax and semantic changes

I am working on these features, but they are not yet available. Contributions are welcome!

## What's Next?

Now that you have Mux installed, explore the documentation:

- [Language Guide](../language-guide/overview.md) - Complete language overview
- [Types and Variables](../language-guide/types.md) - Type system and conversions
- [Variable Declarations](../language-guide/variables.md) - Using auto and const
- [Design Philosophy](../design-notes/philosophy.md) - Why Mux is designed the way it is
