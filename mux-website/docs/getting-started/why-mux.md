# Why Mux?

## The Origin Story

<!-- TODO: Add the actual story of how Mux got its name from Derek -->

Mux (short for "MuxLang") was created to explore what a modern programming language could look like when combining the best aspects of existing languages while maintaining simplicity and clarity.

## What Problems Does Mux Solve?

### 1. **The Python Ease-of-Use Problem**

Python is beloved for its readability and ease of use, but it lacks:
- Strong static typing (until recently, and even then it's optional)
- Compile-time error checking
- Native performance

**Mux's Approach:** Combine Python's clean syntax and ease of use with mandatory static typing and LLVM-powered native compilation.

### 2. **The Rust-Inspired Safety Path**

Rust sets the bar for memory safety, using an innovative ownership model. It is an amazing achievement, but requires significant learning investment.

**Mux's Approach:** Achieve memory safety through reference counting instead of compile-time borrow checking. This provides excellent safety with a gentler learning curve, letting you focus on your problem instead of fighting the type system.

### 3. **The Go Design Space**

Go excels at simplicity, fast compilation, and building reliable network services.

**Mux's Approach:** Inspired by Go's minimalism but pushes further with modern language features like generics with monomorphization, pattern matching with guards, and Result/Optional types for expressive error handling.

## Design Goals

### What Mux Aims For

**Readability First**
Code should be easy to read and understand. No semicolons, clear syntax, explicit types.

**Explicit Over Implicit**
No implicit type conversions. No hidden behavior. What you see is what you get.

**Safety Without Complexity**
Memory safety through reference counting. No manual memory management, no borrow checker.

**Fast Compilation**
LLVM backend for native performance, but optimized for quick iteration during development.

**Beginner Friendly**
If you know Python or JavaScript, you should be able to read Mux code immediately.

### Current Priorities

Mux is focused on getting the fundamentals right first:

**Core Language Features**
Solid foundations matter more than adding every possible feature. We're building a complete, well-designed language, not rushing to add every bell and whistle.

**Runtime Performance**
Reference counting provides excellent performance for most use cases while keeping the memory model simple and predictable.

**Learning-Friendly**
Mux prioritizes being easy to learn and read over providing maximum control or performance at the expense of complexity.

**Concurrency**
Thread-safe reference counting is in place. Higher-level concurrency features will be added after the core language stabilizes.

## Honest Tradeoffs

Every language makes tradeoffs. Here is what to expect:

**Mux Gives You:**
- Easy-to-understand memory model (reference counting)
- Simple, readable syntax
- Strong compile-time guarantees
- Native performance (faster than Python/JavaScript)
- Predictable behavior with no garbage collection pauses

**Tradeoffs to Know:**
- Reference counting has overhead (faster than GC languages, slower than Rust in some cases)
- Explicit type conversions (more verbose than dynamic languages, but safer)
- Early-stage ecosystem (growing, but not yet mature)
- Active development (we're building toward stability)

## Who Is Mux For?

**Mux is a great choice if:**
- You want Python's ease of use with compile-time type safety
- You appreciate memory safety without a borrow checker
- You want a modern language that is easy to teach and learn
- You value readability and maintainability
- You're building applications where developer productivity matters

**Mux is growing toward:**
- Maximum performance (getting closer with each release)
- A rich ecosystem of libraries
- Production-ready concurrency features
- Long-term stability

## Philosophy

Mux follows these core principles:

**Clarity over completeness**  
Better to have fewer, well-understood features than many half-baked ones.

**Intentional design over feature accumulation**  
Every feature must justify its complexity by improving understanding or safety.

**Documentation as a tool for thinking**  
Writing good docs reveals design flaws. If you can't explain it simply, it's probably not simple enough.

**Honest communication**  
No marketing hype. No promises we can't keep. Just honest assessment of what works and what doesn't.

## Coming from Other Languages?

### From Python
You will love: The familiar syntax, ease of use, and clean code style
You will appreciate: Compile-time type checking catching bugs before they happen

### From JavaScript/TypeScript
You will love: No more `undefined` surprises, Result/Optional instead of null
You will appreciate: A stricter type system that catches bugs at compile time

### From Go
You will love: The simplicity and minimalism
You will enjoy: Pattern matching and generics (familiar if you used Go 1.18+)

### From Rust
You will love: No borrow checker, simpler lifetime management
You will appreciate: Reference counting with predictable performance

## What's Next?

Ready to try Mux? Head to the [Quick Start](./quick-start.md) guide to install and write your first program.

Want to understand the design decisions? Check out the [Design Notes](../design-notes/philosophy.md).

## Current Status

**Mux is in active development.** The language is growing and improving rapidly:

- Core language features are stabilizing
- The compiler and runtime are getting faster with each release
- Documentation is expanding
- We're building toward a stable 1.0 release

This is an exciting time to get involved and help shape Mux's future.

If you're interested in following along or contributing, check out the [GitHub repository](https://github.com/derekcorniello/mux-lang).
