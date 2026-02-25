# Design Philosophy

Mux is built on clear principles that guide every design decision. This page explains the "why" behind Mux's design.

## Core Principles

### 1. Clarity Over Completeness

**Principle:** It's better to have fewer, well-understood features than many half-baked ones.

**What this means in practice:**
- Mux doesn't try to support every programming paradigm
- Features must have clear, explainable semantics
- When in doubt, leave it out

**Example:** Mux doesn't have complex macro systems or compile-time code generation. While powerful, these features make code harder to understand and debug.

### 2. Intentional Design Over Feature Accumulation

**Principle:** Every feature must justify its complexity by improving understanding or safety.

**What this means in practice:**
- New features require strong justification
- Complexity must provide proportional value
- I say "no" more often than "yes"

**Example:** Mux uses reference counting instead of a borrow checker. This is intentional, I trade some performance for dramatically simpler mental models.

### 3. Explicit Over Implicit

**Principle:** Code should be clear about what it's doing. No hidden behavior.

**What this means in practice:**
- No implicit type conversions
- No hidden memory allocations (where possible)
- Function calls are obvious, not disguised as operators

**Example:**
```mux title="explicit_conversions.mux"
// ERROR: No implicit conversion
auto x = 1 + 1.5  

// Explicit: You must choose how to convert
auto x = (1).to_float() + 1.5  // 2.5 as float
auto y = 1 + (1.5).to_int()   // 2 as int
```

### 4. Readable Syntax

**Principle:** Code is read far more than it's written. Optimize for reading.

**What this means in practice:**
- No semicolons (less visual noise)
- Clear keywords (`func`, `returns`, `auto`)
- Obvious control flow

**Example:**
```mux title="readable_syntax.mux"
// Clean and readable
func calculate(int x) returns int {
    if x > 10 {
        return x * 2
    }
    return x
}
```

### 5. Safety Without Complexity

**Principle:** Memory safety doesn't mean slow performance or difficult code.

**What this means in practice:**
- Reference counting for automatic memory management
- No manual `malloc`/`free`
- No borrow checker to fight with
- Compile-time type safety

**Tradeoff:** Reference counting has runtime overhead. That is okay, safety and simplicity are worth it for most use cases.

## What Mux Prioritizes

Understanding what Mux focuses on is just as important as what it does not:

### Focused Design

Mux has clear opinions about what belongs in the language. It does not try to be everything to everyone.

**Why:** Languages that try to support every paradigm often do nothing particularly well. Focus enables excellence.

### Developer Experience Over Raw Performance

Mux prioritizes readability, safety, and developer productivity over absolute maximum performance.

**Why:** Most programs are not CPU-bound. For those that are, profile first, optimize later. Reference counting provides excellent performance for most use cases.

## Key Design Decisions

### Why Reference Counting?

**Decision:** Mux uses reference counting for memory management instead of:
- Garbage collection (like Go, Java)
- Borrow checking (like Rust)
- Manual management (like C)

**Rationale:**
- Simpler than borrow checking
- Deterministic cleanup (unlike GC)
- No stop-the-world pauses
- Cycles require manual breaking (documented pattern)
- Slower than Rust in some cases

### Why No Implicit Conversions?

**Decision:** All type conversions must be explicit using `.to_*()` methods.

**Rationale:**
- No surprise behavior
- Clear what type you're working with
- Prevents subtle bugs
- More verbose than dynamic languages

### Why No Semicolons?

**Decision:** Statements terminate with newlines, not semicolons.

**Rationale:**
- Less visual clutter
- Easier for beginners
- Follows Python/Go convention
- Requires careful expression design

### Why Pattern Matching?

**Decision:** Include Rust-style pattern matching with guards.

**Rationale:**
- Elegant error handling
- Safer than if/else chains
- Compiler checks exhaustiveness
- Natural for `result` and `optional`

```mux title="pattern_matching_errors.mux"
match divide(10, 0) {
    ok(result) { print("Success: " + result.to_string()) }
    err(error) { print("Error: " + error) }
}
```

### Why Monomorphization for Generics?

**Decision:** Generics are specialized at compile-time (like Rust, C++).

**Rationale:**
- Zero runtime cost
- Full type safety
- Enables optimization
- Increases binary size
- Slower compile times

## Evolution of Design

Mux's design will evolve, but guided by these principles:

**Changes I might make:**
- Adding new features that fit the philosophy
- Improving syntax based on real-world usage
- Better error messages and tooling

**Changes I won't make:**
- Adding implicit conversions
- Introducing a garbage collector
- Making the type system optional

## Questions About Design?

The best place to discuss Mux's design decisions is on [GitHub Discussions](https://github.com/derekcorniello/mux-lang/discussions).

I value:
- Thoughtful critique
- Real-world use cases
- Honest feedback

I don't promise to agree with every suggestion, but I promise to consider them seriously.

---

**Next:** [See how Mux compares to other languages](./comparisons.md)
