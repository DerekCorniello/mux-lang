# Language Reference

This section provides detailed, technical documentation of Mux's syntax and semantics.

## Purpose

The Language Reference is the authoritative source for:
- Exact syntax definitions
- Semantic behavior
- Edge cases and corner cases
- Implementation details

This is different from the [Language Guide](../language-guide/index.md), which focuses on teaching and examples.

## Contents

### Core Reference

| Topic | Description |
|-------|-------------|
| [Lexical Structure](./lexical-structure.md) | Tokens, keywords, identifiers, literals, comments |
| [Grammar](./grammar.md) | Formal BNF-style syntax definitions |
| [Type System](./type-system.md) | Type rules, conversions, inference, generics |
| [Operators](./operators.md) | Operator precedence, associativity, behavior |

### Behavior Reference

| Topic | Description |
|-------|-------------|
| [Statements](./statements.md) | Detailed semantics of each statement type |
| [Expressions](./expressions.md) | Expression evaluation rules |
| [Memory Model](./memory-model.md) | Reference counting specifics |
| [ABI](./abi.md) | Binary interface for external interop |

### Quick Reference

| Topic | Description |
|-------|-------------|
| [Operators](./operators.md#operator-precedence) | Operator precedence table |
| [Grammar](./grammar.md) | Reserved keywords |
| [Type System](./type-system.md#primitive-types) | Primitive type specifications |
| [Memory Model](./memory-model.md#memory-layout) | Memory layout diagrams |

## Reference vs Guide

| Language Guide | Language Reference |
|----------------|-------------------|
| Learning-oriented | Reference-oriented |
| Concept explanations | Precise rules |
| Practical examples | Formal definitions |
| Progressive complexity | Topic-by-topic organization |

## Additional Resources

- [Language Guide](../language-guide/index.md) - Practical language documentation
- [README.md](https://github.com/derekcorniello/mux-lang/blob/main/README.md) - Comprehensive language specification
- [Source Code](https://github.com/derekcorniello/mux-lang) - The compiler itself is the ultimate reference

## Contributing

If you find discrepancies between documentation and implementation, please [file an issue](https://github.com/derekcorniello/mux-lang/issues).
