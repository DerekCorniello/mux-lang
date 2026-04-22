# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.1] - 2026-04-22

### Changed
- **Compiler maintainability work**: Reduced complexity across compiler modules with a broad cleanup and refactor pass.
- **Standard library internals**: Refactored and optimized stdlib implementations for better consistency and maintainability.
- **Developer workflow and project metadata**: Updated AI agent guidance, OpenCode configuration, and supporting repository automation files.
- **Documentation and website updates**: Improved README content and landing page structure, examples, and installation guidance.

### Fixed
- **Codegen regressions**: Fixed recent LLVM IR generation regressions and related import handling issues.
- **Website behavior**: Corrected landing page rendering details, including list key usage and stack example behavior.
- **Build and CI support scripts**: Fixed tooling and script issues affecting local and CI workflows.
- **Versioning release prep**: Synced release metadata and version-related files for `0.2.1`.

### Security
- **Dependency and vulnerability updates**: Applied dependency maintenance and vulnerability fixes, including Dependabot-driven updates.
- **Static analysis cleanup**: Addressed SonarCloud findings and code quality issues across the codebase.

## [0.2.0] - 2026-03-24

### Added
- **Standard library**: Full implementation of standard library modules (`math`, `io`, `net`, `sql`, `random`, `datetime`, `dsa`).
- **Data structures library**: New `dsa` module with binary tree, graph, and other data structures.
- **SQL support**: SQL client functionality for database interactions.
- **HTTP client**: Built‑in HTTP client for making web requests.
- **Network server architecture**: Foundation for building network servers.
- **JSON, CSV, and environment utilities**: Tools for handling JSON, CSV, and environment variables.
- **Networking primitives**: Low‑level networking building blocks.
- **IO stdlib library**: Standard I/O operations.
- **Error message improvements**: More helpful and context‑aware error messages.
- **Refactored codebase to Rust idioms**: Improved readability and maintainability.
- **CI improvements**: Fixed continuous integration pipelines.
- **Project tooling & hooks**: Updated pre‑commit hooks and development tooling.

### Changed
- **Upgraded to LLVM 17** (already present, but now formally documented).
- **Improved installation process**: Better installer scripts and platform detection.
- **Simplified project structure**: Cleanup of repository layout.

### Fixed
- **Numerous bug fixes** across the compiler and runtime.
- **Reference counting issues**: Fixed memory management bugs.
- **Type checking edge cases**: Corrected handling of complex type scenarios.
- **Code generation correctness**: Fixed issues with LLVM IR generation.
- **Exhaustiveness checking in match statements**: Guards and wildcards now work correctly.
- **Class and interface resolution**: Fixed bugs in type hierarchy.

### Security
- **Resolved dependabot alerts** (see PR #140).

## [0.1.2] - 2026-02-08

### Added
- **Match as switch statement**: Extended `match` to work as a switch statement for any type (not just enums).
- **Improved pattern matching**: Enhanced exhaustiveness checking and guard support.

### Fixed
- **Reference and chaining fixes**: Resolved issues with reference handling and method chaining.
- **Function return handling**: Corrected return value processing.
- **Class‑related bugs**: Fixed errors in class instantiation and inheritance.
- **Frontend cleanup**: Removed erroneous information from error messages.

## [0.1.1] - 2026-02-07

### Fixed
- **Crates.io publishing**: Fixed configuration and metadata for publishing to crates.io.
- **Build updates**: Adjusted build scripts for proper release artifacts.

## [0.1.0] - 2026-02-07

### Added
- **Initial public release** of the Mux compiler and runtime.
- **Core language features**: Static typing, generics, pattern matching, error handling (`result<T,E>`, `optional<T>`).
- **LLVM‑based code generation**: Produces native executables.
- **Reference‑counted memory management**: Automatic memory safety.
- **Basic standard library**: Collections, string operations, I/O.
- **Installer scripts** for Linux, macOS, and Windows.
- **Documentation website** (mux‑lang.dev) with language specification.

### Known Issues
- No LSP or code formatter yet.
- Standard library is minimal.
- Breaking changes expected.
