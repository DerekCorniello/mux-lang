# Mux WASM Runtime Plan

## Goal
Enable Mux code to compile and run entirely in the browser via WebAssembly (WASM), eliminating the need for a server backend for the playground.

## Why WASM?
- **Zero infrastructure costs** -- no server to maintain or pay for
- **Instant execution** -- no network latency, code runs in the user's browser
- **Privacy** -- user code never leaves the machine
- **Offline capable** -- playground works without internet after initial load

## Constraints
- **Native builds must be unaffected** -- all WASM changes behind `#[cfg]` or feature flags
- **Buildable with `cargo build` as before** -- default features = LLVM backend
- **WASM target**: `cargo build --target wasm32-unknown-unknown --no-default-features --features backend-cranelift`
- **No regressions** -- all existing test scripts must pass on native

---

## Phase 0: Runtime Cleanup (1-2 days)

### Goal
Make mux-runtime compile on `wasm32-unknown-unknown` without breaking native builds.

### Tasks

#### 0.1 Remove unused `gc` dependency
- `gc = "0.4"` is listed in `mux-runtime/Cargo.toml` but **never imported** anywhere in the codebase
- **Action**: Delete the line from `Cargo.toml`
- **Verification**: `cargo build` passes, `cargo test` passes

#### 0.2 Replace `libc::rand()` with pure Rust PRNG
- `random.rs` (lines 13, 25, 27, 46) uses `libc::srand()`, `libc::rand()`, `libc::RAND_MAX`
- **Action**: Replace with a tiny pure-Rust PRNG (e.g., xorshift32 or LCG)
  - No new dependencies needed -- implement a minimal LCG inline
  - Remove the `libc` dependency from Cargo.toml
  - If `libc` is still needed elsewhere (unlikely after sync gating), feature-gate it
- **Verification**: `cargo run -- test_scripts/random.mux` produces same output range
  - Note: removing `libc::rand()` means the RNG output will differ on native too. This is acceptable -- the new PRNG is deterministic and cross-platform.

#### 0.3 Add WASM stubs for `sync.rs`
- `sync.rs` has two backends: `#[cfg(unix)]` (pthread) and `#[cfg(windows)]` (SRWLock)
- **Action**: Add `#[cfg(target_arch = "wasm32")]` third backend with stub implementations:
  - `MuxMutex`: no-op lock/unlock (single-threaded WASM)
  - `MuxRwLock`: no-op read/write/unlock
  - `MuxCondVar`: no-op wait/signal/broadcast
  - `mux_spawn`: return error "threading not supported on WASM"
  - `mux_thread_sleep`: no-op
- **Verification**: Runtime compiles on `wasm32-unknown-unknown`

#### 0.4 Guard `thread::sleep` in `datetime.rs`
- `datetime.rs` (lines 139, 151) calls `thread::sleep()`
- **Action**: Add `#[cfg(not(target_arch = "wasm32"))]` guard. On WASM, make it a no-op or panic with "sleep not supported"
- **Verification**: Runtime compiles on `wasm32-unknown-unknown`

#### 0.5 Gate `net`/`sql` features on WASM
- `net.rs` (TcpStream, UdpSocket) and `sql.rs` (rusqlite, postgres, mysql) don't compile on WASM
- **Action**: These are already feature-gated (`#[cfg(feature = "net")]`, `#[cfg(feature = "sql")]`)
  - Ensure the default features exclude net/sql on WASM (e.g., feature `wasm = ["core", "json", "csv"]`)
- **Verification**: `cargo build --target wasm32-unknown-unknown` with appropriate features passes

#### 0.6 Verify all native tests still pass
- `cargo build && cargo run -- test_scripts/*.mux` -- all 79 test scripts pass
- `cargo clippy` -- no warnings
- `cargo fmt` -- formatting clean

### Files to modify
- `mux-runtime/Cargo.toml` -- remove `gc`, remove `libc`, add features
- `mux-runtime/src/random.rs` -- replace libc calls with pure Rust PRNG
- `mux-runtime/src/sync.rs` -- add WASM backend
- `mux-runtime/src/datetime.rs` -- guard thread::sleep
- `mux-runtime/src/lib.rs` -- adjust feature gates

---

## Phase 1: Cranelift Codegen Backend (2-4 weeks)

### Goal
Create a new codegen module that uses [Cranelift](https://github.com/bytecodealliance/cranelift) instead of LLVM/inkwell, producing native object files from Mux AST.

### Why Cranelift?
- **Pure Rust** -- no native dependencies, compiles everywhere including WASM
- **Mature** -- used by Wasmtime, rustc_codegen_cranelift, various JIT compilers
- **Good codegen** -- reasonable optimization passes (no LLVM-level opts, but sufficient)
- **Cross-platform** -- x86-64, ARM64, WASM as targets

### Architecture

The existing codegen lives at `mux-compiler/src/codegen/` (~14K LOC). The new backend will live alongside it:

```
mux-compiler/src/
  codegen/                 # existing LLVM backend (untouched)
  codegen_cranelift/       # new Cranelift backend
    mod.rs                 # CodeGenerator struct, generate(), emit_object()
    types.rs               # Mux type -> Cranelift type mapping
    expressions.rs         # Expression codegen
    statements.rs          # Statement codegen
    functions.rs           # Function declaration/codegen
    runtime.rs             # Runtime function declarations
    memory.rs              # Refcounting, boxing, unboxing
    constructors.rs        # Class/enum constructor codegen
    methods.rs             # Method codegen
    classes.rs             # Class codegen
    generics.rs            # Generic monomorphization
    operators.rs           # Operator codegen
```

### Tasks

#### 1.1 Add Cranelift dependencies to Cargo.toml
```toml
[features]
default = ["backend-llvm"]
backend-llvm = ["inkwell"]
backend-cranelift = []

[dependencies]
cranelift = "0.117"
cranelift-module = "0.117"
cranelift-object = "0.117"
cranelift-simplejit = "0.117"  # optional, for JIT testing
target-lexicon = "0.13"
```

#### 1.2 Create `codegen_cranelift/mod.rs` -- CodeGenerator struct
- Match the existing CodeGenerator's public API:
  ```rust
  pub struct CodeGenerator {
      module: cranelift_module::Module,
      builder: cranelift_codegen::Context,
      // ... same state as LLVM backend
  }

  impl CodeGenerator {
      pub fn new(analyzer: &mut SemanticAnalyzer) -> Self
      pub fn generate(&mut self, nodes: &[AstNode]) -> Result<(), String>
      pub fn emit_object(&self, filename: &str) -> Result<(), String>
  }
  ```
- Maintain same maps: type_map, vtable_map, enum_variants, field_map, classes, constructors, variables, functions, etc.
- Replace inkwell types with Cranelift equivalents:
  - `Context` → `cranelift_codegen::Context`
  - `Module<'a>` → `cranelift_module::Module`
  - `Builder<'a>` → `FuncCursor` + `FuncBuilder`
  - `BasicValueEnum<'a>` → `ir::Value`
  - `PointerValue<'a>` → `ir::Value`
  - `FunctionValue<'a>` → `ir::FuncRef`
  - `BasicTypeEnum<'a>` → `ir::Type`

#### 1.3 Create `codegen_cranelift/types.rs`
- Map Mux types to Cranelift types (same as LLVM):
  - int → `types::I64`
  - float → `types::F64`
  - bool → `types::B8` or `types::I8`
  - char → `types::I64`
  - everything else → `types::I64` (pointer to `*mut Value`)
- Port `llvm_type_from_resolved_type()`

#### 1.4 Create `codegen_cranelift/runtime.rs`
- Port all ~150 runtime function declarations from existing `codegen/runtime.rs`
- Declare external functions via `Module::declare_function()` with appropriate signatures
- Port the `box_value()` / `unbox_value()` helpers

#### 1.5 Create `codegen_cranelift/expressions.rs`
- Port expression codegen from LLVM to Cranelift
- Key differences:
  - **No phi nodes** -- use Cranelift block parameters (`Block::append_block_param`)
  - **No GEP** -- use `iconst + iadd` for pointer arithmetic
  - **Alloca** → `create_sized_stack_slot` + `stack_addr`
  - **Load/Store** → `FuncCursor::ins().load()` / `.store()`
  - **Call** → `FuncCursor::ins().call()`

#### 1.6 Create `codegen_cranelift/statements.rs`
- Port statement codegen (variable declarations, assignments, returns, if/else, loops, match)
- If/else: use block params instead of phi nodes for merging values

#### 1.7 Create `codegen_cranelift/functions.rs`
- Port function declaration and codegen (parameters, returns, calling convention)
- Maintain shadow module concept for cross-module function references

#### 1.8 Create `codegen_cranelift/memory.rs`
- Port refcounting hooks (mux_rc_inc, mux_rc_dec calls at scope exits)
- Port boxing/unboxing logic

#### 1.9 Create `codegen_cranelift/constructors.rs`
- Port class and enum constructor generation
- Port vtable generation

#### 1.10 Create `codegen_cranelift/methods.rs`
- Port method declaration and codegen
- Port method lookup and dispatch

#### 1.11 Create `codegen_cranelift/classes.rs`
- Port class type layout and field access

#### 1.12 Create `codegen_cranelift/generics.rs`
- Port generic monomorphization (generating concrete copies for each instantiation)

#### 1.13 Create `codegen_cranelift/operators.rs`
- Port operator codegen (arithmetic, comparison, logical)

#### 1.14 Wire up codegen selection in `main.rs`
- Add `--backend llvm|cranelift` CLI flag (default: `llvm`)
- Add feature-gated dispatch:
  ```rust
  #[cfg(feature = "backend-cranelift")]
  use codegen_cranelift::CodeGenerator;

  #[cfg(feature = "backend-llvm")]
  use codegen::CodeGenerator;
  ```
- Adjust linker step: Cranelift emits `.o` files instead of `.ll` files
  - `cc file.o -o executable -lmux_runtime -L<lib_dir>` instead of `clang file.ll -o executable`

#### 1.15 Initial test pass
- `cargo run -- test_scripts/hello_world.mux --backend cranelift` -- basic program works
- Iterate on simplest features first: literals, prints, arithmetic, variables, functions

### Testing Strategy
- Every test script run with **both** backends and compare output
- Start with simplest test scripts and work up:
  1. `hello_world.mux`, `arithmetic.mux`, `variables.mux`
  2. `if_else.mux`, `loops.mux`, `functions.mux`
  3. `lists.mux`, `maps.mux`, `tuples.mux`
  4. `classes.mux`, `enums.mux`, `interfaces.mux`
  5. `generics.mux`, `optionals.mux`, `results.mux`
  6. `imports.mux`, `closures.mux`
- Add a comparison runner script `scripts/test-cranelift.sh` that runs all tests with both backends

### Files to create
- `mux-compiler/src/codegen_cranelift/` (13 new files)
- `scripts/test-cranelift.sh` (test runner)

### Files to modify
- `mux-compiler/Cargo.toml` -- add Cranelift deps + features
- `mux-compiler/src/main.rs` -- CLI flag + feature-gated dispatch
- `mux-compiler/src/lib.rs` -- export new module

---

## Phase 2: WASM Packaging (3-5 days)

### Goal
Compile the Cranelift-backed compiler to WASM and expose a JavaScript API for the web playground.

### Tasks

#### 2.1 Create WASM API crate
- New crate: `mux-wasm/` (or feature in mux-compiler)
- Use `wasm-bindgen` for JS interop
- Expose API:
  ```rust
  #[wasm_bindgen]
  pub fn compile(code: &str) -> Result<CompileResult, JsValue>

  #[wasm_bindgen]
  pub struct CompileResult {
      pub object_bytes: Vec<u8>,   // compiled .wasm bytecode
      pub errors: Vec<String>,
      pub warnings: Vec<String>,
  }
  ```

#### 2.2 Build runtime for WASM
- Compile mux-runtime with `--target wasm32-unknown-unknown`
- Use `#[cfg(target_arch = "wasm32")]` to exclude net/sql/sync
- Export all 268 C-ABI functions as WASM exports

#### 2.3 Link compiler + runtime into single WASM module
- Static link mux-runtime into the compiler WASM module
- The runtime functions are called by the generated code at runtime
- Use `wasm-pack build --target web` for browser-ready output

#### 2.4 Wire up Monaco editor
- Update `mux-website/src/hooks/useMuxExecutor.ts` (or create new hook)
- Load the WASM module on page load
- `compile(code)` → receive WASM bytes + instantiate in a sandbox
- Capture stdout/stderr from the running WASM instance

### Files to create
- `mux-wasm/Cargo.toml` -- WASM bindings
- `mux-wasm/src/lib.rs` -- WASM API

### Files to modify
- `mux-website/src/hooks/useMuxExecutor.ts` -- use WASM instead of fetch

---

## Phase 3: Testing & Integration (1 week)

### Goal
All 79 test scripts pass with the Cranelift backend. Website playground works end-to-end.

### Tasks

#### 3.1 Regression test all 79 test scripts
- Run every test script with `--backend cranelift`
- Compare output with `--backend llvm`
- Document any differences or failures

#### 3.2 WASM integration test
- Test the WASM module in a headless browser (e.g., `wasm-bindgen-test-runner`)
- Verify basic compilation and execution round-trip

#### 3.3 Website integration
- Embed WASM module in the website (load from `static/wasm/`)
- Verify Monaco editor → compile → run works in browser
- Test both light and dark modes

#### 3.4 Performance testing
- Measure WASM module load time
- Measure compilation time for small/medium/large programs
- Measure execution time vs native (expect 2-5x slower in WASM, which is normal)

### Files to modify
- `mux-website/static/wasm/` -- deployed WASM artifacts
- `mux-website/src/theme/CodeBlock/index.tsx` -- ensure consistent with WASM path

---

## Phase 4: Benchmarking & Decision (After Phase 1)

### Goal
Compare Cranelift vs LLVM backends empirically to decide whether to drop LLVM entirely.

### Why benchmark?
- LLVM generates higher-quality machine code (aggressive optimization passes)
- Cranelift generates code faster but with fewer optimizations (~86% of LLVM quality on average)
- For Mux, most CPU time is spent inside the runtime (collections, refcounting, string ops), not in the generated glue code. So the real-world gap is likely smaller than 14%.
- Hard data will determine whether we keep both backends or drop LLVM.

### Benchmark Suite

Test scripts covering different workload profiles:

| Category | Tests | What it stresses |
|---|---|---|
| **Compute-heavy** | `fib(40)`, `collatz`, `primes` | Raw arithmetic, loops, Cranelift code quality |
| **Collections** | `lists`, `maps`, `sets` | Runtime-heavy (gap should be minimal) |
| **Pattern matching** | `match`, `enums`, `optionals`, `results` | Branch-heavy codegen |
| **OOP** | `classes`, `interfaces`, `generics` | Vtable dispatch, generics monomorphization |
| **Real-world mix** | `bintree`, `json_parse` | Mixed workload |

### Command

```bash
./scripts/bench.sh

# For each test:
#   cargo build --release -o /tmp/out-llvm  test.mux          (LLVM, default)
#   cargo build --release -o /tmp/out-clif test.mux --backend cranelift
#   hyperfine --warmup 3 /tmp/out-llvm /tmp/out-clif

# Compilation time:
#   hyperfine 'cargo build --release -o /tmp/out-llvm test.mux' \
#             'cargo build --release -o /tmp/out-clif test.mux --backend cranelift'
```

### Metrics

| Metric | How to measure | Expected gap |
|---|---|---|
| Compilation time | `hyperfine` on `cargo build` | Cranelift **3-10x faster** |
| Execution time | `hyperfine` on compiled binary | LLVM **5-14% faster** |
| Binary size | `ls -lh` | Similar |
| WASM binary size | `ls -lh` | N/A (LLVM can't produce WASM) |

### Decision Criteria

| If Cranelift execution is... | Recommendation |
|---|---|
| Within **5%** of LLVM | Drop LLVM entirely. Single backend, simpler code, WASM support. |
| **5-15%** slower | Keep both. Cranelift for WASM + dev, LLVM for release builds. |
| **>15%** slower | Keep both. Default to LLVM. Consider optimizing Cranelift backend. |

### Tasks

#### 4.1 Create benchmark runner script
- `scripts/bench.sh` that loops over test scripts, builds with both backends, collects timing data
- Outputs a markdown table comparing results

#### 4.2 Run compute-heavy benchmarks
- fib, collatz, primes, matrix_mult
- These show the widest gap between LLVM and Cranelift

#### 4.3 Run runtime-heavy benchmarks
- lists, maps, sets, tuples
- These should show minimal gap (time spent in Mux runtime)

#### 4.4 Run mixed benchmarks
- bintree, json_parse, classes, generics
- Real-world performance comparison

#### 4.5 Make decision
- Analyze results
- If dropping LLVM: remove LLVM codegen, simplify feature flags, update docs
- If keeping both: document when to use each backend

### Files to create/modify
- `scripts/bench.sh` -- benchmark runner
- `mux-compiler/Cargo.toml` -- if dropping LLVM, remove `inkwell` + LLVM features
- `mux-compiler/src/codegen/` -- if dropping LLVM, delete entire directory
- `mux-compiler/src/main.rs` -- simplify to single backend if dropping LLVM

---

## Implementation Order

1. **Phase 0** -- Runtime cleanup (critical path, unblocks everything)
2. **Phase 1, tasks 1.1-1.4** -- Cranelift scaffolding + types + runtime declarations
3. **Phase 1, tasks 1.5-1.13** -- Incrementally port expressions/statements/functions/memory/classes/generics
4. **Phase 1, tasks 1.14-1.15** -- Wire up CLI + basic test pass
5. **Phase 4** -- Benchmark Cranelift vs LLVM (before WASM packaging, to learn if we drop LLVM)
6. **Phase 2** -- WASM packaging
7. **Phase 3** -- Full integration testing

## Success Criteria

- [ ] `cargo build` produces a working native compiler (LLVM or Cranelift depending on decision in Phase 4)
- [ ] All 79 test scripts produce identical output with Cranelift backend
- [ ] Benchmark data collected: compilation time + execution time across all workload types
- [ ] Decision documented: whether to keep or drop LLVM backend
- [ ] `cargo build --target wasm32-unknown-unknown --features backend-cranelift` produces a working WASM module
- [ ] Website playground loads WASM module and compiles/runs Mux code in-browser
- [ ] `cargo clippy --all-targets --all-features -- -D warnings` passes
