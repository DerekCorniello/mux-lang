# Implementation Plan: Map Index Access and Assignment

## Goal
Enable Pythonic `[]` indexing syntax for maps with both read and write operations, while maintaining type safety and proper error handling.

## Design Decisions (Confirmed)

1. **Error Message:** Include the actual key value: `"Key 'Charlie' not found in map"`
2. **Type Mismatch:** Caught at compile time (semantics layer)
3. **Empty Map Access:** Runtime error (can't determine keys at compile time)

## Implementation Phases

### Phase 1: Semantics Layer - Type Checking for Map Index Access

**File:** `mux-compiler/src/semantics/mod.rs`

**1.1 Update `get_expression_type()` (line ~840)**
Extend `ListAccess` match arm to handle:
- `Type::List(elem_type)` → return `elem_type`
- `Type::Map(key_type, value_type)` → return `value_type`
- `Type::EmptyMap` → error with helpful message
- Other types → error mentioning both lists and maps

**1.2 Update `analyze_expression()` (line ~3173)**
Extend `ListAccess` validation:
- For lists: verify index is `Int` type (existing logic)
- For maps: verify index type matches map's key type
- Provide helpful error messages for type mismatches

**1.3 Add Assignment Type Checking (line ~1175)**
For `BinaryOp::Assignment` when LHS is `ListAccess`:
- Get target collection type (list or map)
- Verify RHS type matches element type (for lists) or value type (for maps)
- Error at compile time if types don't match

### Phase 2: Code Generation - Reading Maps with Error Handling

**File:** `mux-compiler/src/codegen/expressions.rs`

**2.1 Extend `ListAccess` code generation (line ~2436)**
Detect if target is list or map type:
- **For lists:** Keep existing `mux_list_get_value` + null check + bounds error
- **For maps:**
  - Call `mux_map_get` (returns `Optional`)
  - Check if Optional is None
  - If None: generate error message with key value → print → exit(1)
  - If Some: extract value using `extract_value_from_ptr`

**Implementation details:**
- Use `format!("Key '{}' not found in map", key_value)` for error message
- Use existing error block pattern (similar to list bounds checking)
- Call `mux_optional_is_some` to check for key existence

### Phase 3: Code Generation - Index Assignment

**File:** `mux-compiler/src/codegen/expressions.rs`

**3.1 Extend `Binary` expression assignment handling (line ~1175)**

Add case for when left-hand side is `ExpressionKind::ListAccess`:
- Extract collection type from semantic analyzer
- For **list assignment**:
  - Call `mux_value_get_list` to extract raw list pointer
  - Call `mux_list_set(index, value)` to set the element
- For **map assignment**:
  - Call `mux_value_get_map` to extract raw map pointer (verify this exists, or add it)
  - Call `mux_map_put(key, value)` to insert/update

**Runtime Dependencies:**
- `mux_list_set` - ✓ exists in runtime
- `mux_map_put` - ✓ exists in runtime
- `mux_value_get_map` - need to verify or implement

### Phase 4: Runtime Additions (if needed)

**File:** `mux-runtime/src/map.rs` (if `mux_value_get_map` doesn't exist)

Check if we need to add `mux_value_get_map` function to extract map pointer from boxed Value, similar to `mux_value_get_list`.

### Phase 5: Testing

**Test file:** `test_scripts/collections.mux`

Expected behavior after implementation:
```mux
auto avgs = {"Alice": 90, "Bob": 85}
print(avgs["Bob"])              // Output: 85
avgs["Bob"] = 100               // Update succeeds
print(avgs["Bob"])              // Output: 100
print(avgs.get("Charlie"))      // Output: Optional::none (safe)
print(avgs["Charlie"])          // Runtime error: "Key 'Charlie' not found in map"

// Type mismatch (compile-time error)
// avgs["Bob"] = "string"       // Error: expected Int, found String
```

## Files to Modify

1. `mux-compiler/src/semantics/mod.rs` - Type checking and validation
2. `mux-compiler/src/codegen/expressions.rs` - Code generation for read/write
3. `mux-runtime/src/map.rs` - Add `mux_value_get_map` if needed

## Dependencies

- `mux_list_set` - ✓ Available in runtime
- `mux_map_put` - ✓ Available in runtime  
- `mux_map_get` - ✓ Available in runtime
- `mux_value_get_list` - ✓ Available in runtime
- `mux_value_get_map` - Need to verify
- `mux_optional_is_some` - ✓ Available in runtime (for checking map result)

## Testing Commands

```bash
cargo build
cargo run -- run test_scripts/collections.mux
cargo clippy
```

## Risk Assessment

- **Low Risk:** Changes are additive, existing list functionality preserved
- **Medium Risk:** Map access error handling requires proper LLVM block generation
- **Mitigation:** Follow existing patterns from list access error handling
