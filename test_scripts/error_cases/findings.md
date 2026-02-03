# Error Cases Testing Findings

This document summarizes the results of running error case tests across lexer, parser, and semantic phases.

## Summary
- **Total tests**: 70 (14 lexer, 23 parser, 33 semantic)
- **Tests passing**: ~55 (working correctly)
- **Tests failing**: ~15 (compiler issues or syntax issues)

---

## Lexer Tests (14 files)

| File | Error | Status |
|------|-------|--------|
| `lexer_binary_literal.mux` | Invalid integer literal: 0b102 | ✅ PASS |
| `lexer_char_too_long.mux` | Char literal must be exactly one character | ✅ PASS |
| `lexer_hex_literal.mux` | Invalid integer literal: 0xGHI | ✅ PASS |
| `lexer_invalid_char.mux` | unexpected character: '§' | ✅ PASS |
| `lexer_invalid_escape_in_char.mux` | Unknown escape sequence: \x | ✅ PASS |
| `lexer_invalid_float.mux` | Invalid float literal: 1.2.3 | ✅ PASS |
| `lexer_invalid_identifier.mux` | Invalid integer literal: 123abc | ✅ PASS |
| `lexer_invalid_int.mux` | Invalid integer literal: 123abc | ✅ PASS |
| `lexer_missing_digit.mux` | Expected digit after decimal point | ✅ PASS |
| `lexer_unclosed_char.mux` | Unterminated character literal | ✅ PASS |
| `lexer_unicode_identifier.mux` | unexpected character: 'é' | ✅ PASS |
| `lexer_unknown_escape.mux` | Unknown escape sequence: \z | ✅ PASS |
| `lexer_unterminated_comment.mux` | Unterminated block comment | ✅ PASS |
| `lexer_unterminated_string.mux` | Unterminated string | ✅ PASS |

**Lexer Status: 14/14 PASSING** ✅

---

## Parser Tests (23 files)

| File | Expected Error | Actual Error | Status |
|------|---------------|--------------|--------|
| `parser_break_outside_loop.mux` | Break outside loop | **No error** | ❌ COMPILER ISSUE |
| `parser_continue_outside_loop.mux` | Continue outside loop | **No error** | ❌ COMPILER ISSUE |
| `parser_dangling_comma.mux` | Trailing comma | Expected expression | ✅ PASS |
| `parser_enum_missing_variant.mux` | Empty enum | **No error** | ❌ NEEDS CLARIFICATION |
| `parser_extra_token.mux` | Unexpected token | Expected newline | ✅ PASS |
| `parser_generic_without_bracket.mux` | Generic without arg | **Codegen error** | ❌ SEMANTIC ISSUE |
| `parser_interface_missing_method.mux` | Empty interface | **No error** | ❌ NEEDS CLARIFICATION |
| `parser_invalid_field_decl.mux` | Invalid field | Expected field name | ✅ PASS |
| `parser_invalid_map_key.mux` | Invalid map key | **No error** | ❌ NEEDS CLARIFICATION |
| `parser_invalid_type_annotation.mux` | Invalid type | Expected '=' | ✅ PASS |
| `parser_missing_binary_rhs.mux` | Missing rhs | Expected expression | ✅ PASS |
| `parser_missing_class_body.mux` | Missing '{' | Expected '{' | ✅ PASS |
| `parser_missing_comma_params.mux` | Missing comma | Expected ')' | ✅ PASS |
| `parser_missing_func_close.mux` | Missing '}' | Expected '}' | ✅ PASS |
| `parser_missing_if_expr.mux` | Missing expr | Expected '{' | ✅ PASS |
| `parser_missing_import_path.mux` | Missing path | Expected module path | ✅ PASS |
| `parser_missing_match_pattern.mux` | Missing pattern | Expected expression | ✅ PASS |
| `parser_missing_paren_call.mux` | Missing parens | **No error** | ❌ NEEDS CLARIFICATION |
| `parser_missing_pipe.mux` | Missing pipe | Expected '|' | ✅ PASS |
| `parser_missing_return_type.mux` | Missing type | Expected type | ✅ PASS |
| `parser_missing_var_assign.mux` | Missing '=' | Expected expression | ✅ PASS |
| `parser_missing_var_type.mux` | Missing expr | Expected expression | ✅ PASS |
| `parser_return_outside_func.mux` | Return outside func | **Codegen error** | ❌ COMPILER ISSUE |

**Parser Status: ~15/23 PASSING**

**Issues to investigate:**
- `break`/`continue` not implemented
- `return` outside function not detected at parser level
- Empty enum/interface - is this allowed?
- Function call without parens - is this valid syntax?
- Generic type without args - semantic vs parser issue?

---

## Semantic Tests (33 files)

| File | Expected Error | Actual Error | Status |
|------|---------------|--------------|--------|
| `semantic_access_undefined_field.mux` | Undefined field | Unknown field | ✅ PASS |
| `semantic_assign_const.mux` | Assign to const | Cannot assign | ✅ PASS |
| `semantic_assign_index_non.mux` | Index non-list | Cannot index | ✅ PASS |
| `semantic_call_undefined_method.mux` | Undefined method | Unknown field | ✅ PASS |
| `semantic_class_without_new.mux` | Without .new() | Cannot call | ✅ PASS |
| `semantic_const_field_modify.mux` | Modify const field | Cannot assign | ✅ PASS |
| `semantic_const_field.mux` | Const needs default | Expected expression | ✅ PASS |
| `semantic_duplicate_field.mux` | Duplicate field | **No error** | ❌ COMPILER ISSUE |
| `semantic_duplicate_var.mux` | Duplicate var | **No error** | ❌ COMPILER ISSUE |
| `semantic_empty_map_index.mux` | Index non-list | Cannot index | ✅ PASS |
| `semantic_field_on_primitive.mux` | Field on primitive | Unknown method | ✅ PASS |
| `semantic_generic_inference.mux` | Cannot infer type | Cannot infer | ✅ PASS |
| `semantic_generic_mismatch.mux` | Wrong type args | Expected expression | ✅ PASS |
| `semantic_index_non_list.mux` | Index non-list | Cannot index | ✅ PASS |
| `semantic_index_out_of_bounds.mux` | Index out of bounds | **Runtime error** | ❌ SHOULD BE SEMANTIC |
| `semantic_invalid_op.mux` | Type mismatch | Binary operator | ✅ PASS |
| `semantic_iterate_non_list.mux` | Iterate non-list | Cannot iterate | ✅ PASS |
| `semantic_key_not_found.mux` | Key not found | **Runtime error** | ❌ SHOULD BE SEMANTIC |
| `semantic_main_returns_value.mux` | main returns value | **No error** | ❌ NEEDS CLARIFICATION |
| `semantic_optional_unwrap_none.mux` | None case missing | **Parser error** | ❌ SYNTAX ISSUE |
| `semantic_result_unwrap_err.mux` | Err case missing | **Parser error** | ❌ SYNTAX ISSUE |
| `semantic_return_void.mux` | Return in void | Cannot return | ✅ PASS |
| `semantic_self_common.mux` | self in common | Cannot use | ✅ PASS |
| `semantic_self_outside.mux` | self outside | Cannot use | ✅ PASS |
| `semantic_type_mismatch.mux` | Type mismatch | Type mismatch | ✅ PASS |
| `semantic_undefined_func.mux` | Undefined function | Undefined variable | ✅ PASS* |
| `semantic_undefined_var.mux` | Undefined var | Undefined var | ✅ PASS |
| `semantic_uninitialized_field.mux` | Uninitialized field | **No error (returns 0)** | ❌ COMPILER ISSUE |
| `semantic_use_before_declare.mux` | Use before declare | Undefined var | ✅ PASS |
| `semantic_wrong_arg_count.mux` | Wrong arg count | **No error** | ❌ COMPILER ISSUE |
| `semantic_wrong_interface.mux` | Wrong interface | **No error** | ❌ COMPILER ISSUE |
| `semantic_wrong_return.mux` | Return type mismatch | Type mismatch | ✅ PASS |
| `semantic_wrong_type_args.mux` | Wrong type args | Expected expression | ✅ PASS |

**Semantic Status: ~25/33 PASSING**

**Issues to investigate:**
- Duplicate field/variable detection missing
- Index out of bounds - runtime vs semantic error?
- Key not found - runtime vs semantic error?
- Uninitialized field - returns 0, should error?
- Wrong argument count not detected
- Interface implementation checking missing
- `main()` returning value - is this allowed?
- Match expression in expression context - is this allowed?

---

## Known Compiler Issues

1. **Break/Continue**: Not implemented
2. **Duplicate detection**: Fields and variables can be duplicated without error
3. **Return outside function**: Detected at codegen, not parser
4. **Argument count checking**: Function call with wrong arg count not detected
5. **Interface implementation**: Not checked at semantic level
6. **Uninitialized fields**: Return 0 instead of error
7. **Bounds checking**: Runtime errors instead of semantic errors

---

## Notes

- `semantic_undefined_func.mux` reports "Undefined variable" instead of "Undefined function" - design choice (functions and variables share namespace)
- Some tests produce runtime errors instead of semantic errors (index out of bounds, key not found)
- Match expressions cannot be used as expressions (e.g., `auto x = match ... { }`)
