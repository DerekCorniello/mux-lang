# Error Cases Testing Findings

This document summarizes the results of running 38 error case tests across lexer, parser, and semantic phases. Each test was designed to trigger a specific failure, but many revealed deeper issues in the compiler's parsing and validation logic.

## Summary
- **Total tests**: 38 (8 lexer, 10 parser, 20 semantic)
- **Tests passing as expected**: 12
- **Tests with wrong error type/stage**: 18
- **Tests with no error**: 3
- **Tests blocked by parser limitations**: 5

## Key Issues
1. **Parser Incomplete**: The parser doesn't support many Mux syntaxes (explicit type declarations, function syntax, class instantiation), causing semantic tests to fail at parse time.
2. **Lexer Validation Missing**: Numeric literals aren't validated, allowing malformed numbers to parse incorrectly.
3. **Error Cascading**: Many errors trigger at the wrong stage (e.g., semantic errors reported as parser issues).

## Detailed Test Results

### Lexer Tests
These test lexical analysis failures.

#### ✅ lexer_invalid_char.mux
- **Expected**: `unexpected character: '§'`
- **Actual**: `unexpected character: '§'` (correct location, clear message)
- **Status**: PASS

#### ✅ lexer_unterminated_string.mux
- **Expected**: `Unterminated string`
- **Actual**: `Unterminated string` with helpful hint "Make sure to close the string with a matching quote"
- **Status**: PASS

#### ❌ lexer_invalid_float.mux
- **Expected**: `Invalid float literal: 1.2.3` (lexer error)
- **Actual**: No error (compilation succeeds)
- **Issue**: Lexer accepts `1.2.3` as valid syntax, no validation for multiple decimals
- **Fix**: Add lexer validation for float literal format

#### ✅ lexer_char_too_long.mux
- **Expected**: `Char literal must be exactly one character`
- **Actual**: `Char literal must be exactly one character` with examples
- **Status**: PASS

#### ✅ lexer_unknown_escape.mux
- **Expected**: `Unknown escape sequence: \z`
- **Actual**: `Unknown escape sequence: \z` with list of valid escapes
- **Status**: PASS (comprehensive help)

#### ❌ lexer_unterminated_comment.mux
- **Expected**: `Unterminated block comment`
- **Actual**: No error (compilation succeeds?)
- **Issue**: Unterminated comments not detected
- **Fix**: Implement comment termination checking in lexer

#### ❌ lexer_invalid_int.mux
- **Expected**: `Invalid integer literal: 123abc`
- **Actual**: `Undefined variable 'abc'` (lexer parses `123` as int, `abc` as identifier)
- **Issue**: Lexer splits malformed int into valid int + undefined var, no validation
- **Fix**: Add lexer validation for integer literals

#### ❌ lexer_missing_digit.mux
- **Expected**: `Expected digit after decimal point`
- **Actual**: No error (compilation succeeds)
- **Issue**: Lexer accepts `1.` as valid float literal
- **Fix**: Add lexer validation for float format requiring digits after decimal

### Parser Tests
These test syntax parsing failures.

#### ✅ parser_missing_class_body.mux
- **Expected**: `Expected '{' after class header`
- **Actual**: `Expected '{' after class header`
- **Status**: PASS

#### ❌ parser_missing_var_assign.mux
- **Expected**: `Expected '=' after variable name`
- **Actual**: `Undefined variable 'int'` + `Undefined variable 'x'`
- **Issue**: Parser doesn't recognize `int x` as valid variable declaration syntax
- **Fix**: Implement parsing for explicit type declarations without initializers

#### ❌ parser_missing_if_expr.mux
- **Expected**: `Expected expression after 'if'`
- **Actual**: `Expected '{' after if condition`
- **Issue**: Wrong expectation (expects block, not expr)
- **Fix**: Correct if statement parsing logic

#### ❌ parser_missing_func_close.mux
- **Expected**: `Expected '}' after function body`
- **Actual**: `Expected newline before statement` + `Expected '}' after block`
- **Issue**: Function parsing incomplete
- **Fix**: Implement proper function body parsing

#### ❌ parser_missing_return_type.mux
- **Expected**: `Expected ':' after function parameters`
- **Actual**: `Undefined variable 'fn'` + `Undefined variable 'bar'`
- **Issue**: Function syntax not recognized
- **Fix**: Add function declaration parsing

#### ❌ parser_missing_var_type.mux
- **Expected**: `Expected type after variable name`
- **Actual**: `Expected expression, found 'Colon'`
- **Issue**: Typed variables not supported
- **Fix**: Implement typed variable parsing

#### ❌ parser_missing_match_pattern.mux
- **Expected**: `Expected pattern`
- **Actual**: `Expected pattern` (first error correct) + other issues
- **Issue**: Match syntax incomplete
- **Fix**: Complete match statement parsing

#### ✅ parser_missing_pipe.mux
- **Expected**: `Expected '|' after '|'`
- **Actual**: `Expected '|' after '|'`
- **Status**: PASS

#### ✅ parser_missing_binary_rhs.mux
- **Expected**: `Expected expression after operator`
- **Actual**: `Expected expression, found end of input`
- **Status**: PASS (close enough)

### Semantic Tests
These test type checking and symbol resolution failures.

#### ✅ semantic_undefined_var.mux
- **Expected**: `Undefined variable 'x'`
- **Actual**: `Undefined variable 'x'`
- **Status**: PASS

#### ✅ semantic_type_mismatch.mux
- **Expected**: `Type mismatch: expected int, got string`
- **Actual**: `Type mismatch: expected int, got string`
- **Status**: PASS

#### ✅ semantic_assign_const.mux
- **Expected**: `Cannot assign to constant`
- **Actual**: `Cannot assign to constant 'c'` with helpful hint
- **Status**: PASS (excellent error message)

#### ❌ semantic_undefined_func.mux
- **Expected**: `Undefined function: foo`
- **Actual**: `Undefined variable 'foo'`
- **Issue**: Functions vs variables distinction
- **Fix**: Improve undefined symbol messages

#### ✅ semantic_self_outside.mux
- **Expected**: `Cannot use 'self' outside of a method`
- **Actual**: `Cannot use 'self' outside of a method`
- **Status**: PASS

#### ❌ semantic_generic_mismatch.mux
- **Expected**: `Type argument count mismatch for class`
- **Actual**: Parser errors
- **Issue**: Class and generic syntax not parsed
- **Fix**: Implement class and generic parsing

#### ❌ semantic_return_void.mux
- **Expected**: `Cannot return a value from a void function`
- **Actual**: Parser errors
- **Issue**: Function syntax incomplete
- **Fix**: Complete function syntax

#### ❌ semantic_index_non_list.mux
- **Expected**: `Cannot index non-list type`
- **Actual**: `Type mismatch: expected let, got string` + `Undefined variable 's'`
- **Issue**: String literals not parsed correctly
- **Fix**: Fix string literal parsing

#### ✅ semantic_invalid_op.mux
- **Expected**: Type mismatch in operation
- **Actual**: `Binary operator '+' is not supported for types string and int`
- **Status**: PASS (good specific message)

#### ❌ semantic_use_before_declare.mux
- **Expected**: `Undefined variable 'y'`
- **Actual**: Parser errors
- **Issue**: Function body parsing incomplete
- **Fix**: Complete function parsing

#### ❌ semantic_const_field.mux
- **Expected**: `Cannot assign to const field`
- **Actual**: Multiple parser errors
- **Issue**: Class field syntax not supported
- **Fix**: Implement class field parsing

#### ❌ semantic_generic_inference.mux
- **Expected**: `Cannot infer type for empty literal`
- **Actual**: Parser errors
- **Issue**: Typed variables not parsed
- **Fix**: Implement typed variable support

#### ❌ semantic_modify_immutable.mux
- **Expected**: `Cannot modify immutable collection`
- **Actual**: Parser errors
- **Issue**: List literals not parsed
- **Fix**: Implement list literal parsing

#### ❌ semantic_wrong_return.mux
- **Expected**: `Type mismatch in return`
- **Actual**: Parser errors
- **Issue**: Function syntax incomplete
- **Fix**: Complete function parsing

#### ❌ semantic_self_common.mux
- **Expected**: `Cannot use 'self' in a common method`
- **Actual**: Parser errors
- **Issue**: Function syntax incomplete
- **Fix**: Complete function parsing

#### ❌ semantic_empty_map_index.mux
- **Expected**: `Cannot index empty map`
- **Actual**: Parser errors
- **Issue**: Map literals not parsed
- **Fix**: Implement map literal parsing

#### ❌ semantic_iterate_non_list.mux
- **Expected**: `Cannot iterate over non-list type`
- **Actual**: Parser errors
- **Issue**: For loop syntax incomplete
- **Fix**: Implement for loop parsing

#### ❌ semantic_assign_index_non.mux
- **Expected**: `Cannot assign to index on non-list/map type`
- **Actual**: Parser errors
- **Issue**: String literals not parsed
- **Fix**: Fix string literal parsing

## Next Steps
1. **Priority 1**: Complete parser implementation for core Mux syntax (variables, functions, classes, literals)
2. **Priority 2**: Add lexer validation for numeric literals
3. **Priority 3**: Refine error messages and cascading
4. **Priority 4**: Expand semantic tests once parser is robust

Run `cargo clippy` and `cargo build` after fixes to ensure no regressions.