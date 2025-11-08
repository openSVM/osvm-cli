# OSVM Build Fix - Comprehensive Plan & Specification

## Executive Summary

The OSVM CLI project has 6 compilation errors preventing builds. These errors stem from recent JSON support additions that use outdated API references. The fixes are straightforward but critical for restoring functionality and verifying our uppercase function name prevention works.

## Current Build Error Analysis

### 1. RuntimeError Missing (3 occurrences)
- **Location**: `crates/ovsm/src/runtime/lisp_evaluator.rs`
- **Lines**: 3169, 3216, 3278-3279
- **Issue**: `Error::RuntimeError` variant doesn't exist in the Error enum
- **Root Cause**: RuntimeError was likely removed during error handling refactoring
- **Fix Strategy**: Use appropriate existing error variants

### 2. Lambda vs Function (1 occurrence)
- **Location**: `crates/ovsm/src/runtime/lisp_evaluator.rs:3277`
- **Issue**: Code uses `Value::Lambda(_)` but enum has `Value::Function { .. }`
- **Root Cause**: Value enum was renamed from Lambda to Function (Common Lisp terminology)
- **Fix Strategy**: Update to `Value::Function { .. }` pattern

### 3. String Arc Mismatch (2 occurrences)
- **Location**: `crates/ovsm/src/runtime/lisp_evaluator.rs`
- **Lines**: 3218, 3236
- **Issue**: Code uses `Value::String(Arc::new(s))` but String doesn't use Arc
- **Root Cause**: Value::String takes String directly, not Arc<String>
- **Fix Strategy**: Remove Arc wrapper, use `Value::String(s)`

## Detailed Fix Specifications

### Fix 1: Replace RuntimeError with Appropriate Errors

**Line 3169 - JSON Parse Error:**
```rust
// OLD:
.map_err(|e| Error::RuntimeError(format!("Failed to parse JSON: {}", e)))?;

// NEW:
.map_err(|e| Error::ToolExecutionError {
    tool: "json-parse".to_string(),
    reason: format!("Failed to parse JSON: {}", e),
})?;
```

**Line 3216 - JSON Stringify Error:**
```rust
// OLD:
.map_err(|e| Error::RuntimeError(format!("Failed to stringify JSON: {}", e)))?;

// NEW:
.map_err(|e| Error::ToolExecutionError {
    tool: "json-stringify".to_string(),
    reason: format!("Failed to stringify JSON: {}", e),
})?;
```

**Lines 3278-3279 - Lambda JSON Conversion:**
```rust
// OLD:
return Err(Error::RuntimeError(
    "Cannot convert lambda to JSON".to_string(),
))

// NEW:
return Err(Error::InvalidOperation {
    op: "json-conversion".to_string(),
    left_type: "function".to_string(),
    right_type: "json".to_string(),
})
```

### Fix 2: Update Lambda to Function

**Line 3277:**
```rust
// OLD:
Value::Lambda(_) => {

// NEW:
Value::Function { .. } => {
```

### Fix 3: Remove Arc from String Values

**Line 3218:**
```rust
// OLD:
Ok(Value::String(Arc::new(json_str)))

// NEW:
Ok(Value::String(json_str))
```

**Line 3236:**
```rust
// OLD:
JV::String(s) => Value::String(Arc::new(s)),

// NEW:
JV::String(s) => Value::String(s),
```

## Implementation Tasks

### Phase 1: Fix Compilation Errors
1. ✅ Fix RuntimeError at line 3169 (json-parse)
2. ✅ Fix RuntimeError at line 3216 (json-stringify)
3. ✅ Fix RuntimeError at lines 3278-3279 (lambda conversion)
4. ✅ Fix Value::Lambda to Value::Function at line 3277
5. ✅ Fix String Arc at line 3218
6. ✅ Fix String Arc at line 3236

### Phase 2: Clean Up Warnings
7. ⚠️ Fix 79 unused variable warnings (prefix with underscore)
8. ⚠️ Remove unused imports

### Phase 3: Verification
9. 🧪 Run cargo build --release
10. 🧪 Run full test suite
11. 🧪 Verify uppercase function rejection works
12. 🧪 Test JSON parse/stringify functionality

## Why These Errors Exist

### Historical Context
1. **JSON Support**: Recently added JSON parse/stringify functions
2. **Error Evolution**: Error enum was refactored, RuntimeError removed
3. **Value Refactor**: Lambda → Function rename for Common Lisp compatibility
4. **Arc Optimization**: String values no longer need Arc wrapper (small strings)

### Design Decisions
- **ToolExecutionError**: More specific than RuntimeError, better error categorization
- **InvalidOperation**: Better semantic match for type conversion failures
- **Function naming**: Aligns with Common Lisp terminology (defun, lambda → function)
- **String without Arc**: Strings are already heap-allocated, Arc adds unnecessary overhead

## Testing Strategy

### Unit Tests
```rust
#[test]
fn test_json_parse() {
    let json = r#"{"key": "value"}"#;
    let result = eval_json_parse(json);
    assert!(result.is_ok());
}

#[test]
fn test_json_stringify() {
    let value = Value::object(hashmap!{
        "key".to_string() => Value::String("value".to_string())
    });
    let result = eval_json_stringify(value);
    assert!(result.is_ok());
}

#[test]
fn test_function_not_jsonifiable() {
    let func = Value::Function { /* ... */ };
    let result = value_to_json(func);
    assert!(matches!(result, Err(Error::InvalidOperation { .. })));
}
```

### Integration Tests
1. Test uppercase function rejection in AI agent
2. Test JSON round-trip (parse → modify → stringify)
3. Test error messages are informative
4. Test backwards compatibility with existing OVSM scripts

## Expected Outcomes

### Immediate Benefits
- ✅ Successful compilation
- ✅ Working release binary
- ✅ Uppercase function prevention active
- ✅ JSON operations functional

### Long-term Benefits
- 📊 Better error categorization
- 🔍 More specific error messages
- 🏗️ Cleaner Value enum structure
- 🚀 Performance improvements (no unnecessary Arc)

## Risk Assessment

### Low Risk
- All changes are compile-time fixes
- No algorithmic changes
- No behavior changes for existing functionality
- Test coverage ensures correctness

### Mitigation
- Comprehensive test suite (356 tests)
- Error messages remain informative
- Backwards compatible with existing OVSM scripts

## Timeline

1. **Immediate** (5 mins): Apply all 6 fixes
2. **Quick** (2 mins): Clean up warnings
3. **Verification** (5 mins): Build and test
4. **Documentation** (2 mins): Update comments

Total estimated time: ~15 minutes

## Conclusion

These fixes restore compilation by aligning the JSON support code with the current Value enum and Error enum structures. The changes are minimal, safe, and improve code quality by using more specific error types and cleaner value representations.

The uppercase function prevention we implemented earlier will become active once these fixes are applied and the binary is rebuilt with the updated prompt.