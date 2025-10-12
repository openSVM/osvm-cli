# Bug Report: Comparison Operators Always Return True

## Status
**INVESTIGATING** - Discovered during testing of parser fix

## Summary
All comparison operations (`>`, `<`, `>=`, `<=`) are evaluating to `true` regardless of the actual operand values. This affects IF statements, WHILE loops, and any conditional logic.

## Evidence

### Test Results
```ovsm
# Test 1: Should return "x is smaller"
$x = 1
$y = 5
IF $x > $y THEN
    RETURN "x is bigger"
RETURN "x is smaller"
# Actual: "x is bigger" ❌
```

```ovsm
# Test 2: Should return "1 is smaller"
IF 1 > 5 THEN
    RETURN "1 is bigger"
RETURN "1 is smaller"
# Actual: "1 is bigger" ❌
```

```ovsm
# Test 3: Should return "all small"
FOR $i IN [1..3]:  # [1, 2]
    IF $i > 2 THEN
        RETURN "found big"
RETURN "all small"
# Actual: "found big" ❌
```

All three tests return the wrong branch, suggesting ALL comparisons evaluate to `true`.

## Unit Tests
The `test_comparison_operators` unit test in `runtime::evaluator::tests` **PASSES**, which means:
- Comparisons work correctly in isolation (direct RETURN statements)
- The bug is specific to IF statement condition evaluation
- Likely NOT a bug in `apply_binary_op()`

## Hypothesis
The issue may be in how IF statements evaluate their conditions:
1. **Parser**: IF condition expression might be malformed
2. **Evaluator**: `execute_if_statement()` might be inverting the condition
3. **Value**: `is_truthy()` might be broken for Bool values

## Investigation Steps

### 1. Check Parser Output
Verify the AST structure for a simple IF with comparison:
```rust
let code = "IF 1 > 5 THEN RETURN \"yes\" ELSE RETURN \"no\"";
// Check if condition is parsed correctly as Binary(Gt, Int(1), Int(5))
```

### 2. Check Evaluator Logic
In `src/runtime/evaluator.rs` line 74-88:
```rust
Statement::If { condition, then_branch, else_branch } => {
    let cond_val = self.evaluate_expression(condition)?;
    if cond_val.is_truthy() {
        self.execute_block(then_branch)
    } else if let Some(else_b) = else_branch {
        self.execute_block(else_b)
    } else {
        Ok(ExecutionFlow::Continue)
    }
}
```

Check if:
- `evaluate_expression(condition)` returns correct Bool value
- `is_truthy()` correctly interprets Bool(false) as false

### 3. Check Value::is_truthy()
In `src/runtime/value.rs` line 51-63:
```rust
pub fn is_truthy(&self) -> bool {
    match self {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
        Value::Object(obj) => !obj.is_empty(),
        Value::Range { .. } => true,
    }
}
```

This looks correct - `Bool(b) => *b` should work.

## Test Files Created
- `examples/test_simple_comparison.md` - Demonstrates the bug
- `examples/test_if_condition.md` - Multiple IF condition tests
- `examples/test_break_in_if.md` - BREAK with comparison (returns 0 instead of 15)

## Impact
**CRITICAL** - All conditional logic is broken. Programs cannot make decisions based on comparisons.

## Related
This bug may have existed before the parser fix, or may have been introduced by it. Needs investigation to determine if this is:
1. A regression from removing IF/FOR/WHILE from `is_end_of_block()`
2. A pre-existing bug that was masked
3. A new bug in how IF parses conditions after the fix

## Next Steps
1. Test comparisons on a commit BEFORE the parser fix to see if bug pre-exists
2. Add debug logging to `execute_if_statement()` to see what `cond_val` is
3. Check if the problem is specific to nested IF or affects all IF statements
