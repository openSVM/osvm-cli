# OVSM Parser Fix - Detailed Implementation Plan

## Problem Summary

**Current Issue:** IF-THEN-ELSE statements inside WHILE/FOR loops cause infinite loops or incorrect execution.

**Root Cause:** The parser's THEN/ELSE block parsers consume statements that should belong to the parent loop block, preventing loop increment/exit conditions from executing.

## Investigation Findings

### What Works ✅
1. **Lexer INDENT/DEDENT Emission**: The scanner correctly emits INDENT/DEDENT tokens via `handle_indentation()` (scanner.rs:393-472)
2. **DEDENT Recognition**: The parser's `is_end_of_block()` function recognizes DEDENT as a block-ending token (parser.rs:744)
3. **Simple Nested Blocks**: Single-level IF-THEN-ELSE works fine
4. **Loops Without Conditionals**: WHILE/FOR loops work correctly when they don't contain IF-THEN-ELSE

### What Doesn't Work ❌
1. **IF-THEN-ELSE inside WHILE loops**: ELSE block consumes statements beyond its scope
2. **IF-THEN-ELSE inside FOR loops**: Same issue as WHILE
3. **Nested IF-THEN-ELSE in loops**: Compounding of the problem

## Implementation Plan

### Phase 1: Parser Block Consumption Fix (High Priority)

**Objective:** Make THEN/ELSE block parsers properly respect DEDENT tokens

**Files to Modify:**
- `crates/ovsm/src/parser/mod.rs`

**Changes Required:**

1. **Update `parse_if_statement()` method** (around line 1200-1300)
   ```rust
   // Current problematic code in ELSE block parsing:
   fn parse_else_block(&mut self) -> Result<Vec<Statement>> {
       self.expect(TokenKind::Else)?;
       // Missing: Check for DEDENT before consuming statements!

       // Need to add:
       let mut statements = Vec::new();
       while !self.is_at_end() && !self.is_end_of_block() {
           // Stop if we hit a DEDENT that takes us back to parent block level
           if matches!(self.peek().kind, TokenKind::Dedent) {
               break;
           }
           statements.push(self.parse_statement()?);
       }
       Ok(statements)
   }
   ```

2. **Update `parse_then_block()` method** (similar pattern)
   ```rust
   fn parse_then_block(&mut self) -> Result<Vec<Statement>> {
       self.expect(TokenKind::Then)?;

       let mut statements = Vec::new();
       while !self.is_at_end() && !self.is_end_of_block() {
           // Check for DEDENT to stop at correct indentation level
           if matches!(self.peek().kind, TokenKind::Dedent) {
               break;
           }
           statements.push(self.parse_statement()?);
       }
       Ok(statements)
   }
   ```

3. **Add Indentation Level Tracking**
   ```rust
   // Add to Parser struct:
   pub struct Parser {
       tokens: Vec<Token>,
       current: usize,
       indent_level: usize,  // NEW: Track current indentation level
   }

   // Update in parse methods:
   fn parse_while_statement(&mut self) -> Result<Statement> {
       self.expect(TokenKind::While)?;
       let condition = self.parse_expression()?;
       self.expect(TokenKind::Colon)?;

       // Track indentation increase
       self.expect(TokenKind::Indent)?;
       let saved_level = self.indent_level;
       self.indent_level += 1;

       let body = self.parse_block()?;

       // Expect DEDENT back to parent level
       self.expect(TokenKind::Dedent)?;
       self.indent_level = saved_level;

       Ok(Statement::While { condition, body })
   }
   ```

### Phase 2: Enhanced Block-End Detection (Medium Priority)

**Objective:** Make `is_end_of_block()` context-aware

**Implementation:**
```rust
fn is_end_of_block_for_context(&self, context: BlockContext) -> bool {
    match self.peek().kind {
        TokenKind::Dedent => true,
        TokenKind::Else | TokenKind::Elif => {
            // Only end block if we're in a THEN context
            matches!(context, BlockContext::Then)
        }
        TokenKind::Eof => true,
        _ => false,
    }
}

enum BlockContext {
    Then,
    Else,
    Elif,
    WhileBody,
    ForBody,
    TopLevel,
}
```

### Phase 3: Comprehensive Testing (High Priority)

**Test Cases to Add:**

1. **Basic IF-THEN-ELSE in WHILE**
   ```ovsm
   WHILE $done == 0:
       IF $x == 0 THEN
           $x = 1
       ELSE
           $x = 2
       $done = 1  # Must execute!
   ```

2. **Multiple IF-THEN-ELSE in Loop**
   ```ovsm
   WHILE $continue:
       IF $a THEN
           $x = 1
       ELSE
           $x = 2

       IF $b THEN
           $y = 1
       ELSE
           $y = 2

       $continue = 0
   ```

3. **Nested IF-THEN-ELSE in Loop**
   ```ovsm
   WHILE $done == 0:
       IF $outer THEN
           IF $inner THEN
               $x = 1
           ELSE
               $x = 2
       ELSE
           $x = 3
       $done = 1
   ```

4. **FOR loop with IF-THEN-ELSE**
   ```ovsm
   FOR $i IN [1..10]:
       IF $i == 5 THEN
           LOG(message: "Five")
       ELSE
           LOG(message: "Other")
       LOG(message: "After IF")
   ```

### Phase 4: Token Stream Validation (Low Priority)

**Objective:** Add debug tooling to validate token streams

**Implementation:**
```rust
#[cfg(test)]
fn validate_token_stream(tokens: &[Token]) -> Result<()> {
    let mut indent_stack = vec![0];

    for (i, token) in tokens.iter().enumerate() {
        match &token.kind {
            TokenKind::Indent => {
                // Verify INDENT follows a colon
                if i > 0 {
                    assert!(matches!(tokens[i-1].kind, TokenKind::Colon),
                        "INDENT at {} not preceded by colon", i);
                }
                indent_stack.push(indent_stack.last().unwrap() + 1);
            }
            TokenKind::Dedent => {
                assert!(indent_stack.len() > 1,
                    "DEDENT at {} with empty stack", i);
                indent_stack.pop();
            }
            _ => {}
        }
    }

    assert_eq!(indent_stack.len(), 1,
        "Unbalanced INDENT/DEDENT: stack = {:?}", indent_stack);
    Ok(())
}
```

## Implementation Timeline

### Week 1: Core Parser Fixes
- [ ] Day 1-2: Implement Phase 1 (block consumption fix)
- [ ] Day 3-4: Add indentation level tracking
- [ ] Day 5: Code review and refinement

### Week 2: Testing & Validation
- [ ] Day 1-2: Implement Phase 3 (comprehensive tests)
- [ ] Day 3-4: Fix any discovered issues
- [ ] Day 5: Regression testing

### Week 3: Polish & Documentation
- [ ] Day 1-2: Implement Phase 2 (enhanced detection)
- [ ] Day 3: Implement Phase 4 (validation tooling)
- [ ] Day 4-5: Documentation updates

## Testing Strategy

### Unit Tests
```bash
cd crates/ovsm
cargo test --lib test_if_in_while
cargo test --lib test_if_in_for
cargo test --lib test_nested_if_in_loop
```

### Integration Tests
```bash
cargo test --test ovsm_integration_tests
```

### Manual Testing
```bash
# Test all pumpfun scripts
./target/release/osvm ovsm run pumpfun_5min_fixed.ovsm
./target/release/osvm ovsm run /tmp/pumpfun_1min.ovsm
./target/release/osvm ovsm run /tmp/pumpfun_30sec.ovsm
```

## Success Criteria

1. ✅ All pumpfun scripts execute without hanging
2. ✅ Loop increment/exit conditions execute correctly
3. ✅ IF-THEN-ELSE blocks respect parent loop scope
4. ✅ All existing tests continue to pass
5. ✅ New tests for nested blocks pass
6. ✅ No performance regression (< 5% slowdown)

## Rollback Plan

If the fix introduces regressions:
1. Revert parser changes
2. Keep test additions for future attempts
3. Update CLAUDE.md with findings
4. Consider alternative approaches (see below)

## Alternative Approaches (If Main Plan Fails)

### Option A: Explicit Block Terminators
Add optional ENDWHILE, ENDIF keywords:
```ovsm
WHILE $done == 0:
    IF $x == 0 THEN
        $x = 1
    ENDIF
    $done = 1
ENDWHILE
```

### Option B: Brace-Based Syntax
Switch to explicit braces for blocks:
```ovsm
WHILE $done == 0 {
    IF $x == 0 THEN {
        $x = 1
    } ELSE {
        $x = 2
    }
    $done = 1
}
```

### Option C: Parser Rewrite
Complete parser rewrite using recursive descent with explicit state machine for indentation levels.

## Related Issues

- Parser bug documented in CLAUDE.md
- OVSM_INDENT_DEDENT_IMPLEMENTATION_PLAN.md (previous attempt)
- Git commit: "docs: add critical parser bug report for IF-THEN-ELSE in loops"

## References

- Parser implementation: `crates/ovsm/src/parser/mod.rs`
- Scanner implementation: `crates/ovsm/src/lexer/scanner.rs`
- Token definitions: `crates/ovsm/src/lexer/token.rs`
- Test suite: `tests/ovsm_integration_tests.rs`

---

**Status:** READY FOR IMPLEMENTATION
**Priority:** HIGH (blocking pumpfun scripts)
**Estimated Effort:** 2-3 weeks
**Risk Level:** MEDIUM (requires careful testing)
