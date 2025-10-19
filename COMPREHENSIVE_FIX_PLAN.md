# Comprehensive OVSM Parser Fix Plan

**Date:** 2025-10-19
**Status:** READY FOR IMPLEMENTATION
**Priority:** CRITICAL

---

## Executive Summary

After deep investigation, I've discovered that **all previous documentation about this bug is incorrect**. The existing plans claim:
- ✅ Lexer emits INDENT/DEDENT tokens (scanner.rs:393-472)
- ✅ Parser recognizes DEDENT tokens (parser.rs:744)
- ❌ Parser just needs to check for DEDENT before consuming

**THE TRUTH:**
- ❌ **INDENT/DEDENT tokens DO NOT EXIST in TokenKind enum** (token.rs)
- ❌ **Lexer has NO indentation handling code** (`handle_indentation()` doesn't exist)
- ❌ **Parser checks for non-existent tokens** (parser.rs:235, 305 check `TokenKind::Dedent`)

The OVSM language claims to support Python-style indentation-based blocks, but the infrastructure for this **has never been implemented**. The parser works for simple cases using keyword-based heuristics, but breaks down with nested structures.

---

## Root Cause Analysis

### Current Implementation
OVSM supports THREE block styles:
1. **C-style braces**: `WHILE condition { statements }`
2. **Python-style indentation**: `WHILE condition: <indent> statements <dedent>`
3. **Explicit terminators**: `WHILE condition: statements ENDWHILE`

**The C-style and explicit terminator approaches work fine.** The Python-style indentation approach is **fundamentally broken** because:

1. **No tokens to represent indentation changes**
2. **Parser guesses block boundaries** by checking for specific keywords (ELSE, ENDIF, etc.)
3. **Guess fails for nested blocks** - when IF-THEN-ELSE is inside a WHILE loop, the parser doesn't know where the IF ends and the WHILE body continues

### Why Simple Scripts Work
```ovsm
# This works:
IF $x == 0 THEN
    LOG(message: "Zero")
ELSE
    LOG(message: "Not zero")
# Parser sees "end of file" and knows IF must end
```

### Why Nested Scripts Fail
```ovsm
# This fails (infinite loop):
WHILE $done == 0:
    IF $count == 0 THEN
        LOG(message: "First")
    ELSE
        LOG(message: "Second")

    $count = $count + 1  # Parser thinks this belongs to ELSE block!
    $done = 1            # This never executes!
```

**What happens:**
1. Parser enters WHILE loop body (parser.rs:447-453)
2. Parser sees IF statement, enters THEN block (parser.rs:221-255)
3. Parser sees ELSE keyword, enters ELSE block (parser.rs:260-317)
4. **ELSE block parser keeps consuming until it hits a keyword it recognizes** (line 295-306)
5. It sees `$count = ...` and `$done = ...` - these aren't keywords, so it consumes them as part of ELSE
6. WHILE loop body gets empty remaining statements → infinite loop!

---

## The Solution: Implement INDENT/DEDENT Infrastructure

We need to implement **proper indentation-based parsing** similar to Python. This is a 3-phase implementation:

### Phase 1: Add INDENT/DEDENT Tokens (Week 1)

#### Step 1.1: Update Token Types
**File:** `crates/ovsm/src/lexer/token.rs`

```rust
// Add after line 211 (after Newline):
/// Indentation increase (Python-style)
Indent,
/// Indentation decrease (Python-style)
Dedent,
```

#### Step 1.2: Implement Scanner Indentation Tracking
**File:** `crates/ovsm/src/lexer/scanner.rs`

Add fields to Scanner struct (after line 17):
```rust
pub struct Scanner {
    // ... existing fields ...
    /// Stack of indentation levels for Python-style blocks
    indent_stack: Vec<usize>,
    /// Pending DEDENT tokens to emit
    pending_dedents: usize,
}
```

Update `Scanner::new()` (after line 29):
```rust
Scanner {
    // ... existing fields ...
    indent_stack: vec![0],  // Start with base indentation level
    pending_dedents: 0,
}
```

Add indentation handling method (new method around line 200):
```rust
/// Handle indentation changes and emit INDENT/DEDENT tokens
fn handle_indentation(&mut self, new_indent: usize) -> Result<()> {
    let current_indent = *self.indent_stack.last().unwrap();

    if new_indent > current_indent {
        // Indentation increased - emit INDENT
        self.indent_stack.push(new_indent);
        self.add_token(TokenKind::Indent);
    } else if new_indent < current_indent {
        // Indentation decreased - emit DEDENT(s)
        while let Some(&level) = self.indent_stack.last() {
            if level <= new_indent {
                break;
            }
            self.indent_stack.pop();
            self.pending_dedents += 1;
        }

        // Verify we landed on a valid indentation level
        if self.indent_stack.last() != Some(&new_indent) {
            return Err(Error::ScanError(format!(
                "Indentation error at line {}: {} spaces doesn't match any outer level",
                self.line, new_indent
            )));
        }

        // Emit all pending DEDENT tokens
        for _ in 0..self.pending_dedents {
            self.add_token(TokenKind::Dedent);
        }
        self.pending_dedents = 0;
    }
    // If new_indent == current_indent, do nothing (same level)

    Ok(())
}

/// Calculate indentation level of current line
fn calculate_indentation(&self) -> usize {
    let mut indent = 0;
    let mut pos = self.start;

    while pos < self.source.len() {
        match self.source[pos] {
            ' ' => indent += 1,
            '\t' => indent += 4,  // Tab = 4 spaces
            _ => break,
        }
        pos += 1;
    }

    indent
}
```

Modify `scan_token()` to handle newlines with indentation (around line 50):
```rust
fn scan_token(&mut self) -> Result<()> {
    let c = self.advance();

    match c {
        '\n' => {
            // Emit newline token
            self.add_token(TokenKind::Newline);
            self.line += 1;
            self.column = 1;

            // Skip blank lines and comments
            while self.peek() == '\n' || self.peek() == '#' {
                if self.peek() == '\n' {
                    self.advance();
                    self.line += 1;
                    self.column = 1;
                } else {
                    // Skip comment line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
            }

            // Now check indentation of next non-blank line (if not EOF)
            if !self.is_at_end() {
                let indent = self.calculate_indentation();
                self.handle_indentation(indent)?;
            } else {
                // At EOF, emit all remaining DEDENTs
                while self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    self.add_token(TokenKind::Dedent);
                }
            }
        }
        // ... rest of existing scan_token() logic ...
    }

    Ok(())
}
```

Also emit final DEDENTs in `scan_tokens()` (around line 45):
```rust
pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
    while !self.is_at_end() {
        self.start = self.current;
        self.scan_token()?;
    }

    // Emit remaining DEDENTs to return to base level
    while self.indent_stack.len() > 1 {
        self.indent_stack.pop();
        self.add_token(TokenKind::Dedent);
    }

    self.tokens.push(Token::new(
        TokenKind::Eof,
        String::new(),
        self.line,
        self.column,
    ));

    Ok(self.tokens.clone())
}
```

#### Step 1.3: Add Scanner Tests
**File:** `crates/ovsm/src/lexer/scanner.rs` (in tests module)

```rust
#[test]
fn test_indentation_tokens() {
    let source = r#"
IF true THEN
    $x = 1
    $y = 2
"#;
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Expected: IF, true, THEN, NEWLINE, INDENT, $x, =, 1, NEWLINE, ...
    // Find INDENT token
    let has_indent = tokens.iter().any(|t| matches!(t.kind, TokenKind::Indent));
    assert!(has_indent, "Should have INDENT token");
}

#[test]
fn test_dedent_tokens() {
    let source = r#"
WHILE true:
    $x = 1
$y = 2
"#;
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Should have DEDENT before $y
    let has_dedent = tokens.iter().any(|t| matches!(t.kind, TokenKind::Dedent));
    assert!(has_dedent, "Should have DEDENT token");
}

#[test]
fn test_nested_indentation() {
    let source = r#"
WHILE $done == 0:
    IF $x == 0 THEN
        $a = 1
    ELSE
        $b = 2
    $done = 1
"#;
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Print tokens for debugging
    for (i, token) in tokens.iter().enumerate() {
        println!("{}: {:?}", i, token.kind);
    }

    // Count INDENTs and DEDENTs - should be balanced
    let indent_count = tokens.iter().filter(|t| matches!(t.kind, TokenKind::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t.kind, TokenKind::Dedent)).count();
    assert_eq!(indent_count, dedent_count, "INDENT/DEDENT should be balanced");
}
```

---

### Phase 2: Update Parser to Use INDENT/DEDENT (Week 2)

#### Step 2.1: Fix IF Statement Parsing
**File:** `crates/ovsm/src/parser/mod.rs`

Update THEN block parsing (around line 220-255):
```rust
// After THEN keyword, expect either brace or INDENT
if self.match_token(&TokenKind::LeftBrace) {
    // C-style brace handling (keep existing logic)
    // ...
} else {
    // Python-style: expect INDENT after THEN
    self.consume(TokenKind::Indent, "Expected INDENT after THEN")?;
    self.skip_newlines();

    let mut statements = Vec::new();
    // Consume statements until we hit DEDENT
    while !self.check(&TokenKind::Dedent) && !self.is_at_end() {
        statements.push(self.statement()?);
        self.skip_newlines();
    }

    // Consume the DEDENT that ends THEN block
    self.consume(TokenKind::Dedent, "Expected DEDENT to end THEN block")?;

    statements
}
```

Update ELSE block parsing (around line 283-317):
```rust
} else if self.match_token(&TokenKind::Else) {
    self.advance();
    self.skip_newlines();

    if self.match_token(&TokenKind::LeftBrace) {
        // C-style brace handling (keep existing logic)
        // ...
    } else {
        // Python-style: expect INDENT after ELSE
        self.consume(TokenKind::Indent, "Expected INDENT after ELSE")?;
        self.skip_newlines();

        let mut statements = Vec::new();
        // Consume statements until we hit DEDENT
        while !self.check(&TokenKind::Dedent) && !self.is_at_end() {
            statements.push(self.statement()?);
            self.skip_newlines();
        }

        // Consume the DEDENT that ends ELSE block
        self.consume(TokenKind::Dedent, "Expected DEDENT to end ELSE block")?;

        Some(statements)
    }
}
```

#### Step 2.2: Fix WHILE Loop Parsing
**File:** `crates/ovsm/src/parser/mod.rs`

Update WHILE body parsing (around line 442-455):
```rust
} else {
    // Python-style: WHILE condition: statements
    self.consume(TokenKind::Colon, "Expected ':' after WHILE condition")?;
    self.skip_newlines();

    // Expect INDENT to start loop body
    self.consume(TokenKind::Indent, "Expected INDENT after WHILE:")?;
    self.skip_newlines();

    let mut statements = Vec::new();
    // Consume statements until DEDENT
    while !self.check(&TokenKind::Dedent) && !self.is_at_end() {
        statements.push(self.statement()?);
        self.skip_newlines();
    }

    // Consume DEDENT that ends loop body
    self.consume(TokenKind::Dedent, "Expected DEDENT to end WHILE block")?;

    statements
}
```

#### Step 2.3: Fix FOR Loop Parsing
**File:** `crates/ovsm/src/parser/mod.rs`

Update FOR body parsing (around line 498-512) - same pattern as WHILE.

#### Step 2.4: Remove Obsolete Code
**File:** `crates/ovsm/src/parser/mod.rs`

Delete or deprecate:
- `is_end_of_block()` method (line 1241-1266) - no longer needed
- `is_end_of_loop_block()` method (line 1268-1276) - no longer needed

These heuristic methods are replaced by explicit INDENT/DEDENT handling.

---

### Phase 3: Comprehensive Testing (Week 3)

#### Step 3.1: Unit Tests
**File:** `crates/ovsm/src/parser/mod.rs` (tests module)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_if_in_while_loop() {
        let source = r#"
$done = 0
$count = 0
WHILE $done == 0:
    IF $count == 0 THEN
        $count = 1
    ELSE
        $count = 2
    $done = 1
RETURN $done
"#;
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok(), "Should parse IF inside WHILE without error");
    }

    #[test]
    fn test_multiple_if_in_loop() {
        let source = r#"
$x = 0
FOR $i IN [1..3]:
    IF $i == 1 THEN
        $x = $x + 1

    IF $i == 2 THEN
        $x = $x + 10
RETURN $x
"#;
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok(), "Should handle multiple IFs in FOR loop");
    }

    #[test]
    fn test_nested_if_in_loop() {
        let source = r#"
WHILE $done == 0:
    IF $outer THEN
        IF $inner THEN
            $x = 1
        ELSE
            $x = 2
    ELSE
        $x = 3
    $done = 1
"#;
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok(), "Should handle nested IF in WHILE loop");
    }
}
```

#### Step 3.2: Integration Tests
**File:** `tests/ovsm_integration_tests.rs`

Add test case:
```rust
#[test]
fn test_ovsm_if_in_while_execution() {
    // Create test script
    let script = r#"
$count = 0
$iterations = 0
WHILE $count < 3:
    IF $count == 0 THEN
        LOG(message: "First")
    ELSE
        LOG(message: "Other")

    $count = $count + 1
    $iterations = $iterations + 1

RETURN $iterations
"#;

    let script_path = "/tmp/test_if_in_while.ovsm";
    std::fs::write(script_path, script).unwrap();

    // Run script
    let output = std::process::Command::new("cargo")
        .args(&["run", "--bin", "osvm", "--", "ovsm", "run", script_path])
        .output()
        .expect("Failed to run command");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should complete (not timeout) and return 3
    assert!(stdout.contains("3"), "Should complete 3 iterations: {}", stdout);
}
```

#### Step 3.3: Real-World Script Testing
Test with actual pumpfun scripts:
```bash
cargo run --bin osvm -- ovsm run pumpfun_5min_simple.ovsm
cargo run --bin osvm -- ovsm run pumpfun_5min_correct.ovsm
# Should complete without infinite loops
```

---

## Implementation Timeline

### Week 1: Lexer Infrastructure
- **Days 1-2:** Add INDENT/DEDENT tokens to TokenKind enum
- **Days 3-4:** Implement Scanner indentation tracking and emission
- **Day 5:** Write and debug scanner unit tests

### Week 2: Parser Updates
- **Days 1-2:** Update IF statement parsing (THEN/ELSE blocks)
- **Days 3-4:** Update WHILE and FOR loop parsing
- **Day 5:** Remove obsolete block-detection heuristics

### Week 3: Testing & Validation
- **Days 1-2:** Write comprehensive unit and integration tests
- **Day 3:** Test with real pumpfun scripts
- **Days 4-5:** Fix any discovered issues, regression testing

---

## Success Criteria

✅ **Lexer emits INDENT/DEDENT tokens** correctly for all indentation changes
✅ **Parser consumes INDENT/DEDENT tokens** in all block contexts
✅ **All existing tests pass** (no regressions)
✅ **New tests pass** for IF-in-WHILE, nested IFs, multiple IFs
✅ **Pumpfun scripts complete** without infinite loops
✅ **Performance acceptable** (< 10% slowdown from token overhead)

---

## Rollback Plan

If the implementation fails or causes regressions:

1. **Revert commits** to restore original (broken but predictable) behavior
2. **Keep test additions** for next attempt
3. **Document findings** in this file
4. **Consider alternative**: Add explicit ENDIF/ENDWHILE/ENDFOR requirement

---

## Alternative Approaches (If Main Plan Fails)

### Option A: Require Explicit Terminators
Force users to write:
```ovsm
WHILE $done == 0:
    IF $x == 0 THEN
        $a = 1
    ENDIF
    $done = 1
ENDWHILE
```

**Pros:** Easier to implement, clear block boundaries
**Cons:** Verbose, defeats purpose of Python-style syntax

### Option B: Require Braces for Nested Blocks
Keep Python-style for top-level, require braces for nested:
```ovsm
WHILE $done == 0:
    IF $x == 0 THEN {
        $a = 1
    } ELSE {
        $b = 2
    }
    $done = 1
```

**Pros:** Hybrid approach, explicit where needed
**Cons:** Inconsistent syntax rules

### Option C: Complete Parser Rewrite
Rewrite parser from scratch using modern parsing techniques (Pratt parser, etc.)

**Pros:** Clean slate, better architecture
**Cons:** Massive effort (3+ months), high risk

---

## Key Files to Modify

1. **`crates/ovsm/src/lexer/token.rs`** - Add Indent/Dedent to TokenKind enum
2. **`crates/ovsm/src/lexer/scanner.rs`** - Implement indentation tracking
3. **`crates/ovsm/src/parser/mod.rs`** - Update all block parsing logic
4. **`tests/ovsm_integration_tests.rs`** - Add comprehensive tests

---

## References

- OVSM language spec: `crates/ovsm/README.md`
- Python indentation rules: https://docs.python.org/3/reference/lexical_analysis.html#indentation
- Existing (incorrect) plans:
  - `OVSM_PARSER_FIX_IMPLEMENTATION_PLAN.md` (claims infrastructure exists)
  - `PARSER_FIX_INVESTIGATION_SUMMARY.md` (based on wrong assumptions)
  - `PARSER_INVESTIGATION_SUMMARY.md` (outdated)

---

## Status Update - What Previous Plans Got Wrong

All previous investigation documents claim:
> ✅ "Lexer correctly emits INDENT/DEDENT tokens via `handle_indentation()` (scanner.rs:393-472)"

**This is completely false.** Line 393 of scanner.rs starts the TEST MODULE, not an indentation handler. The line numbers are from outdated documentation that never matched reality.

The parser code at lines 235 and 305 checks for `TokenKind::Dedent`, but this token **doesn't exist**. These checks were likely added as an attempted fix, but can never succeed because the lexer never emits these tokens.

**The entire INDENT/DEDENT infrastructure must be built from scratch.**

---

**STATUS:** COMPREHENSIVE PLAN READY
**PRIORITY:** CRITICAL
**ESTIMATED EFFORT:** 3 weeks (1 week per phase)
**RISK LEVEL:** MEDIUM-HIGH (major infrastructure addition)
**CONFIDENCE:** HIGH (problem is now fully understood)
