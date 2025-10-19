# CRITICAL: OVSM Parser Bug - IF-THEN-ELSE in WHILE Loops

## Summary

The OVSM parser has a critical bug that prevents IF-THEN-ELSE statements from working correctly when nested inside WHILE loops. The parser incorrectly determines where the ELSE block ends, causing it to either:
1. Consume statements that belong to the parent WHILE loop body, OR
2. Prematurely terminate the WHILE loop after the ELSE block

This bug affects BOTH Python-style (colon-based) and C-style (brace-based) WHILE loop syntax.

## Impact

**Severity: CRITICAL** - This bug makes common programming patterns impossible in OVSM:
- Cannot use IF-THEN-ELSE for conditional logic inside loops
- Pagination logic with conditional RPC calls fails
- Scripts enter infinite loops or silently skip code
- No workaround exists except completely avoiding IF-THEN-ELSE in loops

## Root Cause

**Location:** `crates/ovsm/src/parser/mod.rs`, lines 270-287

The ELSE block parser uses this termination logic:
```rust
while !self.check(&TokenKind::RightBrace)
    && !self.check(&TokenKind::Else)
    && !self.check(&TokenKind::EndIf)
    && !self.check(&TokenKind::End)
    && !self.check(&TokenKind::Branch)
    && !self.check(&TokenKind::Catch)
    && !self.check(&TokenKind::WaitAll)
    && !self.check(&TokenKind::WaitAny)
    && !self.check(&TokenKind::Race)
    && !self.check(&TokenKind::Return)
    && !self.is_at_end()
{
    statements.push(self.statement()?);
    self.skip_newlines();
}
```

**The Problem:** The parser continues collecting statements for the ELSE block until it sees one of the above keywords. However, statements like `LOG()`, variable assignments (`$x = ...`), and other common statements do NOT match these keywords. This causes the ELSE block parser to consume statements that actually belong to the parent WHILE loop body.

The parser lacks proper **block boundary detection** for nested contexts. It cannot distinguish between:
- A statement that belongs to the ELSE block
- A statement that belongs to the parent WHILE loop body

## Test Cases

### Test Case 1: Simple IF-THEN-ELSE in WHILE (BROKEN)

```ovsm
$x = 0
WHILE $x < 2:
    LOG(message: "Before IF")
    IF $x == 0 THEN
        LOG(message: "x is 0")
    ELSE
        LOG(message: "x is 1")
    LOG(message: "After IF")  # <-- NEVER EXECUTES
    $x = $x + 1               # <-- NEVER EXECUTES

LOG(message: "Done")
RETURN $x
```

**Expected Output:**
```
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "After IF"
[LOG] "Before IF"
[LOG] "x is 1"
[LOG] "After IF"
[LOG] "Done"
Result: 2
```

**Actual Output:**
```
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "Before IF"
... (infinite loop, never logs "After IF")
```

**Bug Manifestation:** The ELSE block parser consumes `LOG(message: "After IF")` and `$x = $x + 1` as part of the ELSE block, leaving nothing in the WHILE loop body after the IF statement. The loop restarts immediately after the IF completes, without incrementing `$x`.

### Test Case 2: IF without ELSE in WHILE (PARTIALLY WORKS)

```ovsm
$x = 0
WHILE $x < 2:
    LOG(message: "Before IF")
    IF $x == 0 THEN
        LOG(message: "x is 0")
    LOG(message: "After IF")
    $x = $x + 1

LOG(message: "Done")
RETURN $x
```

**Expected Output:**
```
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "After IF"
[LOG] "Before IF"
[LOG] "After IF"
[LOG] "Done"
Result: 2
```

**Actual Output:**
```
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "After IF"
[LOG] "Done"
Result: 1
```

**Bug Manifestation:** The loop only executes once instead of twice. The THEN block parser has similar (but less severe) boundary detection issues.

### Test Case 3: With ENDIF Marker (ALSO BROKEN)

```ovsm
$x = 0
WHILE $x < 2:
    LOG(message: "Before IF")
    IF $x == 0 THEN
        LOG(message: "x is 0")
    ELSE
        LOG(message: "x is 1")
    ENDIF
    LOG(message: "After IF")
    $x = $x + 1

LOG(message: "Done")
LOG(message: $x)
RETURN $x
```

**Expected Output:**
```
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "After IF"
[LOG] "Before IF"
[LOG] "x is 1"
[LOG] "After IF"
[LOG] "Done"
[LOG] 2
Result: 2
```

**Actual Output:**
```
[LOG] "Before IF"
[LOG] "x is 0"
[LOG] "After IF"
[LOG] "Done"
[LOG] 1
[LOG] "Before IF"
[LOG] "x is 1"
[LOG] "After IF"
[LOG] "Done"
[LOG] 2
Result: 2
```

**Bug Manifestation:** The ENDIF marker is consumed correctly, but the WHILE loop parser then continues collecting statements BEYOND the WHILE loop body! The `LOG(message: "Done")` statements are incorrectly parsed as part of the WHILE loop body.

### Test Case 4: C-Style Braces (ALSO BROKEN)

```ovsm
$x = 0
WHILE $x < 2 {
    LOG(message: "Before IF")
    IF $x == 0 THEN
        LOG(message: "x is 0")
    ELSE
        LOG(message: "x is 1")
    LOG(message: "After IF")  # <-- NEVER EXECUTES
    $x = $x + 1               # <-- NEVER EXECUTES
}
LOG(message: "Done")
RETURN $x
```

**Actual Output:** Infinite loop (same as Test Case 1)

**Bug Manifestation:** Braces for the WHILE loop do NOT fix the issue. The IF statement's ELSE block parser still consumes too many statements.

### Test Case 5: Real-World Example - Pagination (BROKEN)

This is the actual use case that revealed the bug:

```ovsm
CONST PUMPFUN = "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
CONST MAX_PER_CALL = 1000

$pages = 0
$before = null
$done = 0

WHILE $done == 0:
    $pages = $pages + 1

    IF $before == null THEN
        $batch = getSignaturesForAddress(address: PUMPFUN, limit: MAX_PER_CALL)
    ELSE
        $batch = getSignaturesForAddress(address: PUMPFUN, limit: MAX_PER_CALL, before: $before)

    $size = COUNT($batch)  # <-- NEVER EXECUTES

    IF $size == 0 THEN
        $done = 1
    ELSE
        $last_idx = $size - 1
        $last = $batch[$last_idx]
        $before = $last.signature

LOG(message: "Pages")
LOG(message: $pages)
RETURN $pages
```

**Expected:** Paginate through RPC results, counting pages until no more results.

**Actual:** Infinite loop. The `$size = COUNT($batch)` line is never executed because the ELSE block parser consumes it as part of the ELSE block.

## Reproduction Steps

1. Build OSVM: `cargo build --release`
2. Create test file `/tmp/test_if_while_bug.ovsm` with Test Case 1 above
3. Run: `./target/release/osvm ovsm run /tmp/test_if_while_bug.ovsm`
4. Observe: Infinite loop, never logs "After IF"
5. Kill process: Ctrl+C

## Attempted Fixes

### Fix Attempt 1: Modified `is_end_of_loop_block()`

**Location:** `crates/ovsm/src/parser/mod.rs`, line 1268

**Change:** Made `is_end_of_loop_block()` NOT treat `ELSE` as a loop terminator:
```rust
fn is_end_of_loop_block(&self) -> bool {
    // Do NOT call is_end_of_block() because it includes ELSE/ELIF
    matches!(
        self.peek().kind,
        TokenKind::RightBrace
            | TokenKind::EndWhile
            | TokenKind::EndFor
            | TokenKind::End
            | TokenKind::Return
            | TokenKind::Branch
            | TokenKind::Catch
            | TokenKind::WaitAll
            | TokenKind::WaitAny
            | TokenKind::Race
    )
}
```

**Result:** Did NOT fix the issue. The problem is in the IF statement parser, not the WHILE parser.

### Why No Workaround Exists

1. **Braces don't help:** C-style `WHILE $x < 2 { }` has the same bug
2. **ENDIF doesn't help:** Explicit `ENDIF` markers create different bugs (consuming statements after the loop)
3. **Refactoring is hard:** Cannot avoid IF-THEN-ELSE for common patterns like pagination
4. **No alternative syntax:** OVSM doesn't provide another way to express conditional logic

## Proposed Solutions

### Solution 1: Implement Indentation Tracking (Recommended)

Add indentation tracking to the lexer/scanner:
1. Scanner emits `INDENT` and `DEDENT` tokens (like Python)
2. Parser uses these tokens to detect block boundaries
3. ELSE block parser stops on `DEDENT` token
4. Maintains backward compatibility with explicit markers

**Pros:**
- Proper fix for the root cause
- Enables true Python-style syntax
- Fixes all related block boundary issues

**Cons:**
- Requires lexer changes
- Moderate implementation effort
- May affect existing scripts

### Solution 2: Require Explicit Block Terminators

Require `ENDIF` for all IF statements inside loops:
1. Parser enforces `ENDIF` when IF is inside WHILE/FOR
2. Clear error messages guide users
3. Parser uses `ENDIF` as termination signal

**Pros:**
- Simpler implementation
- Clear syntax rules

**Cons:**
- Breaks existing scripts
- Verbose syntax
- Still has issues with ENDIF as shown in Test Case 3

### Solution 3: Use Peek-Ahead for Statement Classification

Parser looks ahead to determine if next statement belongs to current block:
1. Check indentation level (heuristic)
2. Look for "de-nesting" patterns
3. Use statistical models

**Pros:**
- No language syntax changes

**Cons:**
- Complex and error-prone
- Heuristics may fail
- Hard to maintain

## Recommended Action

Implement **Solution 1 (Indentation Tracking)** with these steps:

1. **Phase 1:** Add INDENT/DEDENT token emission to scanner
2. **Phase 2:** Update ELSE block parser to respect DEDENT
3. **Phase 3:** Update THEN block parser similarly
4. **Phase 4:** Add comprehensive tests for nested structures
5. **Phase 5:** Update documentation with syntax rules

## Workaround for Users (Temporary)

Until fixed, avoid IF-THEN-ELSE inside loops. Use one of these patterns:

**Pattern 1: Extract to separate variable**
```ovsm
WHILE $done == 0:
    $use_default = ($before == null)
    $batch = getSignaturesForAddress(...)  # Use $use_default in logic
    # ... rest of loop body works
```

**Pattern 2: Duplicate loop logic**
```ovsm
# Handle first iteration separately
$batch = getSignaturesForAddress(address: PUMPFUN, limit: MAX_PER_CALL)
# Process first batch...

# Then loop with $before always set
WHILE $done == 0:
    $batch = getSignaturesForAddress(address: PUMPFUN, limit: MAX_PER_CALL, before: $before)
    # ... rest of loop body
```

**Pattern 3: Use nested functions (if supported)**
```ovsm
# Move conditional logic outside loop
```

## Files Affected

- `crates/ovsm/src/parser/mod.rs` - Parser implementation (lines 187-304, 417-464)
- `crates/ovsm/src/lexer/mod.rs` - Will need changes for Solution 1
- `crates/ovsm/tests/` - Need new test cases for nested IF in loops

## Related Issues

None known. This appears to be the first report of this issue.

## Environment

- OSVM CLI version: 0.9.1
- OVSM version: 1.0.0
- Rust version: 1.80.0+
- OS: Linux 6.16.3

## Additional Notes

This bug was discovered while implementing Solana RPC pagination logic for counting transactions to the Pumpfun program. The pattern of using IF-THEN-ELSE to handle the first iteration (no cursor) versus subsequent iterations (with cursor) is a very common programming pattern that should work in any reasonable language.

The fact that this bug affects BOTH Python-style and C-style syntax suggests a fundamental architectural issue with how the parser determines block boundaries, not just a simple oversight.

## Test Files

Created test files:
- `/tmp/test_if_while_bug.ovsm` - Simple test case demonstrating bug
- `/tmp/test_if_no_else.ovsm` - IF without ELSE (partially works)
- `/tmp/test_endif.ovsm` - Using ENDIF markers (different bugs)
- `/tmp/test_braces.ovsm` - C-style braces (same bug)
- `/tmp/pumpfun_1min_diagnostic.ovsm` - Real-world pagination example

All test files can be run with:
```bash
./target/release/osvm ovsm run <filename>
```
