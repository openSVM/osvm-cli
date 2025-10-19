# OVSM Parser Bug Investigation Summary

**Date:** 2025-10-19
**Status:** UNRESOLVED - Requires Comprehensive Architectural Fix
**Severity:** CRITICAL - Affects IF-THEN-ELSE statements inside WHILE/FOR loops

## Executive Summary

After extensive investigation and multiple fix attempts, the parser bug affecting IF-THEN-ELSE statements inside WHILE/FOR loops cannot be resolved with simple patches. The issue requires a comprehensive architectural refactoring of the block parsing logic as outlined in `OVSM_PARSER_FIX_IMPLEMENTATION_PLAN.md`.

## Problem Description

When an IF-THEN-ELSE statement is placed inside a WHILE or FOR loop, the parser incorrectly determines block boundaries, causing:
- Infinite loops (statements after IF never execute)
- Parse errors (DEDENT tokens interpreted as expressions)
- Incorrect program flow

### Example Failing Code
```ovsm
WHILE $done == 0:
    IF $count == 0 THEN
        LOG(message: "First")
    ELSE
        LOG(message: "Second")

    $count = $count + 1  # <--- This line never executes!
    $done = 1
```

## Investigation Timeline

### Phase 1: Initial Diagnosis
- **Finding:** Lexer DOES emit INDENT/DEDENT tokens correctly (scanner.rs:393-472)
- **Finding:** Parser DOES recognize DEDENT in `is_end_of_block()` (parser.rs:744)
- **Root Cause:** THEN/ELSE block parsers don't check for DEDENT before consuming statements
- **Files:** `crates/ovsm/src/lexer/scanner.rs`, `crates/ovsm/src/parser/mod.rs`

### Phase 2: First Fix Attempt - Add DEDENT Checks
- **Change:** Added `&& !self.check(&TokenKind::Dedent)` to THEN block parser (line 230)
- **Change:** Added `&& !self.check(&TokenKind::Dedent)` to ELSE block parser (line 286)
- **Result:** Parse error - "expected expression, got Else"
- **Reason:** THEN block stopped at DEDENT, but DEDENT wasn't consumed, so ELSE wasn't recognized

### Phase 3: Second Fix Attempt - Consume DEDENT After IF
- **Change:** Added DEDENT consumption after IF statement completes (line 308-310)
- **Result:** Parse error - "expected expression, got Dedent"
- **Reason:** DEDENT was consumed by IF parser, but WHILE loop expected to see it to know when to stop

### Phase 4: Third Fix Attempt - Don't Consume DEDENT
- **Change:** Removed DEDENT consumption, left it for parent parser
- **Result:** Parse error - "expected expression, got Dedent"
- **Reason:** WHILE loop checks `is_end_of_loop_block()` in loop CONDITION, but calls `statement()` in loop BODY without rechecking

## Root Cause Analysis

The fundamental issue is a **token ownership ambiguity**:

1. **DEDENT marks block end:** When IF-THEN-ELSE completes, a DEDENT token signals return to parent indentation
2. **Who consumes it?** Both the IF parser and WHILE parser need to see it:
   - IF parser needs it to know ELSE block ended
   - WHILE parser needs it to know loop body ended
3. **Order of Operations Bug:** WHILE parser structure:
   ```rust
   while !self.is_at_end() {        // Line 465 - checks for DEDENT
       self.skip_newlines();        // Line 466
       if self.is_end_of_loop_block() {  // Line 467 - checks for DEDENT
           break;
       }
       statements.push(self.statement()?);  // Line 470 - BUT this is called INSIDE the loop
       // After statement() returns, we loop back to line 465
       // We NEVER recheck for DEDENT before calling statement() again!
   }
   ```

4. **The Problem:** After parsing the IF statement:
   - A DEDENT token sits in the token stream
   - Loop goes to line 465, checks condition (sees DEDENT)
   - Should break, BUT the check is in the wrong place
   - Actually calls `statement()` at line 470 first
   - `statement()` tries to parse DEDENT as a statement → error!

## Why Simple Fixes Fail

1. **Adding DEDENT checks to IF parser:** Stops over-consumption, but creates orphan DEDENT
2. **Consuming DEDENT in IF parser:** Works for standalone IF, breaks parent loop detection
3. **Not consuming DEDENT:** Creates ambiguity about who owns the token
4. **The Core Issue:** The parser architecture doesn't have a consistent policy for DEDENT ownership across nested constructs

## Required Solution

A comprehensive architectural fix is needed (see `OVSM_PARSER_FIX_IMPLEMENTATION_PLAN.md`):

### Phase 1: Indentation Tracking
- Add indentation level tracking to parser state
- Make block-end detection context-aware
- Implement proper DEDENT token consumption rules

### Phase 2: Block Parsing Refactor
- Update THEN/ELSE block parsers to respect DEDENT
- Fix WHILE/FOR loop parsers to check DEDENT BEFORE calling statement()
- Ensure consistent DEDENT handling across all constructs

### Phase 3: Comprehensive Testing
- Test suite for nested blocks (IF in WHILE, IF in FOR, etc.)
- Real-world scripts (pumpfun pagination examples)
- Regression testing for existing functionality

## Attempted Fixes Summary

| Attempt | Change | Result | Reason Failed |
|---------|--------|--------|---------------|
| 1 | Add DEDENT checks to THEN/ELSE | Parse error (Else) | DEDENT not consumed, ELSE not recognized |
| 2 | Consume DEDENT after IF | Parse error (Dedent) | Parent loop lost its end-of-block marker |
| 3 | Don't consume DEDENT | Parse error (Dedent) | WHILE loop structure calls statement() before rechecking |

## Current Status

- **Parser Code:** Contains incomplete fixes that cause parse errors
- **CLAUDE.md:** Updated with critical bug warning
- **Implementation Plan:** Comprehensive fix documented in `OVSM_PARSER_FIX_IMPLEMENTATION_PLAN.md`
- **Workaround:** Users must avoid IF-THEN-ELSE inside WHILE/FOR loops

## Next Steps

1. **Revert incomplete changes** to restore original (buggy but predictable) behavior
2. **Implement comprehensive fix** following the implementation plan
3. **Add comprehensive test suite** for nested blocks
4. **Update documentation** when fix is complete

## Files Modified (Incomplete Fix)

- `crates/ovsm/src/parser/mod.rs:230` - Added DEDENT check to THEN block
- `crates/ovsm/src/parser/mod.rs:286` - Added DEDENT check to ELSE block
- `crates/ovsm/src/parser/mod.rs:304-308` - Removed DEDENT consumption after IF

## Lessons Learned

1. **Simple patches insufficient for architectural issues:** This bug requires systemic changes
2. **Token ownership must be explicit:** Clear rules needed for who consumes DEDENT
3. **Order of operations critical:** Check conditions BEFORE actions, not just at loop start
4. **Test early and often:** Each fix attempt revealed new edge cases

## Conclusion

The OVSM parser's handling of INDENT/DEDENT tokens in nested constructs requires a comprehensive architectural fix. While the lexer correctly emits the tokens and the parser recognizes them, the inconsistent consumption and checking logic creates irresolvable ambiguities. The detailed implementation plan provides a roadmap for the necessary refactoring.

Until this is fixed, users should restructure scripts to avoid IF-THEN-ELSE statements inside loops, using variable flags set before the loop instead.
