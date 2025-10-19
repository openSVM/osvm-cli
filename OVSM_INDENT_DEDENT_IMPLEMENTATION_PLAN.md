# OVSM Parser Bug Fix: INDENT/DEDENT Implementation Plan

**Status:** CRITICAL BUG - Requires lexer-level indentation tracking
**Priority:** HIGH
**Estimated Effort:** 4-6 hours
**Complexity:** High - Requires architectural changes to lexer and parser

---

## Executive Summary

This document provides a detailed implementation plan for fixing the critical OVSM parser bug where `IF-THEN-ELSE` statements do not work correctly inside `WHILE` and `FOR` loops. The root cause is that the lexer does not emit indentation tokens (INDENT/DEDENT), preventing the parser from correctly detecting block boundaries in Python-style indentation-based syntax.

### Bug Behavior
```ovsm
WHILE $done == 0:
    IF $condition THEN
        // code here
    ELSE
        // BUG: This ELSE block incorrectly consumes the entire WHILE loop body!

    $done = 1  # <--- THIS LINE NEVER EXECUTES - infinite loop results
```

---

## Implementation Overview

### Files to Modify

1. **`crates/ovsm/src/lexer/token.rs`**
   - Add `INDENT` and `DEDENT` to `TokenKind` enum
   - Update `is_keyword()` method if needed
   - Update `Display` implementation

2. **`crates/ovsm/src/lexer/scanner.rs`**
   - Add indentation stack to `Scanner` struct
   - Track indentation levels throughout scanning
   - Emit INDENT/DEDENT tokens at appropriate times
   - Handle edge cases (blank lines, comments, continuations)

3. **`crates/ovsm/src/parser/mod.rs`**
   - Update `is_end_of_block()` to check for `DEDENT`
   - Update all block-parsing functions to respect DEDENT
   - Test with nested structures

4. **Testing**
   - Add unit tests for indentation tracking
   - Add integration tests for nested blocks
   - Verify no regressions in existing tests

---

## Phase 1: Add INDENT/DEDENT Token Types

### File: `crates/ovsm/src/lexer/token.rs`

**Location:** After line 211 (after `Newline` token kind)

```rust
// Add to TokenKind enum after Newline, before Eof:

/// Increase in indentation level
Indent,
/// Decrease in indentation level
Dedent,

// Eof stays as the last token kind
```

**Update `is_keyword()` method** (line 220-262):
- INDENT and DEDENT are NOT keywords, so no changes needed

**Update `Display` implementation** (line 316-328):
```rust
// Add these cases to the match statement:
TokenKind::Indent => write!(f, "<INDENT>"),
TokenKind::Dedent => write!(f, "<DEDENT>"),
```

---

## Phase 2: Implement Indentation Tracking in Scanner

### File: `crates/ovsm/src/lexer/scanner.rs`

### Step 2.1: Update Scanner Struct

**Location:** Lines 5-18

```rust
/// Scanner for tokenizing OVSM source code
pub struct Scanner {
    /// Source code as character vector
    source: Vec<char>,
    /// Accumulated tokens
    tokens: Vec<Token>,
    /// Start position of current token
    start: usize,
    /// Current position in source
    current: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column number (1-indexed)
    column: usize,

    // ADD THESE NEW FIELDS:
    /// Stack of indentation levels (measured in spaces)
    indent_stack: Vec<usize>,
    /// Whether we're at the start of a line
    at_line_start: bool,
    /// Pending DEDENT tokens to emit
    pending_dedents: usize,
}
```

### Step 2.2: Update Constructor

**Location:** Lines 22-31

```rust
pub fn new(source: &str) -> Self {
    Scanner {
        source: source.chars().collect(),
        tokens: Vec::new(),
        start: 0,
        current: 0,
        line: 1,
        column: 1,

        // ADD THESE INITIALIZATIONS:
        indent_stack: vec![0],  // Start with base indentation of 0
        at_line_start: true,    // File starts at line start
        pending_dedents: 0,     // No pending dedents initially
    }
}
```

### Step 2.3: Modify Main Scan Loop

**Location:** Lines 34-48 (`scan_tokens` method)

```rust
pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
    while !self.is_at_end() {
        // Handle pending DEDENT tokens first
        if self.pending_dedents > 0 {
            self.add_token(TokenKind::Dedent);
            self.pending_dedents -= 1;
            continue;
        }

        // Check for indentation changes at line start
        if self.at_line_start && !self.is_at_end() {
            self.handle_indentation()?;
        }

        self.start = self.current;
        self.scan_token()?;
    }

    // Emit remaining DEDENT tokens at end of file
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

### Step 2.4: Implement Indentation Handler

**Add this new method to Scanner** (around line 400):

```rust
/// Handles indentation at the start of a line
fn handle_indentation(&mut self) -> Result<()> {
    // Skip blank lines and comment-only lines
    if self.is_blank_or_comment_line() {
        return Ok(());
    }

    // Measure indentation level
    let mut indent = 0;
    let indent_start = self.current;

    while !self.is_at_end() && (self.peek() == ' ' || self.peek() == '\t') {
        if self.peek() == '\t' {
            // Treat tab as 4 spaces (or 8, be consistent)
            indent += 4;
        } else {
            indent += 1;
        }
        self.advance();
    }

    // Mark that we're no longer at line start
    self.at_line_start = false;

    // If this is a blank line or comment, don't process indentation
    if self.is_at_end() || self.peek() == '\n' || self.peek() == '#' {
        return Ok(());
    }

    // Get current indentation level
    let current_indent = *self.indent_stack.last().unwrap();

    if indent > current_indent {
        // Indentation increased - emit INDENT
        self.indent_stack.push(indent);
        self.add_token(TokenKind::Indent);
    } else if indent < current_indent {
        // Indentation decreased - emit DEDENT(s)
        while self.indent_stack.len() > 1 && *self.indent_stack.last().unwrap() > indent {
            self.indent_stack.pop();
            self.pending_dedents += 1;
        }

        // Check for indentation error (doesn't match any previous level)
        if *self.indent_stack.last().unwrap() != indent {
            return Err(Error::SyntaxError {
                line: self.line,
                col: self.column,
                message: format!(
                    "Indentation error: {} spaces doesn't match any outer indentation level",
                    indent
                ),
            });
        }
    }
    // If indent == current_indent, no token needed

    Ok(())
}

/// Checks if the rest of the current line is blank or just a comment
fn is_blank_or_comment_line(&self) -> bool {
    let mut pos = self.current;

    // Skip whitespace
    while pos < self.source.len() && (self.source[pos] == ' ' || self.source[pos] == '\t') {
        pos += 1;
    }

    // Check if we hit end of line or comment
    pos >= self.source.len() || self.source[pos] == '\n' || self.source[pos] == '#'
}
```

### Step 2.5: Update Newline Handling

**Location:** Lines 58-62 (in `scan_token` method)

```rust
'\n' => {
    self.add_token(TokenKind::Newline);
    self.line += 1;
    self.column = 1;
    self.at_line_start = true;  // ADD THIS LINE
}
```

---

## Phase 3: Update Parser

### File: `crates/ovsm/src/parser/mod.rs`

### Step 3.1: Update `is_end_of_block()` Method

**Location:** Find the `is_end_of_block()` method (search for "fn is_end_of_block")

```rust
fn is_end_of_block(&self) -> bool {
    matches!(
        self.peek().kind,
        TokenKind::Else
            | TokenKind::Elif
            | TokenKind::EndIf
            | TokenKind::EndWhile
            | TokenKind::EndFor
            | TokenKind::End
            | TokenKind::RightBrace
            | TokenKind::Branch
            | TokenKind::Catch
            | TokenKind::WaitAll
            | TokenKind::WaitAny
            | TokenKind::Race
            | TokenKind::Dedent  // ADD THIS LINE - Critical for indentation-based blocks
            | TokenKind::Eof
    )
}
```

### Step 3.2: Update Block Parsers

The following methods need to be verified to properly consume DEDENT tokens:

1. **`parse_if_statement()`** - THEN/ELSE/ELIF blocks
2. **`parse_while_statement()`** - WHILE body
3. **`parse_for_statement()`** - FOR body
4. **`parse_try_statement()`** - TRY/CATCH blocks

**General pattern for each block:**

```rust
// After parsing block statements:
while self.check(&TokenKind::Newline) {
    self.advance();
}

// Consume DEDENT if present
if self.check(&TokenKind::Dedent) {
    self.advance();
}
```

### Step 3.3: Skip INDENT Tokens

Add helper to skip INDENT tokens where needed:

```rust
fn skip_indent(&mut self) {
    while self.check(&TokenKind::Indent) {
        self.advance();
    }
}
```

---

## Phase 4: Edge Cases to Handle

### 4.1 Blank Lines
**Behavior:** Blank lines should be ignored and not affect indentation tracking.

```ovsm
WHILE $done == 0:
    $x = 1

    $y = 2  # Blank line above should be ignored
```

**Implementation:** See `is_blank_or_comment_line()` in Step 2.4

### 4.2 Comments
**Behavior:** Comment-only lines should not affect indentation.

```ovsm
WHILE $done == 0:
    $x = 1
    # This is a comment
    $y = 2
```

**Implementation:** Check for `#` in `is_blank_or_comment_line()`

### 4.3 String Continuations
**Behavior:** Multi-line strings should not trigger DEDENT.

```ovsm
$str = "This is a
    multi-line string"
```

**Current handling:** The string scanner in `scan_token()` already handles this (lines 223-261)

### 4.4 Mixed Tabs and Spaces
**Decision:** Tabs = 4 spaces (configurable)
**Recommendation:** Warn users or error on mixed indentation

```rust
// In handle_indentation():
if /* detected mixed tabs and spaces */ {
    eprintln!("Warning: Mixed tabs and spaces detected on line {}", self.line);
}
```

### 4.5 Inconsistent Indentation
**Behavior:** Error if dedent doesn't match any previous indentation level.

```ovsm
WHILE $x:
    $a = 1      # 4 spaces
      $b = 2    # 6 spaces - ERROR: doesn't match any level
```

**Implementation:** Already handled in Step 2.4 (indentation error check)

---

## Phase 5: Testing Strategy

### 5.1 Unit Tests for Scanner

**File:** `crates/ovsm/src/lexer/scanner.rs` (add to existing `#[cfg(test)]` block)

```rust
#[test]
fn test_simple_indent() {
    let source = "IF true THEN\n    $x = 1";
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Verify INDENT token appears after THEN
    // ... assertions ...
}

#[test]
fn test_simple_dedent() {
    let source = "IF true THEN\n    $x = 1\n$y = 2";
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Verify DEDENT token appears before $y
    // ... assertions ...
}

#[test]
fn test_multiple_dedents() {
    let source = "IF true THEN\n    IF true THEN\n        $x = 1\n$y = 2";
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Verify two DEDENT tokens before $y
    // ... assertions ...
}

#[test]
fn test_blank_lines_ignored() {
    let source = "WHILE true:\n    $x = 1\n\n    $y = 2";
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Verify blank line doesn't cause DEDENT
    // ... assertions ...
}

#[test]
fn test_comment_lines_ignored() {
    let source = "WHILE true:\n    $x = 1\n# comment\n    $y = 2";
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    // Verify comment line doesn't cause DEDENT
    // ... assertions ...
}

#[test]
fn test_indentation_error() {
    let source = "IF true THEN\n    $x = 1\n  $y = 2";  // 2 spaces - doesn't match
    let mut scanner = Scanner::new(source);
    let result = scanner.scan_tokens();

    assert!(result.is_err());
    // Verify error message mentions indentation
}
```

### 5.2 Integration Tests for Parser

**File:** `crates/ovsm/tests/parser_tests.rs` (create if doesn't exist)

```rust
#[test]
fn test_if_in_while_loop() {
    let code = r#"
$done = 0
$count = 0

WHILE $done == 0:
    IF $count == 0 THEN
        $count = $count + 1
    ELSE
        $done = 1

    $count = $count + 1

RETURN $count
"#;

    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();

    // Verify AST structure is correct
    // ... assertions ...
}

#[test]
fn test_nested_loops_with_conditionals() {
    let code = r#"
FOR $i IN [1..5]:
    WHILE $i > 0:
        IF $i == 3 THEN
            BREAK
        ELSE
            $i = $i - 1
"#;

    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();

    // Verify nested structure
    // ... assertions ...
}
```

### 5.3 End-to-End Tests

**File:** `tests/ovsm_integration_tests.rs`

```rust
#[test]
fn test_pumpfun_pagination_script() {
    // Test the actual failing script from the bug report
    let script_path = "/tmp/test_parser_fix.ovsm";
    let output = Command::new("./target/release/osvm")
        .args(&["ovsm", "run", script_path])
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());

    // Verify it doesn't infinite loop (would timeout)
    // Verify output contains expected values
}
```

### 5.4 Regression Testing

**Run all existing OVSM tests:**

```bash
cargo test --package ovsm
cargo test --test ovsm_integration_tests
```

**Expected results:**
- All existing tests should still pass
- New tests for indentation should pass
- Scripts with IF-THEN-ELSE in loops should work

---

## Phase 6: Implementation Checklist

### Pre-Implementation
- [ ] Read and understand Python's indentation algorithm (for reference)
- [ ] Review OVSM grammar specification
- [ ] Set up test environment with sample scripts

### Phase 1: Token Types
- [ ] Add `Indent` and `Dedent` to `TokenKind` enum
- [ ] Update `Display` implementation
- [ ] Verify compilation with `cargo check`

### Phase 2: Scanner Changes
- [ ] Add indentation tracking fields to `Scanner` struct
- [ ] Update constructor to initialize new fields
- [ ] Implement `handle_indentation()` method
- [ ] Implement `is_blank_or_comment_line()` helper
- [ ] Modify `scan_tokens()` to handle indentation
- [ ] Update newline handling to set `at_line_start`
- [ ] Add scanner unit tests
- [ ] Run tests: `cargo test --lib scanner`

### Phase 3: Parser Changes
- [ ] Update `is_end_of_block()` to include `Dedent`
- [ ] Review and update `parse_if_statement()`
- [ ] Review and update `parse_while_statement()`
- [ ] Review and update `parse_for_statement()`
- [ ] Add `skip_indent()` helper if needed
- [ ] Add parser unit tests
- [ ] Run tests: `cargo test --lib parser`

### Phase 4: Integration Testing
- [ ] Test `/tmp/test_parser_fix.ovsm` (the bug reproduction case)
- [ ] Test pumpfun pagination scripts
- [ ] Test all example OVSM scripts in `examples/ovsm_scripts/`
- [ ] Run full test suite: `cargo test`
- [ ] Verify no regressions in existing functionality

### Phase 5: Edge Case Testing
- [ ] Test blank lines in various positions
- [ ] Test comment-only lines
- [ ] Test mixed indentation (should warn/error)
- [ ] Test inconsistent dedent (should error)
- [ ] Test deeply nested structures
- [ ] Test all combinations of control structures

### Phase 6: Documentation
- [ ] Update OVSM language specification
- [ ] Add indentation rules to usage guide
- [ ] Update CLAUDE.md to mark bug as fixed
- [ ] Write commit message documenting fix

---

## Implementation Tips

### Debugging Indentation Issues

1. **Add debug logging:**
```rust
fn handle_indentation(&mut self) -> Result<()> {
    eprintln!("Line {}: indent_stack={:?}", self.line, self.indent_stack);
    // ... rest of implementation
}
```

2. **Visualize tokens:**
```rust
fn print_tokens(tokens: &[Token]) {
    for (i, tok) in tokens.iter().enumerate() {
        println!("{}: {:?} at {}:{}", i, tok.kind, tok.line, tok.column);
    }
}
```

3. **Test incrementally:**
   - Start with simple IF statements
   - Add WHILE loops
   - Add nested structures
   - Add edge cases

### Common Pitfalls

1. **Forgetting to set `at_line_start`** after newlines → indentation not checked
2. **Not handling blank lines** → spurious DEDENT tokens
3. **Not emitting pending DEDENTs** → missing block boundaries
4. **Emitting DEDENT in wrong order** → parser confused
5. **Not checking for final DEDENTs at EOF** → unclosed blocks

### Performance Considerations

- Indentation tracking adds minimal overhead (O(1) per line)
- Indent stack depth is proportional to nesting level (typically < 10)
- No significant performance impact expected

---

## Reference Implementation

For reference, see how Python implements indentation in CPython:
- https://github.com/python/cpython/blob/main/Parser/tokenizer.c
- Search for "indentation" and "INDENT/DEDENT" tokens

Key insights from Python's approach:
1. Maintain stack of indentation levels
2. Ignore blank and comment-only lines
3. Emit multiple DEDENTs when unwinding multiple levels
4. Error on inconsistent indentation

---

## Success Criteria

### Required
- [ ] `/tmp/test_parser_fix.ovsm` executes correctly (returns 2, not infinite loop)
- [ ] All pumpfun pagination scripts work correctly
- [ ] All existing OVSM tests pass
- [ ] No regressions in parser behavior

### Nice to Have
- [ ] Helpful error messages for indentation problems
- [ ] Warning on mixed tabs/spaces
- [ ] Performance impact < 5%

---

## Estimated Timeline

| Phase | Time | Description |
|-------|------|-------------|
| Phase 1: Token Types | 15 min | Add INDENT/DEDENT to enum |
| Phase 2: Scanner | 2-3 hours | Implement indentation tracking |
| Phase 3: Parser | 1 hour | Update block boundary detection |
| Phase 4: Integration Testing | 1 hour | Test real scripts |
| Phase 5: Edge Cases | 30 min | Test corner cases |
| Phase 6: Documentation | 30 min | Update docs |
| **Total** | **4-6 hours** | Complete implementation |

---

## Conclusion

This implementation plan provides a clear, step-by-step guide to fixing the critical OVSM parser bug. The fix requires adding indentation tracking to the lexer, which is a well-understood problem with established solutions (see Python, Haskell, etc.).

The key insight is that **OVSM currently has Python-style indentation syntax but lacks the lexer-level token emission mechanism that makes it work**. By adding INDENT/DEDENT tokens and updating the parser to respect them, all nested control structures will work correctly.

---

**Document Version:** 1.0
**Created:** 2025-10-19
**Author:** Claude (Anthropic)
**Status:** Ready for Implementation
