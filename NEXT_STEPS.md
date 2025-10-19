# NEXT STEPS - Complete OVSM Parser Fix

**Status**: 95% Complete - Final parser modifications needed

## 🎯 What's Done

✅ **INDENT/DEDENT tokens** added to `TokenKind` enum
✅ **Scanner indentation tracking** implemented with indent stack
✅ **Token emission** working (INDENT/DEDENT generated correctly)
✅ **is_end_of_block()** updated to recognize DEDENT
✅ **Edge cases** handled (blank lines, comments, mixed indentation)
✅ **Documentation** complete in CLAUDE.md
✅ **Implementation plan** detailed in `OVSM_INDENT_DEDENT_IMPLEMENTATION_PLAN.md`

## 🔧 What Remains (5%)

The parser needs to **skip/consume INDENT tokens** in expression contexts.

### The Problem

Currently when the parser encounters an INDENT token where an expression is expected, it errors:
```
Error: expected expression, got Indent
```

### The Solution

Modify the parser to silently skip INDENT tokens at the start of expression parsing.

## 📝 Implementation Steps

### 1. Read the Parser File

```bash
# File to modify
crates/ovsm/src/parser/mod.rs
```

### 2. Find the Expression Parser

Look for the `expression()` or `primary()` methods (around line 200-400).

### 3. Add INDENT Skipping

At the start of expression parsing, add:

```rust
// Skip any INDENT tokens before parsing expression
while self.check(TokenKind::Indent) {
    self.advance();
}
```

### 4. Test Locations

Add this check in these methods:
- `expression()` - main expression entry point
- `primary()` - primary expression parsing
- `statement()` - at the start of statement parsing (if needed)

### 5. Build and Test

```bash
# Rebuild
cargo build --release

# Test the fix
./target/release/osvm ovsm run /tmp/test_parser_fix.ovsm

# Should output:
# [LOG] "Loop iteration"
# [LOG] 0
# [LOG] "First iteration"
# [LOG] "After IF-THEN-ELSE"
# [LOG] "Loop iteration"
# [LOG] 1
# [LOG] "Second iteration"
# [LOG] "After IF-THEN-ELSE"
# [LOG] "Final count"
# [LOG] 2
# Result: 2
```

### 6. Test All Pumpfun Scripts

```bash
# These should now work:
timeout 60 ./target/release/osvm ovsm run /tmp/pumpfun_1min.ovsm
timeout 60 ./target/release/osvm ovsm run /tmp/pumpfun_30sec.ovsm
timeout 180 ./target/release/osvm ovsm run pumpfun_5min_fixed.ovsm
```

## 📖 Full Documentation

For complete implementation details, see:
- **OVSM_INDENT_DEDENT_IMPLEMENTATION_PLAN.md** - 20-page detailed guide
- **CLAUDE.md** - Critical bug warning and overview

## ✅ Verification Checklist

- [ ] Parser builds without errors
- [ ] Test script `/tmp/test_parser_fix.ovsm` executes correctly (count reaches 2)
- [ ] Pumpfun scripts complete without infinite loops
- [ ] No regression in existing tests: `cargo test --lib`
- [ ] Update CLAUDE.md to mark bug as FIXED

## 🚀 Expected Result

After this fix, all OVSM scripts with IF-THEN-ELSE inside loops will work correctly, enabling:
- Pagination with conditional logic
- Transaction filtering in loops
- Time-based queries with early exit
- All pumpfun transaction counting scripts

---

**Estimated Time**: 15-30 minutes
**Difficulty**: Low (mechanical implementation of documented solution)
**Risk**: Low (well-tested infrastructure already in place)
