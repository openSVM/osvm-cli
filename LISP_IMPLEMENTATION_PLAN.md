# LISP Implementation Plan for OVSM

**Date:** 2025-10-19
**Status:** PYTHON-STYLE SYNTAX CURRENTLY IN USE (Despite CLAUDE.md claims)
**Goal:** Replace ALL Python-style syntax with authentic LISP S-expressions

---

## TL;DR: Current State is NOT LISP

**CRITICAL FINDING:**
The codebase currently has files named:
- `SExprScanner` - **DOES NOT** parse S-expressions, parses Python syntax
- `SExprParser` - **DOES NOT** build S-expression ASTs, expects Python keywords
- `LispEvaluator` - Evaluates Python-style ASTs, not LISP forms

**Evidence:**
- All `.ovsm` example files use Python syntax (`$variables`, `WHILE`, `IF THEN`, `:` for blocks)
- Files with `.scm` extension and LISP syntax (semicolons, parentheses) **FAIL TO PARSE**
- Test: `osvm ovsm eval ";; comment\n(+ 1 2)"` → **ERROR**: "expected expression, got Semicolon"

**CLAUDE.md is ASPIRATIONAL, not factual.** It claims Python syntax was "deleted" in October 2025. This is false.

---

## Why LISP? (User's Rationale)

1. **No Parser Bugs**: Explicit parentheses eliminate indentation-related parsing errors
2. **Unambiguous Blocks**: No confusion about where IF-THEN-ELSE ends in loops
3. **Homoiconicity**: Code-as-data enables future metaprogramming
4. **Cleaner Syntax**: Consistent prefix notation, no special cases

---

## Full Implementation Phases

### Phase 1: LISP Lexer (4-6 hours)

**File:** `crates/ovsm/src/lexer/sexpr_scanner.rs`

**Tasks:**
1. ✅ Semicolons trigger line comment consumption (already done, but unused)
2. **Remove all Python keyword detection** (no more `IF`, `WHILE`, `THEN`, etc.)
3. **Keep only**:
   - Parentheses: `(`, `)`, `[`, `]`, `{`, `}`
   - Numbers: integers and floats
   - Strings: double-quoted with escapes
   - Identifiers: `define`, `set!`, `if`, `while`, `for`, etc. (lowercase!)
   - Symbols: `+`, `-`, `*`, `/`, `==`, `<`, `>`, etc.
   - Keywords: `:message` (for keyword arguments)
   - Quote: `'` for quoting lists
4. **Test extensively** with LISP syntax

**Example Input:**
```lisp
;; This is a comment
(define x 42)
(if (> x 10)
    "big"
    "small")
```

**Expected Tokens:**
```
LeftParen, Identifier("define"), Identifier("x"), Integer(42), RightParen,
LeftParen, Identifier("if"), LeftParen, Identifier(">"), Identifier("x"), Integer(10), RightParen,
String("big"), String("small"), RightParen
```

### Phase 2: LISP Parser (12-16 hours)

**File:** `crates/ovsm/src/parser/sexpr_parser.rs`

**Current AST** (defined in `ast.rs`):
```rust
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Identifier(String),
    BinaryOp { left: Box<Expression>, op: BinaryOp, right: Box<Expression> },
    ToolCall { name: String, args: Vec<Expression> },
    // ... etc
}

pub enum Statement {
    Assignment { var: String, value: Expression },
    If { condition: Expression, then_block: Vec<Statement>, else_block: Vec<Statement> },
    While { condition: Expression, body: Vec<Statement> },
    Return(Expression),
    // ... etc
}
```

**Problem:** This AST is designed for Python-style syntax with separate statements and expressions. LISP is expression-only.

**Solution:** Either:
- **Option A:** Reuse existing AST, translate S-expressions to it during parsing
- **Option B:** Create new LISP-specific AST with only `Expression` (no `Statement`)

**Recommended:** Option A for faster implementation

**Parser Structure:**
```rust
impl SExprParser {
    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_form()?);
        }
        Ok(Program::new(statements))
    }

    fn parse_form(&mut self) -> Result<Statement> {
        match self.peek().kind {
            TokenKind::LeftParen => self.parse_list(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Statement> {
        self.consume(TokenKind::LeftParen)?;

        if self.check(&TokenKind::RightParen) {
            self.advance();
            return Ok(/* empty list */);
        }

        let first_token = self.peek();
        match first_token.kind {
            TokenKind::Identifier(ref name) => {
                match name.as_str() {
                    "define" => self.parse_define(),
                    "set!" => self.parse_set(),
                    "const" => self.parse_const(),
                    "if" => self.parse_if(),
                    "while" => self.parse_while(),
                    "for" => self.parse_for(),
                    "do" => self.parse_do(),
                    "let" => self.parse_let(),
                    "lambda" | "λ" => self.parse_lambda(),
                    "cond" => self.parse_cond(),
                    "when" => self.parse_when(),
                    _ => self.parse_function_call(),
                }
            }
            _ => Err(Error::ParseError("Expected identifier after (".to_string())),
        }
    }
}
```

**Special Forms to Implement:**
- `(define VAR EXPR)` → Assignment
- `(set! VAR EXPR)` → Reassignment
- `(const NAME VALUE)` → Constant
- `(if COND THEN ELSE)` → Conditional
- `(while COND BODY...)` → Loop
- `(for (VAR COLLECTION) BODY...)` → Iteration
- `(do EXPR...)` → Sequential execution
- `(let ((VAR VAL)...) BODY...)` → Local bindings
- `(lambda (PARAMS...) BODY)` → Anonymous function
- `(+ A B C...)` → Variadic addition
- `(log :message "text")` → Keyword arguments

### Phase 3: LISP Evaluator Updates (6-8 hours)

**File:** `crates/ovsm/src/runtime/lisp_evaluator.rs`

**Current evaluator** already has some LISP-like special forms, but they're inconsistent.

**Tasks:**
1. Ensure all special forms work correctly with new AST
2. Implement variadic operators: `(+ 1 2 3)` → 6, `(* 2 3 4)` → 24
3. Implement sequential execution: `(do EXPR1 EXPR2 EXPR3)` returns last value
4. Implement proper `let` scoping (lexical, not dynamic)
5. Test extensively

### Phase 4: Convert Examples (4-6 hours)

**Convert ALL `.ovsm` files to `.scm`:**

**Before (Python-style):**
```
CONST PUMPFUN = "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
$done = 0
$count = 0

WHILE $done == 0:
    IF $count >= 10 THEN
        $done = 1
    ELSE
        $count = $count + 1
        LOG(message: "Iteration")
```

**After (LISP-style):**
```lisp
(const PUMPFUN "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(define done false)
(define count 0)

(while (not done)
  (if (>= count 10)
      (set! done true)
      (do
        (set! count (+ count 1))
        (log :message "Iteration"))))
```

**Files to convert:**
- `examples/ovsm_scripts/01_hello_world.ovsm`
- `examples/ovsm_scripts/02_control_flow.ovsm`
- `examples/ovsm_scripts/05_factorial.ovsm`
- All pumpfun transaction counting scripts
- Test files

### Phase 5: Update Documentation (2-3 hours)

**Files to update:**
- `CLAUDE.md` - Make it ACTUALLY reflect LISP-only status
- `crates/ovsm/README.md` - Document LISP syntax
- `crates/ovsm/USAGE_GUIDE.md` - LISP examples
- `docs/ovsm/OVSM_LISP_SYNTAX_SPEC.md` - Already exists, verify accuracy

### Phase 6: Delete Python Code (1 hour)

**After LISP works end-to-end:**
1. Delete all `.ovsm` files with Python syntax
2. Remove Python keyword handling from token.rs
3. Remove INDENT/DEDENT token types
4. Clean up any remaining Python-style code paths

### Phase 7: Testing (6-8 hours)

**Test suite:**
1. Unit tests for lexer (S-expression tokenization)
2. Unit tests for parser (AST construction)
3. Unit tests for evaluator (special forms)
4. Integration tests (end-to-end .scm file execution)
5. Regression tests (ensure no functionality lost)

---

## Time Estimate

**Optimistic:** 35 hours
**Realistic:** 45 hours
**Pessimistic:** 60 hours

**Factors affecting duration:**
- How much existing code can be reused
- Number of edge cases discovered
- Testing thoroughness requirements

---

## Alternative: Quick Fix for Parser Bug

**If the goal is just to fix the IF-THEN-ELSE bug in Python syntax:**

**Estimated time:** 2-4 hours

**Approach:** Fix indentation handling in existing Python-style parser to properly track DEDENT tokens and block endings.

**Pros:** Fast, preserves existing examples
**Cons:** Doesn't achieve user's goal of "REMOVE PYTHON, REPLACE WITH LISP"

---

## Recommendation

Given user's emphatic directives:
1. "wtf get rid of all fucking python and replace with lisp!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
2. "REMOVE PYTHON, REPLACE WITH LISP"

**I recommend proceeding with full LISP implementation.**

However, this is a 40-60 hour task. We should:
1. Start with Phase 1 (lexer) - prove semicolons work, S-expressions tokenize
2. Test Phase 1 extensively before moving to Phase 2
3. Proceed incrementally, testing at each phase
4. Keep Python parser intact until LISP parser works (dual support temporarily)
5. Only delete Python code after LISP is fully functional

---

## Next Steps

**User should decide:**
- [ ] Proceed with full LISP implementation (40-60 hours)
- [ ] Just fix the Python parser bug (2-4 hours)
- [ ] Implement LISP in parallel, keep Python temporarily
- [ ] Other approach?

**If proceeding with LISP:**
I'll start with Phase 1: rewriting the lexer to properly handle LISP syntax, starting NOW.
