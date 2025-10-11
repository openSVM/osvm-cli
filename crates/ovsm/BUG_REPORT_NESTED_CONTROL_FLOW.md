# Bug Report: Variable Scope Issues in Nested Control Flow

## Summary
Variables defined in FOR/WHILE loop bodies become inaccessible after encountering an IF statement within the loop body. This appears to be a scope management bug in the interpreter.

## Severity
**CRITICAL** - Causes infinite loops in WHILE statements and breaks basic variable access patterns.

## Affected Constructs
- FOR loops with IF statements in body
- WHILE loops with IF statements in body (causes infinite loop when counter update fails)

## Minimal Reproduction

### Test Case 1: FOR with IF (Variable Access Fails)
```ovsm
FOR $i IN [1..3]:
    $before = $i     # ✓ Works
    IF true THEN
        $x = 1
    $after = $i      # ✗ Error: UndefinedVariable: i
PRINT $after
```

**Expected:** Prints 3
**Actual:** `Error: UndefinedVariable: i at line 5`

### Test Case 2: WHILE with IF (Infinite Loop)
```ovsm
$i = 0
WHILE $i < 3:
    $before = $i     # ✓ Works
    IF true THEN
        $dummy = 1
    $i = $i + 1      # ✗ Error: UndefinedVariable: i (causes infinite loop)
PRINT $i
```

**Expected:** Prints 3
**Actual:** Hangs forever (timeout)

### Test Case 3: Control - Works Without IF
```ovsm
FOR $i IN [1..3]:
    $before = $i     # ✓ Works
    $after = $i      # ✓ Works (no IF in between)
PRINT $after
```

**Expected:** Prints 3
**Actual:** Prints 3 ✓

## Investigation Findings

### What We Know
1. The bug is **RUNTIME**, not parse-time (code parses successfully)
2. Variable becomes inaccessible **after** IF statement in same block
3. Variables **before** IF statement work fine
4. Bug occurs regardless of IF condition (true/false)
5. Bug occurs with both THEN-only and THEN-ELSE branches
6. Bug does NOT occur in nested FOR loops without IF statements

### Scope Analysis
Examined `crates/ovsm/src/evaluator/mod.rs`:

#### Current Scope Implementation
- `Evaluator` maintains `scopes: Vec<Scope>` (line 16)
- `enter_scope()` pushes new scope (lines 22-24)
- `exit_scope()` pops scope (lines 26-28)
- `set()` walks scope chain backwards to find variable (lines 111-131)
- `get()` walks scope chain backwards to retrieve variable (lines 91-109)

#### Key Methods and Their Scope Behavior

**ForLoop (lines 145-160):**
```rust
fn execute_for_loop(...) -> Result<ExecutionFlow> {
    self.enter_scope();  // Enter loop scope
    for item in items {
        self.env.set(var_name, item)?;
        match self.execute_block(body)? {
            ExecutionFlow::Break => break,
            ExecutionFlow::Continue => continue,
            ExecutionFlow::Return(_) => return ...,
            ExecutionFlow::Continue => {}
        }
    }
    self.exit_scope();  // Exit loop scope
}
```

**WhileLoop (lines 162-177):**
```rust
fn execute_while_loop(...) -> Result<ExecutionFlow> {
    self.enter_scope();  // Enter loop scope
    loop {
        let cond = self.evaluate_expression(condition)?;
        if !is_truthy(&cond) { break; }
        match self.execute_block(body)? {
            ExecutionFlow::Break => break,
            ExecutionFlow::Continue => continue,
            ExecutionFlow::Return(_) => return ...,
            ExecutionFlow::Continue => {}
        }
    }
    self.exit_scope();  // Exit loop scope
}
```

**IfStatement (lines 179-190):**
```rust
fn execute_if_statement(...) -> Result<ExecutionFlow> {
    let cond = self.evaluate_expression(condition)?;
    if is_truthy(&cond) {
        self.execute_block(then_branch)
    } else if let Some(else_branch) = else_branch {
        self.execute_block(else_branch)
    } else {
        Ok(ExecutionFlow::Continue)
    }
}
```

**⚠️ CRITICAL OBSERVATION:** IF statement does NOT call `enter_scope()` or `exit_scope()`!

**execute_block (lines 120-128):**
```rust
fn execute_block(&mut self, stmts: &[Statement]) -> Result<ExecutionFlow> {
    for stmt in stmts {
        match self.execute_statement(stmt)? {
            ExecutionFlow::Continue => {}
            flow => return Ok(flow),
        }
    }
    Ok(ExecutionFlow::Continue)
}
```

**⚠️ CRITICAL OBSERVATION:** `execute_block()` does NOT manage scopes - it just iterates statements!

### Hypothesis 1: Parser Bug (REJECTED)
Initially suspected that `is_end_of_block()` (parser/mod.rs:744) includes `TokenKind::If`, which might cause the parser to incorrectly terminate the FOR body when it sees an IF.

**Rejected because:** The error is runtime (`UndefinedVariable: i`), not parse-time. If the parser failed, we'd get a syntax error or the AST would be malformed.

### Hypothesis 2: IF Creates Implicit Scope (ACTIVE INVESTIGATION)
The IF statement might be creating a scope implicitly somewhere, or the variable lookup is failing due to scope chain corruption after IF execution.

**Evidence supporting:**
- Variables work BEFORE IF
- Variables fail AFTER IF
- Bug occurs in both FOR and WHILE loops
- Bug does NOT occur in nested FOR/FOR (no IF involved)

### Hypothesis 3: Assignment Scoping Issue
When `$after = $i` executes after the IF:
1. Evaluates RHS `$i` via `get()` - walks scope chain
2. Should find `$i` in loop scope (set by FOR loop iterator)
3. Calls `set("after", value)`
4. Since "after" doesn't exist, creates it in current scope

**Potential issue:** Maybe `set()` is creating "after" in the wrong scope, or the scope chain is corrupted?

## Tests Created

1. `examples/test_break_continue.md` - Original BREAK/CONTINUE tests
2. `examples/debug_break.md` - Focused BREAK test
3. `examples/debug_for_return.md` - Tests RETURN in FOR (works!)
4. `examples/debug_if_condition.md` - Minimal IF bug reproduction
5. `examples/test_while_if.md` - WHILE + IF test (hangs forever)

## Next Steps for Debugging

### 1. Add Trace Logging
Modify `evaluator/mod.rs` to log scope operations:
```rust
fn enter_scope(&mut self) {
    eprintln!("ENTER_SCOPE: depth={}", self.scopes.len());
    self.scopes.push(Scope::default());
}

fn exit_scope(&mut self) {
    eprintln!("EXIT_SCOPE: depth={}", self.scopes.len());
    self.scopes.pop();
}

pub fn get(&self, name: &str) -> Result<Value> {
    eprintln!("GET({}): scope_depth={}", name, self.scopes.len());
    // ... existing code
}

pub fn set(&mut self, name: &str, value: Value) -> Result<()> {
    eprintln!("SET({}, {:?}): scope_depth={}", name, value, self.scopes.len());
    // ... existing code
}
```

### 2. Test with Trace Output
```bash
cargo run --example qa_test_runner -- examples/debug_if_condition.md 2>&1 | grep -E "(ENTER|EXIT|GET|SET)"
```

This will show the exact scope operations and reveal where the scope chain breaks.

### 3. Check AST Structure
Verify that the parser is producing the correct AST:
```rust
// Add to minimal test
let code = r#"
FOR $i IN [1..3]:
    $before = $i
    IF true THEN
        $x = 1
    $after = $i
"#;
let scanner = Scanner::new(code);
let tokens = scanner.scan_tokens()?;
let mut parser = Parser::new(tokens);
let ast = parser.parse()?;
eprintln!("AST: {:#?}", ast);
```

### 4. Possible Fixes

**Option A: IF should create scope (like other control flow)**
```rust
fn execute_if_statement(...) -> Result<ExecutionFlow> {
    let cond = self.evaluate_expression(condition)?;
    if is_truthy(&cond) {
        self.enter_scope();
        let result = self.execute_block(then_branch);
        self.exit_scope();
        result
    } else if let Some(else_branch) = else_branch {
        self.enter_scope();
        let result = self.execute_block(else_branch);
        self.exit_scope();
        result
    } else {
        Ok(ExecutionFlow::Continue)
    }
}
```

**Option B: Fix scope lookup in get()**
If the issue is in variable lookup, ensure `get()` properly walks the entire scope chain.

**Option C: Fix parser's block termination**
If the parser is incorrectly parsing the FOR body, fix `is_end_of_block()` or the loop body parser.

## Related Code Files
- `crates/ovsm/src/evaluator/mod.rs` - Scope management (lines 14-131)
- `crates/ovsm/src/evaluator/environment.rs` - Variable storage
- `crates/ovsm/src/parser/mod.rs` - Statement parsing (lines 106-210)

## Impact
This bug makes OVSM unusable for real-world scenarios:
- Cannot use IF statements inside loops reliably
- WHILE loops with IF become infinite loops
- Basic control flow patterns are broken

## Priority
**P0 - BLOCKING** - Must be fixed before any production use.
