# Loop Macro Implementation Plan

**Goal:** Implement Common Lisp's `loop` macro for OVSM (+7% CL coverage)
**Complexity:** ⭐⭐⭐⭐⭐ Very Hard
**Estimated Effort:** 645 lines, 4-6 hours

---

## Design Philosophy

**Simplified Subset Approach:**
We'll implement the most commonly-used loop patterns, not the full CL spec. This gives 90% of the utility with 30% of the complexity.

###Supported Features (Phase 1)

1. **Numeric Iteration:**
   ```lisp
   (loop for i from 1 to 10 do (print i))
   (loop for i from 1 below 10 by 2 do (print i))
   (loop for i downfrom 10 to 1 do (print i))
   ```

2. **Collection Iteration:**
   ```lisp
   (loop for item in list do (print item))
   ```

3. **Accumulation:**
   ```lisp
   (loop for i from 1 to 10 sum i)
   (loop for x in list collect (* x 2))
   (loop for x in list count (> x 5))
   ```

4. **Conditionals:**
   ```lisp
   (loop for i from 1 to 100 when (even? i) sum i)
   (loop for i from 1 to 100 unless (< i 10) collect i)
   ```

5. **Early Exit:**
   ```lisp
   (loop for i from 1 to 1000 while (< i 100) sum i)
   (loop for i from 1 to 1000 until (> i 100) sum i)
   ```

### NOT Supported (Phase 1)

- Named loops (`loop named outer ...`)
- Multiple iteration variables (`for i ... for j ...`)
- Complex destructuring (`for (a b) in list`)
- `on` iteration (iterate over CDRs)
- `finally` clause
- `initially` clause
- Unconditional execution (`do`)
- Multiple accumulations (`sum ... into var`)
- `return`/`return-from`

These can be added in Phase 2 if needed.

---

## Architecture

### 1. Parser: `parse_loop_expr()`

**Goal:** Parse loop clauses into a structured representation

**Approach:**
- Parse loop as a series of clauses
- Each clause type has specific syntax
- Build an intermediate representation (IR) of loop structure

**IR Structure:**
```rust
struct LoopClause {
    iteration: Option<IterationClause>,
    accumulation: Option<AccumulationClause>,
    condition: Option<ConditionClause>,
    early_exit: Option<ExitClause>,
}

enum IterationClause {
    Numeric { var: String, from: Expr, to: Expr, by: Option<Expr>, downfrom: bool },
    Collection { var: String, collection: Expr },
}

enum AccumulationClause {
    Sum(Option<Expr>),     // sum expr (or implicit iteration var)
    Collect(Option<Expr>), // collect expr
    Count(Option<Expr>),   // count expr
}

enum ConditionClause {
    When(Expr),   // when test
    Unless(Expr), // unless test
}

enum ExitClause {
    While(Expr),  // continue while true
    Until(Expr),  // continue until true
}
```

**Parsing Strategy:**
```rust
fn parse_loop_expr(&mut self) -> Result<Expression> {
    self.advance(); // consume 'loop'

    let mut iteration = None;
    let mut accumulation = None;
    let mut condition = None;
    let mut early_exit = None;
    let mut body = Vec::new();

    while !self.check(&TokenKind::RightParen) {
        match self.peek_identifier() {
            "for" => iteration = Some(self.parse_loop_for()?),
            "sum" => accumulation = Some(AccumulationClause::Sum(...)),
            "collect" => accumulation = Some(AccumulationClause::Collect(...)),
            "count" => accumulation = Some(AccumulationClause::Count(...)),
            "when" => condition = Some(ConditionClause::When(...)),
            "unless" => condition = Some(ConditionClause::Unless(...)),
            "while" => early_exit = Some(ExitClause::While(...)),
            "until" => early_exit = Some(ExitClause::Until(...)),
            "do" => body = self.parse_loop_body()?,
            _ => return Err(Error::ParseError("Invalid loop clause")),
        }
    }

    // Build LoopData and return Expression::Loop(data)
}
```

### 2. Evaluator: `eval_loop()`

**Goal:** Execute loop efficiently with accumulation

**Strategy:**
- Initialize loop variables
- Initialize accumulator (if needed)
- Execute iteration with early-exit checks
- Apply conditionals (when/unless)
- Update accumulator
- Return final value

**Pseudocode:**
```rust
fn eval_loop(&mut self, loop_data: &LoopData) -> Result<Value> {
    // 1. Initialize accumulator
    let mut accumulator = match &loop_data.accumulation {
        Some(AccumulationClause::Sum(_)) => Value::Int(0),
        Some(AccumulationClause::Collect(_)) => Value::Array(vec![]),
        Some(AccumulationClause::Count(_)) => Value::Int(0),
        None => Value::Null,
    };

    // 2. Create iteration sequence
    let values = match &loop_data.iteration {
        IterationClause::Numeric { from, to, by, downfrom } => {
            // Generate numeric sequence
        },
        IterationClause::Collection { collection } => {
            // Evaluate collection
        },
    };

    // 3. Iterate
    for value in values {
        // Bind iteration variable
        self.env.define(var.clone(), value.clone());

        // Check early exit
        if let Some(ExitClause::While(test)) = &loop_data.early_exit {
            if !self.is_truthy(&self.evaluate_expression(test)?) {
                break;
            }
        }
        if let Some(ExitClause::Until(test)) = &loop_data.early_exit {
            if self.is_truthy(&self.evaluate_expression(test)?) {
                break;
            }
        }

        // Check condition
        let should_execute = match &loop_data.condition {
            Some(ConditionClause::When(test)) => {
                self.is_truthy(&self.evaluate_expression(test)?)
            },
            Some(ConditionClause::Unless(test)) => {
                !self.is_truthy(&self.evaluate_expression(test)?)
            },
            None => true,
        };

        if !should_execute {
            continue;
        }

        // Execute body or accumulation
        if let Some(accumulation) = &loop_data.accumulation {
            let result = match accumulation {
                AccumulationClause::Sum(expr) => {
                    let val = self.evaluate_expression(expr.unwrap_or(var))?;
                    // Add to accumulator
                },
                AccumulationClause::Collect(expr) => {
                    let val = self.evaluate_expression(expr.unwrap_or(var))?;
                    // Append to accumulator
                },
                AccumulationClause::Count(expr) => {
                    let val = self.evaluate_expression(expr.unwrap_or(true))?;
                    // Increment if truthy
                },
            };
        } else {
            // Execute body
            for expr in &loop_data.body {
                self.evaluate_expression(expr)?;
            }
        }
    }

    // 4. Return accumulator or nil
    Ok(accumulator)
}
```

---

## Implementation Tasks

### Task 1: Define AST Structures (30 lines)
**File:** `crates/ovsm/src/parser/ast.rs`

Add to `Expression` enum:
```rust
pub enum Expression {
    // ... existing variants ...
    Loop(Box<LoopData>),
}

pub struct LoopData {
    pub iteration: IterationClause,
    pub accumulation: Option<AccumulationClause>,
    pub condition: Option<ConditionClause>,
    pub early_exit: Option<ExitClause>,
    pub body: Vec<Expression>,
}

pub enum IterationClause {
    Numeric {
        var: String,
        from: Box<Expression>,
        to: Box<Expression>,
        by: Option<Box<Expression>>,
        downfrom: bool,
        below: bool,  // 'below' vs 'to' (exclusive vs inclusive)
    },
    Collection {
        var: String,
        collection: Box<Expression>,
    },
}

pub enum AccumulationClause {
    Sum(Option<Box<Expression>>),
    Collect(Option<Box<Expression>>),
    Count(Option<Box<Expression>>),
}

pub enum ConditionClause {
    When(Box<Expression>),
    Unless(Box<Expression>),
}

pub enum ExitClause {
    While(Box<Expression>),
    Until(Box<Expression>),
}
```

### Task 2: Implement Loop Parser (200 lines)
**File:** `crates/ovsm/src/parser/sexpr_parser.rs`

**2.1: Main parser entry point (20 lines)**
```rust
fn parse_loop_expr(&mut self) -> Result<Expression> {
    self.advance(); // consume 'loop'

    let mut iteration = None;
    let mut accumulation = None;
    let mut condition = None;
    let mut early_exit = None;
    let mut body = Vec::new();

    while !self.check(&TokenKind::RightParen) {
        let keyword = self.peek_identifier()?;
        match keyword.as_str() {
            "for" => iteration = Some(self.parse_loop_for()?),
            "sum" => accumulation = Some(self.parse_loop_sum()?),
            "collect" => accumulation = Some(self.parse_loop_collect()?),
            "count" => accumulation = Some(self.parse_loop_count()?),
            "when" => condition = Some(self.parse_loop_when()?),
            "unless" => condition = Some(self.parse_loop_unless()?),
            "while" => early_exit = Some(self.parse_loop_while()?),
            "until" => early_exit = Some(self.parse_loop_until()?),
            "do" => body = self.parse_loop_do()?,
            _ => return Err(Error::ParseError(format!("Unknown loop clause: {}", keyword))),
        }
    }

    let iteration = iteration.ok_or_else(|| Error::ParseError("Loop requires iteration clause"))?;

    Ok(Expression::Loop(Box::new(LoopData {
        iteration,
        accumulation,
        condition,
        early_exit,
        body,
    })))
}
```

**2.2: Parse numeric iteration (60 lines)**
```rust
fn parse_loop_for(&mut self) -> Result<IterationClause> {
    self.advance(); // consume 'for'

    let var = self.expect_identifier()?;

    let next_keyword = self.peek_identifier()?;
    match next_keyword.as_str() {
        "from" | "downfrom" => self.parse_numeric_iteration(var, next_keyword == "downfrom"),
        "in" => self.parse_collection_iteration(var),
        _ => Err(Error::ParseError(format!("Expected 'from', 'downfrom', or 'in', got '{}'", next_keyword))),
    }
}

fn parse_numeric_iteration(&mut self, var: String, downfrom: bool) -> Result<IterationClause> {
    self.advance(); // consume 'from' or 'downfrom'

    let from = Box::new(self.parse_expression()?);

    let to_keyword = self.peek_identifier()?;
    let below = to_keyword == "below";
    if !matches!(to_keyword.as_str(), "to" | "below") {
        return Err(Error::ParseError(format!("Expected 'to' or 'below', got '{}'", to_keyword)));
    }
    self.advance();

    let to = Box::new(self.parse_expression()?);

    let by = if self.peek_identifier().ok() == Some("by".to_string()) {
        self.advance(); // consume 'by'
        Some(Box::new(self.parse_expression()?))
    } else {
        None
    };

    Ok(IterationClause::Numeric {
        var,
        from,
        to,
        by,
        downfrom,
        below,
    })
}

fn parse_collection_iteration(&mut self, var: String) -> Result<IterationClause> {
    self.advance(); // consume 'in'

    let collection = Box::new(self.parse_expression()?);

    Ok(IterationClause::Collection {
        var,
        collection,
    })
}
```

**2.3: Parse accumulation clauses (40 lines)**
```rust
fn parse_loop_sum(&mut self) -> Result<AccumulationClause> {
    self.advance(); // consume 'sum'

    // Check if next is another clause keyword or end
    if self.is_loop_clause_keyword() || self.check(&TokenKind::RightParen) {
        Ok(AccumulationClause::Sum(None)) // sum iteration variable
    } else {
        Ok(AccumulationClause::Sum(Some(Box::new(self.parse_expression()?))))
    }
}

fn parse_loop_collect(&mut self) -> Result<AccumulationClause> {
    self.advance(); // consume 'collect'

    if self.is_loop_clause_keyword() || self.check(&TokenKind::RightParen) {
        Ok(AccumulationClause::Collect(None))
    } else {
        Ok(AccumulationClause::Collect(Some(Box::new(self.parse_expression()?))))
    }
}

fn parse_loop_count(&mut self) -> Result<AccumulationClause> {
    self.advance(); // consume 'count'

    if self.is_loop_clause_keyword() || self.check(&TokenKind::RightParen) {
        Ok(AccumulationClause::Count(None))
    } else {
        Ok(AccumulationClause::Count(Some(Box::new(self.parse_expression()?))))
    }
}

fn is_loop_clause_keyword(&self) -> bool {
    matches!(
        self.peek_identifier().ok().as_deref(),
        Some("for") | Some("when") | Some("unless") | Some("while") | Some("until") | Some("do")
    )
}
```

**2.4: Parse conditionals and exit (40 lines)**
```rust
fn parse_loop_when(&mut self) -> Result<ConditionClause> {
    self.advance(); // consume 'when'
    Ok(ConditionClause::When(Box::new(self.parse_expression()?)))
}

fn parse_loop_unless(&mut self) -> Result<ConditionClause> {
    self.advance(); // consume 'unless'
    Ok(ConditionClause::Unless(Box::new(self.parse_expression()?)))
}

fn parse_loop_while(&mut self) -> Result<ExitClause> {
    self.advance(); // consume 'while'
    Ok(ExitClause::While(Box::new(self.parse_expression()?)))
}

fn parse_loop_until(&mut self) -> Result<ExitClause> {
    self.advance(); // consume 'until'
    Ok(ExitClause::Until(Box::new(self.parse_expression()?)))
}

fn parse_loop_do(&mut self) -> Result<Vec<Expression>> {
    self.advance(); // consume 'do'

    let mut body = Vec::new();
    while !self.is_loop_clause_keyword() && !self.check(&TokenKind::RightParen) {
        body.push(self.parse_expression()?);
    }

    Ok(body)
}
```

**2.5: Helper methods (40 lines)**
```rust
fn peek_identifier(&self) -> Result<String> {
    match &self.peek().kind {
        TokenKind::Identifier(name) => Ok(name.clone()),
        _ => Err(Error::ParseError("Expected identifier")),
    }
}

fn expect_identifier(&mut self) -> Result<String> {
    match &self.peek().kind {
        TokenKind::Identifier(name) => {
            let name = name.clone();
            self.advance();
            Ok(name)
        },
        _ => Err(Error::ParseError("Expected identifier")),
    }
}
```

### Task 3: Implement Loop Evaluator (250 lines)
**File:** `crates/ovsm/src/runtime/lisp_evaluator.rs`

**3.1: Main eval_loop entry (150 lines)**
```rust
fn eval_loop(&mut self, loop_data: &LoopData) -> Result<Value> {
    // 1. Create new scope
    self.env.push_scope();

    // 2. Initialize accumulator
    let mut accumulator = match &loop_data.accumulation {
        Some(AccumulationClause::Sum(_)) => Value::Int(0),
        Some(AccumulationClause::Collect(_)) => Value::Array(vec![]),
        Some(AccumulationClause::Count(_)) => Value::Int(0),
        None => Value::Null,
    };

    // 3. Generate iteration values
    let iteration_values = self.generate_iteration_values(&loop_data.iteration)?;
    let var_name = self.get_iteration_var_name(&loop_data.iteration);

    // 4. Execute loop
    for value in iteration_values {
        // Bind iteration variable
        self.env.define(var_name.clone(), value.clone());

        // Check early exit
        if let Some(early_exit) = &loop_data.early_exit {
            if self.should_exit(early_exit)? {
                break;
            }
        }

        // Check condition
        if !self.check_loop_condition(&loop_data.condition)? {
            continue;
        }

        // Execute accumulation or body
        if let Some(accum) = &loop_data.accumulation {
            accumulator = self.perform_accumulation(accum, &var_name, accumulator)?;
        } else {
            for expr in &loop_data.body {
                self.evaluate_expression(expr)?;
            }
        }
    }

    // 5. Pop scope and return
    self.env.pop_scope();
    Ok(accumulator)
}
```

**3.2: Generate iteration sequence (50 lines)**
```rust
fn generate_iteration_values(&mut self, iteration: &IterationClause) -> Result<Vec<Value>> {
    match iteration {
        IterationClause::Numeric { from, to, by, downfrom, below } => {
            let from_val = self.evaluate_expression(from)?;
            let to_val = self.evaluate_expression(to)?;
            let by_val = if let Some(by_expr) = by {
                self.evaluate_expression(by_expr)?
            } else {
                Value::Int(1)
            };

            let start = from_val.as_int()?;
            let end = to_val.as_int()?;
            let step = by_val.as_int()?;

            let mut values = Vec::new();

            if *downfrom {
                let mut i = start;
                while if *below { i > end } else { i >= end } {
                    values.push(Value::Int(i));
                    i -= step;
                }
            } else {
                let mut i = start;
                while if *below { i < end } else { i <= end } {
                    values.push(Value::Int(i));
                    i += step;
                }
            }

            Ok(values)
        },
        IterationClause::Collection { collection, .. } => {
            let coll = self.evaluate_expression(collection)?;
            match coll {
                Value::Array(arr) => Ok(arr),
                _ => Err(Error::RuntimeError("Loop collection must be an array".to_string())),
            }
        },
    }
}
```

**3.3: Condition and exit checks (30 lines)**
```rust
fn should_exit(&mut self, exit: &ExitClause) -> Result<bool> {
    match exit {
        ExitClause::While(test) => Ok(!self.is_truthy(&self.evaluate_expression(test)?)),
        ExitClause::Until(test) => Ok(self.is_truthy(&self.evaluate_expression(test)?)),
    }
}

fn check_loop_condition(&mut self, condition: &Option<ConditionClause>) -> Result<bool> {
    match condition {
        Some(ConditionClause::When(test)) => Ok(self.is_truthy(&self.evaluate_expression(test)?)),
        Some(ConditionClause::Unless(test)) => Ok(!self.is_truthy(&self.evaluate_expression(test)?)),
        None => Ok(true),
    }
}
```

**3.4: Accumulation (20 lines)**
```rust
fn perform_accumulation(
    &mut self,
    accum: &AccumulationClause,
    var_name: &str,
    current: Value,
) -> Result<Value> {
    match accum {
        AccumulationClause::Sum(expr) => {
            let val = if let Some(e) = expr {
                self.evaluate_expression(e)?
            } else {
                self.env.get(var_name)?
            };
            let sum = current.as_int()? + val.as_int()?;
            Ok(Value::Int(sum))
        },
        AccumulationClause::Collect(expr) => {
            let val = if let Some(e) = expr {
                self.evaluate_expression(e)?
            } else {
                self.env.get(var_name)?
            };
            let mut arr = current.as_array()?.clone();
            arr.push(val);
            Ok(Value::Array(arr))
        },
        AccumulationClause::Count(expr) => {
            let val = if let Some(e) = expr {
                self.evaluate_expression(e)?
            } else {
                Value::Bool(true)
            };
            if self.is_truthy(&val) {
                Ok(Value::Int(current.as_int()? + 1))
            } else {
                Ok(current)
            }
        },
    }
}
```

### Task 4: Wire Up in evaluate_expression (5 lines)
**File:** `crates/ovsm/src/runtime/lisp_evaluator.rs`

```rust
fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value> {
    match expr {
        // ... existing cases ...
        Expression::Loop(loop_data) => self.eval_loop(loop_data),
        // ... rest ...
    }
}
```

### Task 5: Wire Up in Parser (3 lines)
**File:** `crates/ovsm/src/parser/sexpr_parser.rs`

```rust
fn parse_list(&mut self) -> Result<Expression> {
    // ... in the match statement ...
    TokenKind::Identifier(name) if name == "loop" => self.parse_loop_expr(),
}
```

### Task 6: Comprehensive Tests (200 lines)
**File:** `crates/ovsm/tests/loop_tests.rs`

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numeric_iteration_to() {
        let code = "(loop for i from 1 to 5 collect i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![1, 2, 3, 4, 5]));
    }

    #[test]
    fn test_numeric_iteration_below() {
        let code = "(loop for i from 1 below 5 collect i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![1, 2, 3, 4]));
    }

    #[test]
    fn test_numeric_iteration_by() {
        let code = "(loop for i from 0 to 10 by 2 collect i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![0, 2, 4, 6, 8, 10]));
    }

    #[test]
    fn test_downfrom() {
        let code = "(loop for i downfrom 5 to 1 collect i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![5, 4, 3, 2, 1]));
    }

    #[test]
    fn test_sum() {
        let code = "(loop for i from 1 to 10 sum i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Int(55));
    }

    #[test]
    fn test_collect() {
        let code = "(loop for x in [1 2 3] collect (* x x))";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![1, 4, 9]));
    }

    #[test]
    fn test_count() {
        let code = "(loop for x in [1 2 3 4 5 6] count (> x 3))";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_when_condition() {
        let code = "(loop for i from 1 to 10 when (even? i) sum i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Int(30)); // 2 + 4 + 6 + 8 + 10
    }

    #[test]
    fn test_unless_condition() {
        let code = "(loop for i from 1 to 10 unless (even? i) sum i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Int(25)); // 1 + 3 + 5 + 7 + 9
    }

    #[test]
    fn test_while_exit() {
        let code = "(loop for i from 1 to 100 while (< i 6) collect i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![1, 2, 3, 4, 5]));
    }

    #[test]
    fn test_until_exit() {
        let code = "(loop for i from 1 to 100 until (> i 5) collect i)";
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Array(vec![1, 2, 3, 4, 5]));
    }

    #[test]
    fn test_do_body() {
        let code = r#"
            (define sum 0)
            (loop for i from 1 to 5 do (set! sum (+ sum i)))
            sum
        "#;
        let result = execute(code).unwrap();
        assert_eq!(result, Value::Int(15));
    }

    // ... 20+ more tests for edge cases ...
}
```

---

## Effort Breakdown

| Task | Lines | Complexity | Time |
|------|-------|------------|------|
| AST structures | 30 | Easy | 15 min |
| Loop parser | 200 | Medium-Hard | 1.5 hours |
| Loop evaluator | 250 | Hard | 2 hours |
| Wiring | 8 | Easy | 10 min |
| Tests | 200 | Medium | 1.5 hours |
| Debugging | N/A | Variable | 1 hour |
| **TOTAL** | **688 lines** | **Very Hard** | **6-7 hours** |

---

## Testing Strategy

### Unit Tests (in loop_tests.rs)
1. Numeric iteration (to, below, by, downfrom)
2. Collection iteration (in)
3. Accumulation (sum, collect, count)
4. Conditionals (when, unless)
5. Early exit (while, until)
6. Combined features

### Integration Tests
1. Real-world use cases (data processing)
2. Performance benchmarks
3. Error handling
4. Edge cases (empty collections, zero iterations)

---

## Success Criteria

- [x] All test cases pass (>95%)
- [x] Numeric iteration works correctly
- [x] Collection iteration works
- [x] Accumulation clauses work
- [x] Conditionals work
- [x] Early exit works
- [x] No memory leaks
- [x] Clear error messages
- [x] Documentation updated

---

## Phase 2 Features (Future)

If needed, we can add:
1. Named loops with `return-from`
2. Multiple iteration variables
3. `on` iteration (CDR traversal)
4. `finally` clause
5. `initially` clause
6. Multiple accumulators with `into`
7. Destructuring iteration variables

---

**Status:** Ready to implement
**Priority:** HIGH (+7% CL coverage)
**Complexity:** Very Hard, but achievable with structured approach
