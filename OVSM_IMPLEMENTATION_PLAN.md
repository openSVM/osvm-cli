# OVSM LISP Implementation Plan

## Executive Summary

This document outlines the roadmap to complete OVSM as a full-featured LISP dialect for blockchain automation. Current implementation is at **~70% feature coverage**, with the functional programming core (lambdas, MAP/FILTER/REDUCE) complete but missing essential LISP primitives.

**Total Time Estimate:** 6-9 hours to reach 90% coverage
**Current Status:** Production-ready for functional programming, incomplete for general LISP usage
**Priority:** Phase 1 & 2 (4-6 hours) recommended for completeness

---

## Current Implementation Status (as of commit f9771fc)

### ✅ **Completed Features (70% coverage)**

#### **Functional Programming Core**
- ✅ Lambda functions with closures
- ✅ User-defined functions (`defun`)
- ✅ Higher-order functions: `map`, `filter`, `reduce`, `sort`
- ✅ Lexical scoping with `let`
- ✅ Test coverage: 75/75 tests passing (100%)

#### **Control Flow**
- ✅ `if` - Conditional expressions
- ✅ `while` - While loops
- ✅ `for` - For-each iteration
- ✅ `do` - Sequential execution
- ✅ `when` - Single-branch conditional

#### **Variables & State**
- ✅ `define` - Variable definition
- ✅ `set!` - Variable mutation
- ✅ `const` - Constants

#### **Operators**
- ✅ Arithmetic: `+`, `-`, `*`, `/`, `%` (variadic)
- ✅ Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- ✅ Logical: `and`, `or`, `not`

#### **Collections (Partial)**
- ✅ Objects: `KEYS`, `VALUES`, `GET`, `ASSOC`, `HAS_KEY`, `MERGE` (stdlib tools)
- ✅ Arrays: literals, `range`, `length`
- ✅ Data processing: `SUM`, `COUNT`, `FLATTEN`, `UNIQUE`, `REVERSE`

#### **Utilities**
- ✅ `log` - Logging with `:message` and `:value`
- ✅ `now` - Current timestamp
- ✅ `null?`, `empty?` - Predicates
- ✅ `min`, `max` - Aggregation

### ❌ **Missing Features (30% gap)**

See detailed breakdown in Phase 1-4 below.

---

## Implementation Phases

### **PHASE 1: Core LISP Essentials** ⚡ **PRIORITY: CRITICAL**

**Goal:** Complete the fundamental LISP primitives
**Time Estimate:** 3-4 hours
**Impact:** Makes OVSM feel like a "real" LISP

#### Tasks

| Task | Complexity | Time | Implementation Location | Tests Needed |
|------|-----------|------|------------------------|--------------|
| **1.1 `cond` - Multi-condition branching** | Low | 30 min | `lisp_evaluator.rs:eval_cond()` | 3 tests |
| **1.2 `unless` - Inverted `when`** | Low | 15 min | `lisp_evaluator.rs:eval_unless()` | 2 tests |
| **1.3 List ops: `first`, `rest`, `cons`** | Low | 45 min | Special forms (lowercase) | 5 tests |
| **1.4 Property access: `(. obj field)`** | Medium | 1 hour | Parser + evaluator | 4 tests |
| **1.5 Array indexing: `([] arr idx)`** | Medium | 45 min | Parser + evaluator | 4 tests |

#### Implementation Details

##### **1.1 `cond` - Multi-way conditional**

```lisp
;; Syntax
(cond
  ((>= score 90) "A")
  ((>= score 80) "B")
  ((>= score 70) "C")
  (else "F"))

;; Implementation strategy
;; - Parse list of (condition result) pairs
;; - Evaluate conditions sequentially until one is truthy
;; - Return result of first truthy condition
;; - Support 'else' as catch-all (always truthy)
```

**Implementation:**
```rust
fn eval_cond(&mut self, args: &[Argument]) -> Result<Value> {
    for arg in args {
        // Each arg should be an array literal: (condition result)
        if let Expression::ArrayLiteral(pair) = &arg.value {
            if pair.len() != 2 {
                return Err(Error::ParseError("cond clause must be (condition result)".to_string()));
            }

            // Check for 'else' clause
            if matches!(&pair[0], Expression::Variable(v) if v == "else") {
                return self.evaluate_expression(&pair[1]);
            }

            // Evaluate condition
            let cond_val = self.evaluate_expression(&pair[0])?;
            if cond_val.is_truthy() {
                return self.evaluate_expression(&pair[1]);
            }
        } else {
            return Err(Error::ParseError("cond clauses must be arrays".to_string()));
        }
    }

    Ok(Value::Null) // No condition matched
}
```

##### **1.2 `unless` - Inverted when**

```lisp
;; Syntax
(unless (null? data)
  (process data))

;; Equivalent to
(when (not (null? data))
  (process data))
```

**Implementation:**
```rust
fn eval_unless(&mut self, args: &[Argument]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::InvalidArguments {
            tool: "unless".to_string(),
            reason: "Expected at least condition".to_string(),
        });
    }

    // Evaluate condition
    let cond = self.evaluate_expression(&args[0].value)?;

    // If condition is FALSE, execute body
    if !cond.is_truthy() {
        let mut result = Value::Null;
        for arg in &args[1..] {
            result = self.evaluate_expression(&arg.value)?;
        }
        return Ok(result);
    }

    Ok(Value::Null)
}
```

##### **1.3 List operations**

```lisp
;; first - get first element
(first [1 2 3])  ;; → 1

;; rest - get all but first
(rest [1 2 3])   ;; → [2 3]

;; cons - prepend element
(cons 0 [1 2 3]) ;; → [0 1 2 3]
```

**Implementation:** Add as special forms (lowercase) to avoid conflict with stdlib tools.

##### **1.4 & 1.5 Property/Index Access**

These require **parser changes** to handle special syntax:

```lisp
;; Property access
(. obj field)
(. user name)  ;; → "Alice"

;; Array indexing
([] arr idx)
([] nums 0)    ;; → 10
```

**Parser changes needed:** Handle `Expression::FieldAccess` and `Expression::IndexAccess`

---

### **PHASE 2: Type System** 🛡️ **PRIORITY: HIGH**

**Goal:** Runtime type safety (from TYPES_DESIGN.md Phase 1 & 2)
**Time Estimate:** 1-2 hours
**Impact:** Type checking and assertions for safer scripts

#### Tasks

| Task | Complexity | Time | Implementation Location | Tests Needed |
|------|-----------|------|------------------------|--------------|
| **2.1 Type predicates** | Low | 1 hour | Special forms | 8 tests |
| **2.2 `assert` - Runtime assertion** | Low | 30 min | Special form | 3 tests |
| **2.3 `assert-type` - Type assertion** | Low | 15 min | Special form | 3 tests |

#### Implementation Details

##### **2.1 Type Predicates**

```lisp
;; All return boolean
(int? x)       ;; → true if x is integer
(float? x)     ;; → true if x is float
(number? x)    ;; → true if x is int or float
(string? x)    ;; → true if x is string
(bool? x)      ;; → true if x is boolean
(array? x)     ;; → true if x is array
(object? x)    ;; → true if x is object
(function? x)  ;; → true if x is function
```

**Implementation:**
```rust
fn eval_type_predicate(&mut self, type_name: &str, args: &[Argument]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::InvalidArguments {
            tool: type_name.to_string(),
            reason: "Expected 1 argument".to_string(),
        });
    }

    let val = self.evaluate_expression(&args[0].value)?;

    let result = match type_name {
        "int?" => matches!(val, Value::Int(_)),
        "float?" => matches!(val, Value::Float(_)),
        "number?" => matches!(val, Value::Int(_) | Value::Float(_)),
        "string?" => matches!(val, Value::String(_)),
        "bool?" => matches!(val, Value::Bool(_)),
        "array?" => matches!(val, Value::Array(_)),
        "object?" => matches!(val, Value::Object(_)),
        "function?" => matches!(val, Value::Function { .. }),
        _ => false,
    };

    Ok(Value::Bool(result))
}
```

**Register in match statement:**
```rust
"int?" => self.eval_type_predicate("int?", args),
"float?" => self.eval_type_predicate("float?", args),
"number?" => self.eval_type_predicate("number?", args),
"string?" => self.eval_type_predicate("string?", args),
"bool?" => self.eval_type_predicate("bool?", args),
"array?" => self.eval_type_predicate("array?", args),
"object?" => self.eval_type_predicate("object?", args),
"function?" => self.eval_type_predicate("function?", args),
```

##### **2.2 `assert` - Runtime assertion**

```lisp
;; Syntax
(assert condition "error message")
(assert (!= balance 0) "Balance cannot be zero")

;; Throws error if condition is false
```

**Implementation:**
```rust
fn eval_assert(&mut self, args: &[Argument]) -> Result<Value> {
    if args.len() < 1 || args.len() > 2 {
        return Err(Error::InvalidArguments {
            tool: "assert".to_string(),
            reason: "Expected 1-2 arguments (condition, optional message)".to_string(),
        });
    }

    let cond = self.evaluate_expression(&args[0].value)?;

    if !cond.is_truthy() {
        let message = if args.len() == 2 {
            self.evaluate_expression(&args[1].value)?.to_string_value()
        } else {
            "Assertion failed".to_string()
        };

        return Err(Error::AssertionFailed { message });
    }

    Ok(Value::Null)
}
```

**Add to error.rs:**
```rust
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Error {
    // ... existing variants
    AssertionFailed { message: String },
}
```

##### **2.3 `assert-type` - Type assertion**

```lisp
;; Syntax
(assert-type value expected-type)
(assert-type balance int?)
```

**Implementation:**
```rust
fn eval_assert_type(&mut self, args: &[Argument]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::InvalidArguments {
            tool: "assert-type".to_string(),
            reason: "Expected 2 arguments (value, type-predicate)".to_string(),
        });
    }

    let val = self.evaluate_expression(&args[0].value)?;

    // Second arg should be a type predicate (variable reference)
    if let Expression::Variable(type_name) = &args[1].value {
        let predicate_args = vec![Argument { name: None, value: Expression::Variable("_temp".to_string()) }];

        // Temporarily bind value and check type
        self.env.enter_scope();
        self.env.define("_temp".to_string(), val.clone());
        let result = self.eval_type_predicate(type_name, &predicate_args)?;
        self.env.exit_scope();

        if !result.is_truthy() {
            return Err(Error::TypeError {
                expected: type_name.clone(),
                got: val.type_name(),
            });
        }
    } else {
        return Err(Error::ParseError("assert-type expects type predicate as second arg".to_string()));
    }

    Ok(Value::Null)
}
```

---

### **PHASE 3: Loop Control** 🔁 **PRIORITY: MEDIUM**

**Goal:** Advanced loop control (break, continue)
**Time Estimate:** 1 hour
**Impact:** More expressive iteration logic

#### Tasks

| Task | Complexity | Time | Implementation Location | Tests Needed |
|------|-----------|------|------------------------|--------------|
| **3.1 `break` - Exit loop early** | Medium | 30 min | Evaluator state + while/for | 3 tests |
| **3.2 `continue` - Skip iteration** | Medium | 30 min | Evaluator state + while/for | 3 tests |

#### Implementation Details

Requires **control flow state** in evaluator:

```rust
pub struct LispEvaluator {
    env: Environment,
    registry: Arc<ToolRegistry>,
    // Add control flow state
    break_requested: bool,
    continue_requested: bool,
}
```

**While loop modification:**
```rust
fn eval_while(&mut self, args: &[Argument]) -> Result<Value> {
    // ... existing code ...

    while cond_val.is_truthy() {
        for arg in &args[1..] {
            self.evaluate_expression(&arg.value)?;

            // Check for break/continue
            if self.break_requested {
                self.break_requested = false;
                return Ok(Value::Null);
            }
            if self.continue_requested {
                self.continue_requested = false;
                break; // Break inner loop, continue outer while
            }
        }

        cond_val = self.evaluate_expression(&args[0].value)?;
    }

    Ok(Value::Null)
}
```

---

### **PHASE 4: Advanced Features** 🚀 **PRIORITY: LOW**

**Goal:** Professional polish
**Time Estimate:** 2-3 hours
**Impact:** Nice-to-have improvements

#### Tasks

| Task | Complexity | Time | Implementation Location | Tests Needed |
|------|-----------|------|------------------------|--------------|
| **4.1 `for-indexed` - Loop with index** | Medium | 1 hour | Parser + evaluator | 3 tests |
| **4.2 `dissoc` - Remove map key** | Low | 30 min | Special form | 2 tests |
| **4.3 `format` - String formatting** | Medium | 1-2 hours | Special form | 5 tests |

#### Implementation Notes

These are lower priority - the language is "complete enough" without them. Can be implemented incrementally based on user demand.

---

## Testing Strategy

### **Test Coverage Goals**

| Phase | New Tests | Cumulative Total | Coverage Target |
|-------|-----------|------------------|-----------------|
| Current | 75 | 75 | 70% spec |
| Phase 1 | +18 | 93 | 85% spec |
| Phase 2 | +14 | 107 | 90% spec |
| Phase 3 | +6 | 113 | 92% spec |
| Phase 4 | +10 | 123 | 95% spec |

### **Test Structure**

Each feature needs:
1. **Unit test** in `lisp_evaluator.rs`
2. **E2E test** in `lisp_e2e_tests.rs`
3. **Example** in demo script

Example test template:
```rust
#[test]
fn test_cond_multi_branch() {
    let source = r#"
        (define score 85)
        (cond
          ((>= score 90) "A")
          ((>= score 80) "B")
          ((>= score 70) "C")
          (else "F"))
    "#;

    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, Value::String("B".to_string()));
}
```

---

## Success Criteria

### **Phase 1 Complete**
- ✅ All 5 features implemented
- ✅ 18+ tests passing
- ✅ Demo script updated with examples
- ✅ ~85% spec coverage

### **Phase 2 Complete**
- ✅ Type system foundation in place
- ✅ All 8 type predicates working
- ✅ Assertions functional
- ✅ ~90% spec coverage

### **Overall Success**
- ✅ 100+ tests passing
- ✅ 90%+ feature coverage
- ✅ Production-ready for real-world blockchain automation
- ✅ Comprehensive documentation
- ✅ No breaking changes to existing code

---

## Implementation Order

### **Recommended Sequence:**

1. **Day 1: Phase 1 (3-4 hours)**
   - Morning: `cond`, `unless`, list ops (2 hours)
   - Afternoon: Property/index access (2 hours)
   - Result: Core LISP complete

2. **Day 2: Phase 2 (1-2 hours)**
   - Morning: Type predicates (1 hour)
   - Afternoon: Assertions (1 hour)
   - Result: Type safety complete

3. **Day 3: Phase 3 (optional, 1 hour)**
   - Break/continue implementation
   - Result: Advanced loop control

4. **Future: Phase 4 (as needed)**
   - Implement based on user feedback
   - Not blocking for production use

---

## Risk Assessment

### **Low Risk**
- ✅ Phase 1.1-1.3 (simple special forms)
- ✅ Phase 2 (type predicates)

### **Medium Risk**
- ⚠️ Phase 1.4-1.5 (requires parser changes)
- ⚠️ Phase 3 (requires evaluator state management)

### **Mitigation**
- Start with low-risk items first
- Test incrementally
- Keep changes in small, reviewable commits
- Maintain backward compatibility

---

## Dependencies & Prerequisites

### **No External Dependencies Required**
All implementation uses existing infrastructure:
- ✅ Parser already supports S-expressions
- ✅ Evaluator already has special form dispatch
- ✅ Test framework in place
- ✅ Value types complete

### **Only Internal Changes Needed**
- Add new special forms to match statement
- Add helper functions to evaluator
- Extend parser for property/index access (Phase 1.4-1.5)
- Add control flow state fields (Phase 3)

---

## Maintenance & Future Work

### **After Phase 1+2 (Recommended Minimum)**
OVSM will be feature-complete for:
- ✅ Functional programming (already done)
- ✅ Classic LISP operations
- ✅ Type safety
- ✅ Real-world blockchain scripting

### **Long-term Enhancements (Beyond this plan)**
- Pattern matching (from TYPES_DESIGN.md Phase 4)
- Struct types (from TYPES_DESIGN.md Phase 3)
- Specs (from TYPES_DESIGN.md Phase 5)
- Macros (if needed for metaprogramming)

---

## Conclusion

**Current State:** 70% feature-complete LISP with excellent functional programming core

**After Phase 1+2:** 90% feature-complete, production-ready blockchain scripting language

**Time Investment:** 4-6 hours for "complete enough" implementation

**Recommendation:** Execute Phase 1 & 2 sequentially, then evaluate if Phase 3/4 are needed based on real-world usage.

---

**Document Version:** 1.0
**Last Updated:** 2025-10-22
**Author:** Claude Code
**Status:** Ready for implementation
