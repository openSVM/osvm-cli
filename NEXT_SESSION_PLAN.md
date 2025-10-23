# Next Session Plan: Implement &optional and &key Parameters

**Date Created:** 2025-10-23
**Current Status:** 83% Common Lisp Coverage (90% with loop macro)
**Goal:** Add &optional and &key parameter support (+3% coverage → 93%)

---

## 🎯 Session Objective

Implement Common Lisp-style optional and keyword parameters for OVSM functions.

### Target Syntax
```lisp
;; Optional parameters with defaults
(defun greet (name &optional (greeting "Hello") (punct "!"))
  (str greeting " " name punct))

(greet "Alice")                    ;; → "Hello Alice!"
(greet "Bob" "Hi")                 ;; → "Hi Bob!"
(greet "Charlie" "Hey" "?")        ;; → "Hey Charlie?"

;; Keyword parameters (named arguments)
(defun make-person (&key name (age 0) (city "Unknown"))
  {:name name :age age :city city})

(make-person :name "Alice" :age 30)
;; → {:name "Alice" :age 30 :city "Unknown"}

;; Mixed: required, optional, rest, and keyword
(defun complex (req1 req2 &optional opt1 &rest args &key (debug false))
  ...)
```

---

## 📊 Current Implementation Status

### ✅ What's Already Working
- **Basic parameters:** `(defun foo (x y) ...)`
- **&rest parameters:** `(defun foo (x &rest args) ...)`
- **Loop macro:** ~70% implemented (basic iteration works)
- **Core LISP:** 83% Common Lisp coverage
- **Test pass rate:** 98.4% (436/443 tests)

### 🔍 Where to Modify

#### Files to Change:
1. **`crates/ovsm/src/runtime/lisp_evaluator.rs`**
   - Line 2748: `parse_function_parameters()` - extend to handle &optional/&key
   - Line 2800: `bind_function_parameters()` - extend binding logic
   - Current system supports: `["x", "y", "&rest", "z"]`

2. **`crates/ovsm/src/runtime/value.rs`** (optional refactor)
   - Line 38: `Value::Function { params, body, closure }`
   - Currently: `params: Vec<String>`
   - Could extend to richer structure (but not required for MVP)

3. **`crates/ovsm/tests/`**
   - Create new test file: `optional_key_params_tests.rs`

---

## 🏗️ Implementation Approaches

### Option A: Incremental (Recommended for Next Session)
**Effort:** ~200 lines, 1-2 hours
**Approach:** Keep `Vec<String>` as-is, parse markers inline

**How it works:**
```rust
// Parameters stored as: ["x", "&optional", "y", "10", "z", "20", "&key", "debug", "false"]
// Parse markers during binding to determine:
// - Required: before &optional
// - Optional: between &optional and (&rest or &key)
// - Rest: after &rest
// - Keyword: after &key
```

**Pros:**
- ✅ No AST changes needed
- ✅ Minimal code changes
- ✅ Quick to implement
- ✅ Backward compatible

**Cons:**
- ❌ Less type-safe
- ❌ Defaults stored as strings
- ❌ Slightly hacky

### Option B: Proper Refactor
**Effort:** ~450 lines, 3-4 hours
**Approach:** Create proper `Parameter` enum

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Required(String),
    Optional { name: String, default: Expression },
    Rest(String),
    Keyword { name: String, default: Option<Expression> },
}
```

**Pros:**
- ✅ Type-safe
- ✅ Clean design
- ✅ Easier to maintain
- ✅ Proper error messages

**Cons:**
- ❌ Requires AST changes
- ❌ More files to modify
- ❌ Higher risk of bugs
- ❌ Longer implementation time

---

## 📋 Step-by-Step Implementation Plan (Option A)

### Phase 1: Extend Parameter Parsing (30 min)
**File:** `crates/ovsm/src/runtime/lisp_evaluator.rs:2748`

```rust
fn parse_function_parameters(&self, params_expr: &Expression, context: &str)
    -> Result<Vec<String>>
{
    // Current: handles &rest
    // Add: detect &optional and &key markers
    // Store as: ["x", "&optional", "y", "<default-expr>", "&key", "z", "<default>"]
}
```

**Tasks:**
- [x] Detect `&optional` marker
- [x] Detect `&key` marker
- [x] Parse default values as expressions
- [x] Store in Vec<String> with sentinel values
- [x] Validate marker order (required → &optional → &rest → &key)

### Phase 2: Extend Parameter Binding (45 min)
**File:** `crates/ovsm/src/runtime/lisp_evaluator.rs:2800`

```rust
fn bind_function_parameters(&mut self, params: &[String], args: &[Value], context: &str)
    -> Result<()>
{
    // Current: handles &rest
    // Add: handle &optional with defaults, &key with keyword matching
}
```

**Tasks:**
- [x] Split params into sections: required, optional, rest, keyword
- [x] Bind required params (error if missing)
- [x] Bind optional params with defaults
- [x] Bind rest params (collect remaining positional args)
- [x] Bind keyword params (match :key value pairs)
- [x] Handle missing keyword args with defaults

### Phase 3: Keyword Argument Parsing (30 min)

**Add helper function:**
```rust
fn parse_keyword_args(&self, args: &[Value], start_idx: usize)
    -> Result<HashMap<String, Value>>
{
    // Parse [:key1 val1 :key2 val2] into map
    // Validate all args after start_idx are alternating keywords/values
}
```

### Phase 4: Testing (45 min)

**Create:** `crates/ovsm/tests/optional_key_params_tests.rs`

```rust
// Test cases:
// 1. Optional params with defaults
// 2. Optional params partially provided
// 3. Keyword params only
// 4. Mixed required + optional + keyword
// 5. Error cases (wrong types, missing required, etc.)
```

### Phase 5: Documentation (15 min)

**Update:**
- `FEATURES_STATUS.md` - mark &optional/&key as complete
- `README.md` - add examples
- Code comments

---

## 🧪 Test Cases to Implement

### Basic Optional
```lisp
(defun greet (name &optional (greeting "Hello"))
  (str greeting " " name))

(assert (= (greet "World") "Hello World"))
(assert (= (greet "World" "Hi") "Hi World"))
```

### Basic Keyword
```lisp
(defun make-point (&key (x 0) (y 0))
  {:x x :y y})

(assert (= (make-point) {:x 0 :y 0}))
(assert (= (make-point :x 10) {:x 10 :y 0}))
(assert (= (make-point :y 5 :x 3) {:x 3 :y 5}))
```

### Mixed Parameters
```lisp
(defun complex (a b &optional (c 1) &rest args &key (debug false))
  [a b c args debug])

(assert (= (complex 1 2) [1 2 1 [] false]))
(assert (= (complex 1 2 3 4 5 :debug true) [1 2 3 [4 5] true]))
```

### Error Cases
```lisp
;; Missing required arg
(complex 1)  ;; Should error

;; Invalid keyword
(make-point :z 10)  ;; Should error or ignore

;; Odd number of keyword args
(make-point :x)  ;; Should error
```

---

## 🔧 Helper Reference

### Current System Analysis

**Function storage:**
```rust
// crates/ovsm/src/runtime/value.rs:38
Value::Function {
    params: Vec<String>,           // ["x", "y", "&rest", "z"]
    body: Arc<Expression>,
    closure: Arc<HashMap<String, Value>>,
}
```

**Current binding logic:**
```rust
// crates/ovsm/src/runtime/lisp_evaluator.rs:2800
fn bind_function_parameters(&mut self, params: &[String], args: &[Value]) {
    // Finds &rest marker
    // Binds required params before &rest
    // Collects remaining args into rest param
}
```

---

## 📈 Expected Outcomes

### After Implementation:
- ✅ &optional parameters working (+1.5% coverage)
- ✅ &key parameters working (+1.5% coverage)
- ✅ ~93% Common Lisp coverage (up from 83%)
- ✅ More flexible function APIs
- ✅ Better OVSM script ergonomics

### Future Work (Not This Session):
- destructuring-bind (+2%)
- catch/throw (+2%)
- setf, format, progn (+3%)
- → Path to 100% coverage

---

## 🎬 Ready-to-Use First Prompt

**Copy this to start the next session:**

```
Implement &optional and &key parameters for OVSM functions.

Reference: NEXT_SESSION_PLAN.md

Use Option A (incremental approach):
1. Extend parse_function_parameters() to detect &optional and &key markers
2. Extend bind_function_parameters() to handle defaults and keyword matching
3. Add helper for keyword argument parsing
4. Write comprehensive tests in optional_key_params_tests.rs
5. Update FEATURES_STATUS.md

Target syntax:
(defun greet (name &optional (greeting "Hello") (punct "!"))
  (str greeting " " name punct))

(defun make-person (&key name (age 0) (city "Unknown"))
  {:name name :age age :city city})

Start with parse_function_parameters() in crates/ovsm/src/runtime/lisp_evaluator.rs:2748
```

---

## 📝 Notes

- Loop macro is ~70% complete (basic iteration works, conditionals don't parse yet)
- All OVSM service tests passing (100%)
- Main test suite at 98.4% (436/443 passing)
- Pre-existing UI test failures are unrelated to OVSM work

**Current Coverage:** 83% → **Target:** 93% (+10%)
