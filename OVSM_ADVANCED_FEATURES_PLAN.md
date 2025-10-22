# OVSM Advanced Features Implementation Plan

## Overview

This document outlines the implementation plan for advanced Common Lisp features that will bring OVSM to ~85-90% Common Lisp compatibility.

**Current Status:** OVSM has ~50-60% of Common Lisp core features
**Target:** 85-90% with these 6 advanced features
**Estimated Total Effort:** ~50,000 tokens, 1,200 lines of code
**Estimated Time:** 5-8 hours of focused implementation

---

## Phase 1: &rest Parameters (Variadic Functions/Macros)

### Goal
Support variadic parameters in functions and macros using `&rest` keyword.

### Current Limitation
```lisp
;; Can't do this yet:
(defun sum (&rest numbers)
  (reduce + numbers 0))

(sum 1 2 3 4 5)  ; Should return 15
```

### Design

#### Syntax
- `(defun name (required-param &rest rest-param) body)`
- `(defmacro name (required-param &rest rest-param) body)`
- `&rest` must come last in parameter list
- Collects remaining arguments into an array

#### Implementation Tasks

| Task | File | Effort | Description |
|------|------|--------|-------------|
| 1. Update parameter parsing | `lisp_evaluator.rs` | 80 lines | Parse `&rest` in defun/defmacro |
| 2. Modify function application | `lisp_evaluator.rs` | 60 lines | Collect variadic args into array |
| 3. Update macro expansion | `lisp_evaluator.rs` | 40 lines | Handle &rest in macro params |
| 4. Add validation | `lisp_evaluator.rs` | 30 lines | Ensure &rest is last, only one |
| 5. Write tests | `tests/varargs_tests.rs` | 120 lines | Cover functions + macros |
| **Total** | | **330 lines** | **~8,000 tokens** |

#### Examples After Implementation

```lisp
;; Variadic function
(defun sum (&rest numbers)
  (reduce + numbers 0))

(sum 1 2 3)  ; => 6
(sum)        ; => 0

;; Mixed parameters
(defun greet (greeting &rest names)
  (for (name names)
    (log :message (str greeting " " name))))

(greet "Hello" "Alice" "Bob" "Charlie")

;; Variadic macro
(defmacro when (condition &rest body)
  `(if ,condition
       (do ,@body)
       nil))
```

#### Difficulty: ⭐⭐☆☆☆ (Easy-Medium)
**Why:** Straightforward pattern - just collect extra args into array

---

## Phase 2: let* (Sequential Binding)

### Goal
Implement `let*` where bindings can reference earlier bindings in the same form.

### Current Limitation
```lisp
;; With let, this fails (x not defined when y is bound):
(let ((x 10)
      (y (+ x 5)))  ; ERROR: x undefined
  y)

;; Need let* for sequential bindings:
(let* ((x 10)
       (y (+ x 5)))  ; OK: x is visible here
  y)  ; => 15
```

### Design

#### Syntax
- `(let* ((var1 val1) (var2 val2) ...) body)`
- Each binding can reference previous bindings
- Semantically equivalent to nested `let` forms

#### Implementation Tasks

| Task | File | Effort | Description |
|------|------|--------|-------------|
| 1. Add eval_let_star method | `lisp_evaluator.rs` | 70 lines | Sequential binding logic |
| 2. Add to special forms | `lisp_evaluator.rs` | 5 lines | Register "let*" handler |
| 3. Environment scoping | `lisp_evaluator.rs` | 40 lines | Progressive scope extension |
| 4. Write tests | `tests/let_star_tests.rs` | 80 lines | Nested references |
| **Total** | | **195 lines** | **~4,500 tokens** |

#### Examples After Implementation

```lisp
;; Sequential dependencies
(let* ((x 10)
       (y (* x 2))
       (z (+ x y)))
  z)  ; => 30

;; Shadowing previous binding
(let* ((x 5)
       (x (* x 2)))  ; x refers to previous x
  x)  ; => 10

;; Common pattern: temporary calculations
(let* ((raw-data (fetch-data))
       (filtered (filter valid? raw-data))
       (sorted (sort filtered)))
  (process sorted))
```

#### Difficulty: ⭐⭐☆☆☆ (Easy-Medium)
**Why:** Similar to existing `let`, just iterate and extend scope progressively

---

## Phase 3: flet (Local Function Definitions)

### Goal
Define functions with local scope (non-recursive).

### Current Limitation
```lisp
;; Can't define local helper functions:
(define (process-list items)
  ;; Want helper function here:
  (flet ((double (x) (* x 2)))
    (map double items)))
```

### Design

#### Syntax
- `(flet ((name1 (params1) body1) (name2 (params2) body2)) body)`
- Functions defined in parallel (can't call each other)
- Lexically scoped to the `flet` body

#### Implementation Tasks

| Task | File | Effort | Description |
|------|------|--------|-------------|
| 1. Add eval_flet method | `lisp_evaluator.rs` | 90 lines | Parse function definitions |
| 2. Create local env | `lisp_evaluator.rs` | 50 lines | Extend environment |
| 3. Function value creation | `lisp_evaluator.rs` | 40 lines | Create Value::Function |
| 4. Add to special forms | `lisp_evaluator.rs` | 5 lines | Register "flet" |
| 5. Write tests | `tests/flet_tests.rs` | 100 lines | Local scope, shadowing |
| **Total** | | **285 lines** | **~6,500 tokens** |

#### Examples After Implementation

```lisp
;; Single local function
(flet ((square (x) (* x x)))
  (+ (square 3) (square 4)))  ; => 25

;; Multiple local functions
(flet ((add (a b) (+ a b))
       (mul (a b) (* a b)))
  (add (mul 2 3) (mul 4 5)))  ; => 26

;; Shadowing global function
(defun foo () "global")

(flet ((foo () "local"))
  (foo))  ; => "local"

(foo)  ; => "global"
```

#### Difficulty: ⭐⭐⭐☆☆ (Medium)
**Why:** Need environment management, but simpler than `labels` (no recursion)

---

## Phase 4: labels (Recursive Local Functions)

### Goal
Define recursive local functions that can call themselves and each other.

### Current Limitation
```lisp
;; flet functions can't be recursive:
(flet ((factorial (n)
         (if (<= n 1)
             1
             (* n (factorial (- n 1))))))  ; ERROR: factorial not defined
  (factorial 5))
```

### Design

#### Syntax
- `(labels ((name1 (params1) body1) (name2 (params2) body2)) body)`
- Functions can call themselves (recursion)
- Functions can call each other (mutual recursion)

#### Implementation Tasks

| Task | File | Effort | Description |
|------|------|--------|-------------|
| 1. Add eval_labels method | `lisp_evaluator.rs` | 100 lines | Parse + bind functions |
| 2. Two-pass binding | `lisp_evaluator.rs` | 70 lines | Pre-bind names, then bodies |
| 3. Closure capture | `lisp_evaluator.rs` | 50 lines | Functions see each other |
| 4. Add to special forms | `lisp_evaluator.rs` | 5 lines | Register "labels" |
| 5. Write tests | `tests/labels_tests.rs` | 120 lines | Recursion, mutual recursion |
| **Total** | | **345 lines** | **~8,000 tokens** |

#### Examples After Implementation

```lisp
;; Recursive factorial
(labels ((factorial (n)
           (if (<= n 1)
               1
               (* n (factorial (- n 1))))))
  (factorial 5))  ; => 120

;; Mutual recursion: even/odd
(labels ((is-even (n)
           (if (== n 0)
               true
               (is-odd (- n 1))))
         (is-odd (n)
           (if (== n 0)
               false
               (is-even (- n 1)))))
  (is-even 42))  ; => true

;; Tree traversal
(labels ((sum-tree (node)
           (if (null? node)
               0
               (+ (node :value)
                  (sum-tree (node :left))
                  (sum-tree (node :right))))))
  (sum-tree tree))
```

#### Difficulty: ⭐⭐⭐⭐☆ (Medium-Hard)
**Why:** Two-pass binding for mutual recursion, closure management tricky

---

## Phase 5: case/typecase (Pattern Matching)

### Goal
Efficient multi-way branching based on value or type.

### Current Limitation
```lisp
;; Have to use nested if/cond:
(cond
  ((== x 1) "one")
  ((== x 2) "two")
  ((== x 3) "three")
  (else "other"))

;; Want cleaner syntax:
(case x
  (1 "one")
  (2 "two")
  (3 "three")
  (else "other"))
```

### Design

#### Syntax
```lisp
;; case - match by value equality
(case expr
  (value1 result1)
  (value2 result2)
  ...
  (else default))

;; typecase - match by type
(typecase expr
  (int "it's an integer")
  (string "it's a string")
  (array "it's an array")
  (else "something else"))
```

#### Implementation Tasks

| Task | File | Effort | Description |
|------|------|--------|-------------|
| 1. Add eval_case method | `lisp_evaluator.rs` | 80 lines | Value matching |
| 2. Add eval_typecase method | `lisp_evaluator.rs` | 70 lines | Type matching |
| 3. Clause parsing | `lisp_evaluator.rs` | 50 lines | Parse (value body) pairs |
| 4. Else clause handling | `lisp_evaluator.rs` | 30 lines | Default case |
| 5. Add to special forms | `lisp_evaluator.rs` | 10 lines | Register both |
| 6. Write tests | `tests/case_tests.rs` | 100 lines | All patterns |
| **Total** | | **340 lines** | **~7,500 tokens** |

#### Examples After Implementation

```lisp
;; case - value matching
(define day 3)
(case day
  (1 "Monday")
  (2 "Tuesday")
  (3 "Wednesday")
  (4 "Thursday")
  (5 "Friday")
  ((6 7) "Weekend")  ; Multiple values
  (else "Invalid"))  ; => "Wednesday"

;; typecase - type matching
(defun describe (x)
  (typecase x
    (int "an integer")
    (float "a float")
    (string "a string")
    (array "an array")
    (object "an object")
    (function "a function")
    (else "unknown type")))

(describe 42)        ; => "an integer"
(describe "hello")   ; => "a string"

;; Nested case
(case (operation)
  (add
    (case (mode)
      (fast (fast-add a b))
      (safe (safe-add a b))
      (else (add a b))))
  (multiply
    (multiply a b)))
```

#### Difficulty: ⭐⭐⭐☆☆ (Medium)
**Why:** Straightforward conditional logic, type introspection already available

---

## Phase 6: loop Macro (Iteration Facility)

### Goal
Implement Common Lisp's powerful `loop` macro for iteration.

### Current Limitation
```lisp
;; Have to use explicit loops:
(define sum 0)
(define i 1)
(while (<= i 10)
  (set! sum (+ sum i))
  (set! i (+ i 1)))

;; Want declarative syntax:
(loop for i from 1 to 10
      sum i)
```

### Design

#### Syntax (Simplified subset)
```lisp
;; Basic iteration
(loop for var from start to end
      do body)

;; With collection
(loop for item in collection
      do body)

;; Accumulation
(loop for i from 1 to 10
      sum i)

(loop for item in items
      collect (* item 2))

;; Conditionals
(loop for i from 1 to 100
      when (even? i)
      sum i)

;; Early exit
(loop for item in items
      until (> item 10)
      collect item)
```

#### Implementation Tasks

| Task | File | Effort | Description |
|------|------|--------|-------------|
| 1. Loop DSL parser | `lisp_evaluator.rs` | 150 lines | Parse loop clauses |
| 2. Iteration logic | `lisp_evaluator.rs` | 100 lines | for/from/to/in |
| 3. Accumulation (sum/collect) | `lisp_evaluator.rs` | 80 lines | Collect results |
| 4. Conditionals (when/unless) | `lisp_evaluator.rs` | 60 lines | Filter actions |
| 5. Early exit (while/until) | `lisp_evaluator.rs` | 50 lines | Break conditions |
| 6. Add to special forms | `lisp_evaluator.rs` | 5 lines | Register "loop" |
| 7. Write comprehensive tests | `tests/loop_tests.rs` | 200 lines | All combinations |
| **Total** | | **645 lines** | **~15,000 tokens** |

#### Examples After Implementation

```lisp
;; Sum numbers 1-100
(loop for i from 1 to 100
      sum i)  ; => 5050

;; Collect squares
(loop for x in [1 2 3 4 5]
      collect (* x x))  ; => [1 4 9 16 25]

;; Filter and transform
(loop for n from 1 to 20
      when (even? n)
      collect (* n n))  ; => [4 16 36 64 100 144 196 256 324 400]

;; Multiple accumulation
(loop for i from 1 to 10
      sum i into total
      count (even? i) into evens
      finally (return [total evens]))  ; => [55 5]

;; Early termination
(loop for x in items
      until (> x 100)
      collect x)

;; Nested loops
(loop for i from 1 to 3
      do (loop for j from 1 to 3
               do (log :message (str i "," j))))
```

#### Difficulty: ⭐⭐⭐⭐⭐ (Hard)
**Why:** Complex DSL parsing, many clause types, state management, most ambitious feature

---

## Implementation Order & Dependencies

### Recommended Order

1. **&rest parameters** (Phase 1) - Foundation for others
2. **let*** (Phase 2) - Independent, easy win
3. **flet** (Phase 3) - Prerequisite for labels
4. **case/typecase** (Phase 5) - Independent, useful immediately
5. **labels** (Phase 4) - Builds on flet
6. **loop** (Phase 6) - Most complex, save for last

### Dependency Graph

```
&rest ───┐
         ├─→ labels
flet ────┘

let* (independent)

case/typecase (independent)

loop (independent, but benefits from all above)
```

---

## Effort Summary

| Phase | Feature | Lines | Tokens | Difficulty | Time |
|-------|---------|-------|--------|------------|------|
| 1 | &rest parameters | 330 | 8,000 | ⭐⭐☆☆☆ | 1-1.5h |
| 2 | let* | 195 | 4,500 | ⭐⭐☆☆☆ | 0.5-1h |
| 3 | flet | 285 | 6,500 | ⭐⭐⭐☆☆ | 1-1.5h |
| 5 | case/typecase | 340 | 7,500 | ⭐⭐⭐☆☆ | 1-1.5h |
| 4 | labels | 345 | 8,000 | ⭐⭐⭐⭐☆ | 1.5-2h |
| 6 | loop | 645 | 15,000 | ⭐⭐⭐⭐⭐ | 2-3h |
| **TOTAL** | **All 6** | **2,140** | **49,500** | | **7-10h** |

---

## Testing Strategy

### Test Coverage Requirements

- **Unit tests:** Each feature gets dedicated test file
- **Integration tests:** Features working together
- **Edge cases:** Empty params, nil values, shadowing
- **Error cases:** Invalid syntax, type mismatches

### Test Files to Create

```
crates/ovsm/tests/
├── varargs_tests.rs      (120 lines)
├── let_star_tests.rs     (80 lines)
├── flet_tests.rs         (100 lines)
├── labels_tests.rs       (120 lines)
├── case_tests.rs         (100 lines)
└── loop_tests.rs         (200 lines)
```

**Total test code:** ~720 lines

---

## Documentation Updates

### Files to Update

1. **OVSM_LISP_SYNTAX_SPEC.md** - Add syntax for each feature (~150 lines)
2. **crates/ovsm/README.md** - Update feature list
3. **crates/ovsm/USAGE_GUIDE.md** - Add examples for each
4. **examples/ovsm_scripts/** - Create demo scripts

---

## Success Criteria

After completing all 6 phases:

✅ **Feature parity:** OVSM has 85-90% of Common Lisp core
✅ **Test coverage:** All features have comprehensive tests
✅ **Documentation:** Complete syntax guide with examples
✅ **Performance:** No significant slowdown from new features
✅ **Compatibility:** All existing tests still pass

---

## Alternative: Minimal vs. Complete

### Option A: Minimal (Quick Win)
**Implement:** &rest, let*, case/typecase
**Effort:** 865 lines, 20,000 tokens, 3-4 hours
**Result:** 70% Common Lisp coverage

### Option B: Complete (Full Power)
**Implement:** All 6 features
**Effort:** 2,140 lines, 49,500 tokens, 7-10 hours
**Result:** 85-90% Common Lisp coverage

### Recommendation

Start with **Option A** (minimal), then evaluate if full `loop` macro is needed. The loop macro alone is 30% of the effort but may not be as frequently used as the other features.

---

## Next Steps

1. **Review this plan** - Confirm priorities and scope
2. **Choose implementation order** - All 6 or subset?
3. **Start with Phase 1** - &rest parameters as foundation
4. **Iterate** - Implement, test, document, commit each phase
5. **Celebrate** - You'll have one of the most complete Lisp dialects for blockchain scripting! 🎉

---

**Estimated Completion:**
- **Phases 1-5:** ~5-7 hours (without loop)
- **All 6 Phases:** ~7-10 hours (with loop)

Ready to start? Which phase would you like to begin with?
