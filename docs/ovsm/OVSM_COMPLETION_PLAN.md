# OVSM Completion Plan: 83% → 100% Common Lisp Coverage

**Current Status:** 83% Common Lisp coverage
**Target:** 100% feature-complete Common Lisp dialect
**Gap:** 17% (10 major feature groups)
**Estimated Total Effort:** ~3,500 lines, 80,000 tokens, 20-25 hours

---

## Priority Matrix

| Feature | Coverage Gain | Difficulty | Effort | Priority | Order |
|---------|---------------|------------|--------|----------|-------|
| loop macro | +7% | ⭐⭐⭐⭐⭐ | 645 lines | HIGH | 1 |
| &optional/&key | +3% | ⭐⭐⭐☆☆ | 350 lines | HIGH | 2 |
| destructuring-bind | +2% | ⭐⭐⭐⭐☆ | 400 lines | MEDIUM | 3 |
| catch/throw | +2% | ⭐⭐⭐☆☆ | 280 lines | MEDIUM | 4 |
| setf | +1% | ⭐⭐⭐⭐☆ | 320 lines | MEDIUM | 5 |
| format | +1% | ⭐⭐⭐☆☆ | 250 lines | LOW | 6 |
| progn/prog1/prog2 | +0.5% | ⭐⭐☆☆☆ | 120 lines | LOW | 7 |
| eval | +0.5% | ⭐⭐☆☆☆ | 80 lines | LOW | 8 |
| read/print | +1% | ⭐⭐⭐☆☆ | 200 lines | LOW | 9 |
| CLOS basics | +3% | ⭐⭐⭐⭐⭐ | 800 lines | OPTIONAL | 10 |

---

## Phase 1: loop Macro (Priority: CRITICAL)

### Goal
Implement Common Lisp's declarative loop facility with main iteration patterns.

### Coverage Impact
**+7%** (83% → 90%)

### Design

#### Core Loop Forms
```lisp
;; Numeric iteration
(loop for i from 1 to 10 do (print i))
(loop for i from 1 below 10 by 2 do (print i))
(loop for i downfrom 10 to 1 do (print i))

;; Collection iteration
(loop for item in list do (print item))
(loop for item in list by #'cddr do (print item))

;; Accumulation
(loop for i from 1 to 10 sum i)
(loop for i from 1 to 10 collect (* i i))
(loop for i from 1 to 10 count (even? i))
(loop for x in items append (process x))

;; Conditionals
(loop for i from 1 to 100
      when (even? i) sum i)

(loop for i from 1 to 100
      unless (< i 10) collect i)

;; Early exit
(loop for i from 1 to 1000
      while (< i 100)
      sum i)

(loop for i from 1 to 1000
      until (> i 100)
      sum i)

;; Multiple clauses
(loop for i from 1 to 10
      for j from 10 to 1
      sum (* i j))

;; Named loops (for return)
(loop named outer
      for i from 1 to 10 do
        (loop for j from 1 to 10 do
          (when (= (* i j) 50)
            (return-from outer (list i j)))))
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Loop clause parser | `lisp_evaluator.rs` | 150 | Parse for/from/to/in/by/do/sum/collect |
| 2. Numeric iteration | `lisp_evaluator.rs` | 80 | from/to/below/above/downfrom |
| 3. Collection iteration | `lisp_evaluator.rs` | 70 | in/on lists |
| 4. Accumulation | `lisp_evaluator.rs` | 100 | sum/collect/count/append/maximize/minimize |
| 5. Conditionals | `lisp_evaluator.rs` | 60 | when/unless/if |
| 6. Early exit | `lisp_evaluator.rs` | 50 | while/until/always/never/thereis |
| 7. Return values | `lisp_evaluator.rs` | 40 | finally clause, default return |
| 8. Named loops | `lisp_evaluator.rs` | 45 | loop names, return-from |
| 9. Parser integration | `sexpr_parser.rs` | 50 | parse_loop_expr() |
| 10. Comprehensive tests | `tests/loop_tests.rs` | 200 | All combinations |
| **TOTAL** | | **845 lines** | **~19,000 tokens** |

### Difficulty: ⭐⭐⭐⭐⭐ (Very Hard)
- Complex DSL with many keywords
- State management for accumulation
- Multiple iteration variables
- Nested loop support
- Return value handling

### Estimated Time: 4-6 hours

---

## Phase 2: &optional and &key Parameters

### Goal
Support optional and keyword parameters for more flexible function signatures.

### Coverage Impact
**+3%** (90% → 93%)

### Design

```lisp
;; Optional parameters with defaults
(defun greet (name &optional (greeting "Hello") (punctuation "!"))
  (str greeting " " name punctuation))

(greet "Alice")                    ; => "Hello Alice!"
(greet "Bob" "Hi")                 ; => "Hi Bob!"
(greet "Charlie" "Hey" "?")        ; => "Hey Charlie?"

;; Keyword parameters
(defun make-person (&key name age (city "Unknown"))
  {:name name :age age :city city})

(make-person :name "Alice" :age 30)
; => {:name "Alice" :age 30 :city "Unknown"}

(make-person :age 25 :name "Bob" :city "NYC")
; => {:name "Bob" :age 25 :city "NYC"}

;; Mixed parameters
(defun complex (required &optional opt1 opt2 &rest rest &key key1 key2)
  ...)
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Parser updates | `lisp_evaluator.rs` | 100 | Parse &optional, &key markers |
| 2. Default value handling | `lisp_evaluator.rs` | 80 | Evaluate defaults when not provided |
| 3. Keyword extraction | `lisp_evaluator.rs` | 70 | Extract :key value pairs from args |
| 4. Validation | `lisp_evaluator.rs` | 50 | Check required keys, unknown keys |
| 5. Integration with &rest | `lisp_evaluator.rs` | 30 | Handle all parameter types together |
| 6. Tests | `tests/optional_key_tests.rs` | 120 | All combinations |
| **TOTAL** | | **450 lines** | **~10,000 tokens** |

### Difficulty: ⭐⭐⭐☆☆ (Medium)

### Estimated Time: 2-3 hours

---

## Phase 3: destructuring-bind

### Goal
Pattern matching for destructuring complex data structures.

### Coverage Impact
**+2%** (93% → 95%)

### Design

```lisp
;; List destructuring
(destructuring-bind (a b c) [1 2 3]
  (+ a b c))  ; => 6

;; Nested destructuring
(destructuring-bind (a (b c) d) [1 [2 3] 4]
  (list a b c d))  ; => [1 2 3 4]

;; With &rest
(destructuring-bind (first second &rest rest) [1 2 3 4 5]
  rest)  ; => [3 4 5]

;; With &optional
(destructuring-bind (a &optional (b 10) (c 20)) [1]
  (list a b c))  ; => [1 10 20]

;; With &key
(destructuring-bind (&key name age) {:name "Alice" :age 30}
  (str name " is " age))

;; Tree patterns
(destructuring-bind ((a b) (c d)) [[1 2] [3 4]]
  (+ a b c d))  ; => 10
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Pattern parser | `lisp_evaluator.rs` | 120 | Parse destructuring patterns |
| 2. Matcher | `lisp_evaluator.rs` | 100 | Match pattern against value |
| 3. Nested patterns | `lisp_evaluator.rs` | 80 | Recursive matching |
| 4. &rest/&optional/&key | `lisp_evaluator.rs` | 60 | Handle special markers |
| 5. Error messages | `lisp_evaluator.rs` | 40 | Clear mismatch errors |
| 6. Parser integration | `sexpr_parser.rs` | 40 | parse_destructuring_bind() |
| 7. Tests | `tests/destructuring_tests.rs` | 160 | All patterns |
| **TOTAL** | | **600 lines** | **~13,000 tokens** |

### Difficulty: ⭐⭐⭐⭐☆ (Medium-Hard)

### Estimated Time: 3-4 hours

---

## Phase 4: catch/throw (Non-local Exits)

### Goal
Dynamic exit from nested computations.

### Coverage Impact
**+2%** (95% → 97%)

### Design

```lisp
;; Basic catch/throw
(catch 'done
  (loop for i from 1 to 100 do
    (when (> i 10)
      (throw 'done i))))  ; => 11

;; Nested catch
(catch 'outer
  (catch 'inner
    (throw 'outer 42)))  ; => 42

;; Multiple throws
(defun search-nested (tree target)
  (catch 'found
    (traverse tree
      (lambda (node)
        (when (= node target)
          (throw 'found node))))
    nil))

;; With cleanup
(catch 'exit
  (unwind-protect
    (throw 'exit 42)
    (cleanup-resources)))
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Catch tag registry | `lisp_evaluator.rs` | 60 | Track active catch tags |
| 2. Throw mechanism | `lisp_evaluator.rs` | 50 | Unwind stack to catch |
| 3. Error type | `error.rs` | 30 | ThrowValue error variant |
| 4. Stack unwinding | `lisp_evaluator.rs` | 70 | Propagate throw up |
| 5. Nested catch | `lisp_evaluator.rs` | 40 | Multiple catch levels |
| 6. Parser integration | `sexpr_parser.rs` | 30 | parse catch/throw |
| 7. Tests | `tests/catch_throw_tests.rs` | 120 | All scenarios |
| **TOTAL** | | **400 lines** | **~9,000 tokens** |

### Difficulty: ⭐⭐⭐☆☆ (Medium)

### Estimated Time: 2-3 hours

---

## Phase 5: setf (Generalized Assignment)

### Goal
Unified assignment operator for any place.

### Coverage Impact
**+1%** (97% → 98%)

### Design

```lisp
;; Array element
(setf (nth array 0) 42)
(setf (elt array 5) 100)

;; Object property
(setf (get obj :name) "Alice")

;; Multiple assignments
(setf x 10
      y 20
      z 30)

;; Computed places
(setf (car (cdr list)) 42)

;; Custom setf expanders
(defsetf get (obj key) (value)
  `(set-property ,obj ,key ,value))
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Place parser | `lisp_evaluator.rs` | 80 | Parse assignable places |
| 2. Place types | `ast.rs` | 40 | Define place expressions |
| 3. Assignment logic | `lisp_evaluator.rs` | 90 | Handle each place type |
| 4. Multiple setf | `lisp_evaluator.rs` | 40 | (setf a 1 b 2 c 3) |
| 5. Computed places | `lisp_evaluator.rs` | 60 | Nested access |
| 6. Parser integration | `sexpr_parser.rs` | 30 | parse_setf() |
| 7. Tests | `tests/setf_tests.rs` | 120 | All place types |
| **TOTAL** | | **460 lines** | **~10,000 tokens** |

### Difficulty: ⭐⭐⭐⭐☆ (Medium-Hard)

### Estimated Time: 2.5-3.5 hours

---

## Phase 6: format (String Formatting)

### Goal
Common Lisp's powerful format function.

### Coverage Impact
**+1%** (98% → 99%)

### Design

```lisp
;; Basic formatting
(format nil "Hello, ~A!" "World")  ; => "Hello, World!"

;; Directives
(format nil "Number: ~D" 42)        ; => "Number: 42"
(format nil "Float: ~F" 3.14159)    ; => "Float: 3.14159"
(format nil "Hex: ~X" 255)          ; => "Hex: FF"

;; Iteration
(format nil "~{~A ~}" [1 2 3])      ; => "1 2 3 "

;; Conditionals
(format nil "~:[false~;true~]" condition)

;; Pluralization
(format nil "~D item~:P" count)     ; "1 item" or "2 items"

;; Pretty printing
(format t "~%Line 1~%Line 2~%")
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Format parser | `lisp_evaluator.rs` | 100 | Parse ~directives |
| 2. Basic directives | `lisp_evaluator.rs` | 80 | ~A, ~D, ~F, ~S |
| 3. Iteration ~{ } | `lisp_evaluator.rs` | 60 | Format lists |
| 4. Conditionals ~[ ] | `lisp_evaluator.rs` | 50 | Conditional output |
| 5. Advanced directives | `lisp_evaluator.rs` | 70 | ~X, ~O, ~B, ~:P |
| 6. Output streams | `lisp_evaluator.rs` | 40 | nil vs t vs stream |
| 7. Tests | `tests/format_tests.rs` | 120 | All directives |
| **TOTAL** | | **520 lines** | **~11,000 tokens** |

### Difficulty: ⭐⭐⭐☆☆ (Medium)

### Estimated Time: 2.5-3 hours

---

## Phase 7: progn/prog1/prog2 (Sequence Forms)

### Goal
Additional sequential execution forms.

### Coverage Impact
**+0.5%** (99% → 99.5%)

### Design

```lisp
;; progn - like do, returns last value
(progn
  (print "Step 1")
  (print "Step 2")
  42)  ; => 42

;; prog1 - returns FIRST value
(prog1
  (print "This value returned")
  (print "This executed but not returned"))

;; prog2 - returns SECOND value
(prog2
  (print "Ignored")
  (print "This value returned")
  (print "Also ignored"))
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. eval_progn | `lisp_evaluator.rs` | 30 | Return last value |
| 2. eval_prog1 | `lisp_evaluator.rs` | 30 | Return first value |
| 3. eval_prog2 | `lisp_evaluator.rs` | 30 | Return second value |
| 4. Parser | `sexpr_parser.rs` | 15 | All three forms |
| 5. Tests | `tests/prog_tests.rs` | 60 | All forms |
| **TOTAL** | | **165 lines** | **~3,500 tokens** |

### Difficulty: ⭐⭐☆☆☆ (Easy)

### Estimated Time: 0.5-1 hour

---

## Phase 8: eval (Runtime Evaluation)

### Goal
Evaluate code at runtime.

### Coverage Impact
**+0.5%** (99.5% → 100%)

### Design

```lisp
;; Basic eval
(eval '(+ 1 2 3))  ; => 6

;; With variables
(define x 10)
(eval '(+ x 5))  ; => 15

;; Generate and eval code
(defun make-adder (n)
  (eval `(lambda (x) (+ x ,n))))

(define add5 (make-adder 5))
(add5 10)  ; => 15

;; Eval in environment
(eval '(+ a b) {:a 10 :b 20})  ; => 30
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. eval function | `lisp_evaluator.rs` | 50 | Public eval interface |
| 2. Quote handling | `lisp_evaluator.rs` | 30 | Eval quoted expressions |
| 3. Environment passing | `lisp_evaluator.rs` | 40 | Custom environments |
| 4. Security checks | `lisp_evaluator.rs` | 30 | Limit eval depth |
| 5. Tests | `tests/eval_tests.rs` | 80 | All scenarios |
| **TOTAL** | | **230 lines** | **~5,000 tokens** |

### Difficulty: ⭐⭐☆☆☆ (Easy-Medium)

### Estimated Time: 1-1.5 hours

---

## Phase 9: read/print (S-expression I/O)

### Goal
Read and print S-expressions.

### Coverage Impact
**Included in 100%**

### Design

```lisp
;; Read from string
(read "(+ 1 2 3)")  ; => Expression

;; Print to string
(print '(+ 1 2 3))  ; => "(+ 1 2 3)"

;; prin1 (readable)
(prin1 "hello")  ; => "\"hello\""

;; princ (human-readable)
(princ "hello")  ; => "hello"

;; Read-eval-print loop
(loop
  (print (eval (read))))
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. read function | `lisp_evaluator.rs` | 60 | Parse string to AST |
| 2. print function | `lisp_evaluator.rs` | 50 | AST to string |
| 3. prin1/princ | `lisp_evaluator.rs` | 40 | Different print modes |
| 4. Pretty printer | `lisp_evaluator.rs` | 50 | Indented output |
| 5. Tests | `tests/read_print_tests.rs` | 100 | Round-trip tests |
| **TOTAL** | | **300 lines** | **~6,500 tokens** |

### Difficulty: ⭐⭐⭐☆☆ (Medium)

### Estimated Time: 1.5-2 hours

---

## Phase 10: CLOS Basics (OPTIONAL)

### Goal
Basic object-oriented programming with Common Lisp Object System.

### Coverage Impact
**+3%** (if implemented separately, but not required for 100%)

### Design

```lisp
;; Define class
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)))

;; Create instance
(define alice (make-instance 'person :name "Alice" :age 30))

;; Access
(person-name alice)  ; => "Alice"
(setf (person-age alice) 31)

;; Generic functions
(defgeneric greet (person))

(defmethod greet ((p person))
  (str "Hello, " (person-name p)))

;; Multiple dispatch
(defmethod combine ((x integer) (y integer))
  (+ x y))

(defmethod combine ((x string) (y string))
  (str x y))

;; Inheritance
(defclass employee (person)
  ((company :initarg :company :accessor employee-company)))
```

### Implementation Tasks

| Task | File | Lines | Description |
|------|------|-------|-------------|
| 1. Class definition | `lisp_evaluator.rs` | 150 | defclass parser |
| 2. Slots | `lisp_evaluator.rs` | 100 | Slot definitions |
| 3. Accessors | `lisp_evaluator.rs` | 80 | Auto-generate accessors |
| 4. make-instance | `lisp_evaluator.rs` | 70 | Object creation |
| 5. Generic functions | `lisp_evaluator.rs` | 120 | defgeneric/defmethod |
| 6. Method dispatch | `lisp_evaluator.rs` | 150 | Find applicable methods |
| 7. Inheritance | `lisp_evaluator.rs` | 100 | Superclass handling |
| 8. Tests | `tests/clos_tests.rs` | 180 | All features |
| **TOTAL** | | **950 lines** | **~21,000 tokens** |

### Difficulty: ⭐⭐⭐⭐⭐ (Very Hard)

### Estimated Time: 5-7 hours

**Note:** CLOS is optional - OVSM can be considered "complete" at 100% without it, as it's a separate paradigm.

---

## Implementation Order & Timeline

### Sprint 1: Core Iteration (Week 1)
**Goal:** Reach 90% coverage
- ✅ Phase 1: loop macro (4-6 hours)
- **Checkpoint:** 90% coverage, declarative iteration

### Sprint 2: Advanced Parameters (Week 2)
**Goal:** Reach 93% coverage
- ✅ Phase 2: &optional/&key (2-3 hours)
- **Checkpoint:** 93% coverage, flexible functions

### Sprint 3: Pattern Matching & Control (Week 3)
**Goal:** Reach 97% coverage
- ✅ Phase 3: destructuring-bind (3-4 hours)
- ✅ Phase 4: catch/throw (2-3 hours)
- **Checkpoint:** 97% coverage, powerful abstractions

### Sprint 4: Final Features (Week 4)
**Goal:** Reach 100% coverage
- ✅ Phase 5: setf (2.5-3.5 hours)
- ✅ Phase 6: format (2.5-3 hours)
- ✅ Phase 7: progn/prog1/prog2 (0.5-1 hour)
- ✅ Phase 8: eval (1-1.5 hours)
- ✅ Phase 9: read/print (1.5-2 hours)
- **Checkpoint:** 100% coverage, complete implementation

### Sprint 5: CLOS (Optional, Week 5-6)
**Goal:** Add OOP paradigm
- ⏳ Phase 10: CLOS basics (5-7 hours)
- **Checkpoint:** Full Common Lisp dialect

---

## Total Effort Summary

### Without CLOS (100% Core Coverage)
- **Total Lines:** ~3,970 lines
- **Total Tokens:** ~87,000 tokens
- **Total Time:** 20-27 hours
- **Sprints:** 4 weeks

### With CLOS (100% + OOP)
- **Total Lines:** ~4,920 lines
- **Total Tokens:** ~108,000 tokens
- **Total Time:** 25-34 hours
- **Sprints:** 5-6 weeks

---

## Risk Assessment

### High Risk (Complex Implementation)
1. **loop macro** - Complex DSL, many edge cases
2. **destructuring-bind** - Recursive pattern matching
3. **CLOS** - Entire object system
4. **setf** - Multiple place types

### Medium Risk (Moderate Complexity)
1. **&optional/&key** - Parameter parsing
2. **catch/throw** - Stack unwinding
3. **format** - Many directives

### Low Risk (Straightforward)
1. **progn/prog1/prog2** - Simple variants
2. **eval** - Already have evaluator
3. **read/print** - Wrapper around existing parser

---

## Testing Strategy

### Unit Tests
- Each feature: 80-200 lines of tests
- Edge cases and error conditions
- Integration with existing features

### Integration Tests
- Cross-feature interactions
- Real-world usage patterns
- Performance benchmarks

### Regression Tests
- Ensure existing 83% still works
- No breaking changes
- Backward compatibility

---

## Success Criteria

### Phase Completion
- [ ] All tests passing (>95%)
- [ ] Documentation updated
- [ ] Examples provided
- [ ] Performance acceptable
- [ ] Clean git history

### Final Completion (100%)
- [ ] All 9 core phases complete
- [ ] Comprehensive test suite (>1000 tests)
- [ ] Updated language specification
- [ ] Migration guide for new features
- [ ] Performance benchmarks
- [ ] Production-ready quality

---

## Conclusion

This plan will take OVSM from **83% → 100%** Common Lisp coverage through 9 phases of implementation. The features are prioritized by impact and difficulty, with clear milestones at 90%, 93%, 97%, and 100% coverage.

**Current recommendation:** Start with Phase 1 (loop macro) as it provides the largest coverage gain (+7%) and is the most frequently requested feature in Lisp programming.

**Estimated total time to 100%:** 20-27 hours of focused implementation across 4 weeks.

**With CLOS:** Additional 5-7 hours (optional, for OOP paradigm).

---

*Generated with Claude Code - Completion Planning Session*
