# OVSM Example Scripts

This directory contains example OVSM (Open Solana Virtual Machine) scripts demonstrating LISP language features.

**OVSM v1.0.2** - 90% Common Lisp feature coverage - Production ready!

## Running the Examples

```bash
# Run a specific example
osvm ovsm run examples/ovsm_scripts/balance_check.ovsm

# Check syntax without running
osvm ovsm check examples/ovsm_scripts/loop_example.ovsm

# Evaluate inline LISP code
osvm ovsm eval '(+ 1 2 3 4 5)'

# Start interactive REPL
osvm ovsm repl

# Run all examples
for script in examples/ovsm_scripts/*.ovsm; do
    echo "=== Running $script ==="
    osvm ovsm run "$script"
    echo
done
```

## Language Features

OVSM uses S-expression (LISP) syntax with explicit parentheses:

### Variables and Arithmetic
```lisp
;; Define variables
(define balance 1000)
(define fee 0.02)

;; Calculate
(define cost (* balance fee))
(log :message "Cost:" :value cost)
```

### Control Flow
```lisp
;; Conditionals
(if (> balance 500)
    (log :message "High balance!")
    (log :message "Low balance"))

;; Loops
(define sum 0)
(for (i [1 2 3 4 5])
  (set! sum (+ sum i)))
```

### Advanced Features
```lisp
;; Macros
(defmacro unless (condition &rest body)
  `(if (not ,condition)
       (do ,@body)))

;; Closures
(define (make-counter)
  (define count 0)
  (lambda ()
    (set! count (+ count 1))
    count))

;; Pattern Matching
(case value
  (1 "one")
  (2 "two")
  (otherwise "many"))
```

## Key OVSM Language Features

### Arithmetic Operations
```lisp
;; All arithmetic operators are variadic
(+ 10 20)           ;; Addition → 30
(- 100 30)          ;; Subtraction → 70
(* 5 6)             ;; Multiplication → 30
(/ 100 2)           ;; Division → 50
(% 17 5)            ;; Modulo → 2
```

### Conditionals
```lisp
;; IF-THEN-ELSE (returns a value)
(if (> balance threshold)
    "Sufficient"
    "Insufficient")
```

### Combining Loops and Conditionals
```lisp
;; Factorial calculation
(define n 5)
(define result 1)

(if (< n 0)
    "Error"
    (do
      (for (i (range 1 (+ n 1)))
        (set! result (* result i)))
      result))
```

### Variables
```lisp
;; Define immutable variables
(define variable-name "value")
(define number 42)
```

### Data Types
- **Numbers:** Integer and floating-point
- **Strings:** Text in double quotes
- **Booleans:** `true` and `false` (lowercase)
- **Arrays:** `[item1 item2 item3]` or using square brackets
- **Objects:** `{:key "value"}` with keyword syntax
- **Null:** `null` (lowercase)
- **Ranges:** `(range start end)` function (end is exclusive)

### Control Structures
```lisp
;; IF-THEN-ELSE
(if condition
    then-expr
    else-expr)

;; FOR loop
(for (item collection)
  expr1
  expr2)

;; WHILE loop
(while condition
  expr1
  expr2)
```

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%` (variadic functions)
- Comparison: `<`, `>`, `<=`, `>=`, `=`, `!=`
- Logical: `and`, `or`, `not`

### Important Notes

#### Range Behavior
Ranges are **exclusive** of the end value:
```lisp
(range 1 5)   ;; Creates: [1, 2, 3, 4] (NOT 5!)
(range 1 6)   ;; Creates: [1, 2, 3, 4, 5]
```

#### S-Expression Structure
OVSM uses explicit parentheses for block structure (LISP syntax):
```lisp
;; Clear block boundaries with parentheses
(for (i (range 1 6))
  (set! sum (+ sum i)))

;; Sequential execution with do
(do
  (define sum 0)
  (for (i (range 1 6))
    (set! sum (+ sum i)))
  sum)  ;; Returns final value
```

## OVSM Test Coverage

The OVSM language interpreter has **97.3% test coverage**, ensuring production-ready reliability.

## Getting Help

```bash
# OVSM help
osvm ovsm --help

# Interactive REPL
osvm ovsm repl

# Quick evaluation (LISP syntax)
osvm ovsm eval '(define x 10) (define y 20) (+ x y)'

# View examples catalog
osvm ovsm examples
```

## Future Examples (Phase 2+)

Coming in the next phase:
- Blockchain operations (validator deployment, RPC queries)
- MCP server integration
- AI-powered script generation
- Multi-node orchestration

## Learning Path

1. **Start with:** `01_hello_world.ovsm` - Basic syntax
2. **Then try:** `02_control_flow.ovsm` - Loops
3. **Next:** `03_arithmetic.ovsm` - Math operations
4. **Learn:** `04_conditionals.ovsm` - Branching logic
5. **Apply:** `05_factorial.ovsm` - Combining features

## Resources

- [OVSM Language Documentation](../../crates/ovsm/README.md)
- [OVSM Usage Guide](../../crates/ovsm/USAGE_GUIDE.md)
- [OVSM How-To Guide](../../crates/ovsm/HOW_TO_USE.md)
- [OSVM-CLI Documentation](../../README.md)

---

**Happy coding with OVSM! 🚀**
