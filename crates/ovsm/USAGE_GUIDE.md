# OVSM Usage Guide

**📚 Looking for a complete function reference?** See **[BUILTIN_FUNCTIONS.md](BUILTIN_FUNCTIONS.md)** for a comprehensive glossary of all 91+ built-in functions organized by category.

---

## How to Execute OVSM Scripts

OVSM is a library crate implementing a LISP dialect for blockchain scripting. You can execute scripts in several ways:

### 1. Using the OSVM CLI (Recommended)

The easiest way to run OVSM LISP scripts:

```bash
# Run a LISP script file
osvm ovsm run script.ovsm

# Evaluate inline LISP code
osvm ovsm eval '(define x 42) (+ x 8)'

# Check syntax without running
osvm ovsm check script.ovsm

# Interactive REPL
osvm ovsm repl
```

**Example:**
```bash
cd /path/to/osvm-cli
osvm ovsm run examples/ovsm_scripts/balance_check.ovsm
```

### 2. Programmatic Usage

Use OVSM as a library in your Rust programs:

```rust
use ovsm::{LispEvaluator, SExprParser, SExprScanner, Value};

fn execute_ovsm(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    // Tokenize
    let mut scanner = SExprScanner::new(code);
    let tokens = scanner.scan_tokens()?;

    // Parse
    let mut parser = SExprParser::new(tokens);
    let sexpr = parser.parse()?;

    // Execute
    let mut evaluator = LispEvaluator::new();
    Ok(evaluator.eval(&sexpr)?)
}

fn main() {
    let code = r#"
        (define x 10)
        (if (> x 5)
            "high"
            "low")
    "#;

    match execute_ovsm(code) {
        Ok(result) => println!("Result: {:?}", result),
        Err(err) => eprintln!("Error: {}", err),
    }
}
```

### 3. Running Tests

Execute the test suite to see many more examples:

```bash
cargo test --lib --bins           # Core tests
cargo test --test lisp_e2e_tests  # LISP integration tests
cargo test -- --show-output        # Show test output
```

---

## Example Scripts

### Hello World

```lisp
;; Simple Hello World example
(define message "Hello from OVSM! 🚀")
message
```

**Output:** `String("Hello from OVSM! 🚀")`

---

### Factorial

```lisp
;; Calculate factorial of a number
(define n 5)
(define result 1)

(if (< n 0)
    "Error: Factorial undefined for negative numbers"
    (do
      (for (i (range 1 (+ n 1)))
        (set! result (* result i)))
      result))
```

**Output:** `Int(120)` (5! = 120)

---

### Conditional Logic

```lisp
;; Complex conditional logic
(define score 85)

(if (>= score 90)
    "Grade: A - Excellent!"
    (if (>= score 80)
        "Grade: B - Good job!"
        (if (>= score 70)
            "Grade: C - Average"
            (if (>= score 60)
                "Grade: D - Needs improvement"
                "Grade: F - Failed"))))
```

**Output:** `String("Grade: B - Good job!")`

---

### Array Operations

```lisp
;; Array iteration and operations
(define numbers [1 2 3 4 5])
(define sum 0)
(define count 0)

(for (num numbers)
  (set! sum (+ sum num))
  (set! count (+ count 1)))

(define average (/ sum count))
average
```

**Output:** `Int(3)` (average of 1,2,3,4,5)

---

## OVSM LISP Language Features

### Supported Features ✅

#### Control Flow
- `if` - Conditional execution (always returns a value)
- `for` - Iterate over arrays, ranges, sequences
- `while` - Loop while condition is true
- `do` - Sequential execution (returns last value)
- `define` - Define variables and functions
- `set!` - Mutate existing variables

#### Data Types
- **Integers:** `42`, `-10`
- **Floats:** `3.14`, `-0.5`
- **Strings:** `"hello"`, `"world"`
- **Booleans:** `true`, `false` (lowercase)
- **Null:** `null` (lowercase)
- **Arrays:** `[1 2 3]`, `["a" "b"]`
- **Objects:** `{:name "Alice" :age 30}` (keyword syntax)
- **Ranges:** `(range 1 10)` (exclusive end)

#### Operators
- **Arithmetic:** `+`, `-`, `*`, `/`, `%` (all variadic)
- **Comparison:** `<`, `>`, `<=`, `>=`, `=`, `!=`
- **Logical:** `and`, `or`, `not`
- **Membership:** `in` (check if item in array/string)

#### Variables
- **Definition:** `(define variable value)`
- **Mutation:** `(set! variable new-value)`
- **Scoping:** Proper lexical scope with shadowing

#### Advanced Features (83% Common Lisp coverage)
- **Macros:** `defmacro`, quasiquote, gensym
- **Closures:** First-class functions with lexical scope
- **Let bindings:** `let`, `let*`, `flet`, `labels`
- **Pattern matching:** `case`, `typecase`
- **Multiple values:** `values`, `multiple-value-bind`
- **Dynamic variables:** `defvar`
- **Variadic functions:** `&rest` parameter

---

## Important Syntax Notes

### S-Expression Structure

OVSM uses S-expressions (symbolic expressions) with explicit parentheses:

#### ✅ DO: Use Explicit Parentheses

```lisp
;; GOOD - Clear block boundaries
(for (i (range 1 11))
  (if (> i 5)
      (do
        (log :message "big")
        i)
      (log :message "small")))
```

#### ✅ DO: Use `do` for Sequential Execution

```lisp
;; GOOD - Multiple expressions in sequence
(define sum 0)
(for (i (range 1 11))
  (do
    (set! sum (+ sum i))
    (log :value sum)))  ;; Both expressions execute
```

#### ✅ DO: Return Final Values

```lisp
;; GOOD - Last expression is the return value
(do
  (define sum 0)
  (for (i (range 1 11))
    (set! sum (+ sum i)))
  sum)  ;; Returns the final sum
```

---

## Testing Your Scripts

### Quick Test

```bash
osvm ovsm run your_script.ovsm
```

### With Debugging

Use `log` for debugging output:

```lisp
(define x 10)
(define y 20)
(log :message "X value:" :value x)
(define sum (+ x y))
(log :message "Sum:" :value sum)
sum
```

### Running Unit Tests

```bash
# All tests
cargo test

# LISP-specific tests
cargo test --test lisp_e2e_tests

# Show output
cargo test -- --show-output --nocapture
```

---

## Common Patterns

### Accumulator Pattern

```lisp
(define sum 0)
(for (i (range 1 11))
  (set! sum (+ sum i)))
sum
```

### Find Pattern

```lisp
(define found false)
(for (item array)
  (if (= item target)
      (set! found true)
      null))
found
```

### Filter Pattern

```lisp
(define evens [])
(for (num numbers)
  (if (= (% num 2) 0)
      (set! evens (append evens [num]))
      null))
evens
```

### Nested Loops

```lisp
(define result 0)
(for (i (range 1 6))
  (for (j (range 1 6))
    (set! result (+ result (* i j)))))
result
```

---

## Troubleshooting

### Common Runtime Errors

#### 1. "Undefined variable: name"

**Cause:** Using a variable before it's been defined.

**Example error:**
```lisp
;; BAD
(for (i (range 1 6))
  (set! sum (+ sum i)))  ;; sum not defined!
```

**Solution:** Define variables before use.
```lisp
;; GOOD
(define sum 0)  ;; Initialize first
(for (i (range 1 6))
  (set! sum (+ sum i)))
```

---

#### 2. "Division by zero"

**Cause:** Dividing or taking modulo by zero.

**Example error:**
```lisp
(/ 10 0)  ;; ERROR: Division by zero
(% 5 0)   ;; ERROR: Division by zero
```

**Solution:** Check denominators before division.
```lisp
;; GOOD: Conditional check
(if (= denominator 0)
    "Error: division by zero"
    (/ numerator denominator))
```

---

#### 3. "Index out of bounds"

**Cause:** Accessing array index beyond array size.

**Example error:**
```lisp
(define arr [1 2 3])
(nth arr 5)  ;; ERROR: Index 5 out of bounds (length 3)
```

**Solution:** Check array length before indexing.
```lisp
;; GOOD: Check length
(define arr [1 2 3])
(define index 5)

(if (< index (length arr))
    (nth arr index)
    null)  ;; Default value
```

---

#### 4. "Type error: expected {expected}, got {got}"

**Cause:** Operation expecting one type but receiving another.

**Example errors:**
```lisp
(+ "hello" 5)  ;; ERROR: Cannot add string and number
```

**Solution:** Ensure type compatibility.
```lisp
;; GOOD: Consistent types
(+ "hello" " world")  ;; String concatenation
(+ 5 10)              ;; Number addition

;; GOOD: Explicit type checks
(define text "hello")
(if (and (!= text null) (!= text ""))
    "valid"
    "invalid")
```

---

### Common Parse Errors

#### 1. "Syntax error: Expected closing parenthesis"

**Cause:** Missing closing parenthesis.

**Example error:**
```lisp
;; BAD
(define x (+ 10 20
```

**Solution:** Always balance parentheses.
```lisp
;; GOOD
(define x (+ 10 20))
```

---

#### 2. "Unexpected token"

**Cause:** Syntax error in expression.

**Example errors:**
```lisp
(define x + 5)         ;; Missing left operand
(define y [1 2 3)      ;; Missing closing bracket
```

**Solution:** Check expression syntax.
```lisp
;; GOOD
(define x (+ 10 5))
(define y [1 2 3])
```

---

### Performance Issues

#### Slow Execution

**Cause:** Inefficient patterns or unnecessary calculations.

**Bad pattern:**
```lisp
;; Calculates length every iteration
(for (i (range 0 (length array)))
  (log :value (nth array i)))
```

**Good pattern:**
```lisp
;; Calculate once
(define len (length array))
(for (i (range 0 len))
  (log :value (nth array i)))

;; Or iterate directly
(for (item array)
  (log :value item))
```

---

### Debugging Tips

#### 1. Use `log` for debugging

```lisp
(define x 10)
(log :message "Value of x:" :value x)

(for (i (range 1 6))
  (log :message "Iteration:" :value i)
  (set! sum (+ sum i))
  (log :message "Current sum:" :value sum))

sum
```

#### 2. Test with simple cases first

```lisp
;; Test with small array first
(define test-array [1 2 3])
;; ... test logic ...

;; Then scale to larger arrays
(define real-array [1 2 3 4 5 6 7 8 9 10])
```

---

## Quick Reference

| Feature | Syntax | Example |
|---------|--------|---------|
| Variable | `(define name value)` | `(define x 42)` |
| Mutation | `(set! name value)` | `(set! x 50)` |
| If/Else | `(if cond then else)` | `(if (> x 5) "big" "small")` |
| For Loop | `(for (var seq) body)` | `(for (i (range 1 11)) ...)` |
| While Loop | `(while cond body)` | `(while (< x 10) ...)` |
| Sequential | `(do expr1 expr2 ...)` | `(do (define x 1) (+ x 2))` |
| Array | `[item1 item2 ...]` | `[1 2 3]` |
| Object | `{:key value ...}` | `{:name "Alice"}` |
| Range | `(range start end)` | `(range 1 10)` |
| Comment | `;; text` | `;; This is a comment` |
| Function | `(lambda (args) body)` | `(lambda (x) (* x 2))` |
| Macro | `(defmacro name ...)` | `(defmacro unless ...)` |

---

## Next Steps

1. **Function Reference:** See **[BUILTIN_FUNCTIONS.md](BUILTIN_FUNCTIONS.md)** for complete glossary of all 91+ built-in functions
2. **Explore Examples:** Run all scripts in `examples/ovsm_scripts/` directory
3. **Read Tests:** Check `tests/lisp_e2e_tests.rs` for comprehensive examples
4. **Check Docs:** See `OVSM_LISP_SYNTAX_SPEC.md` for complete language specification
5. **Common Patterns:** See `docs/COMMON_PATTERNS.md` for idiomatic patterns
6. **Check Status:** See `FEATURES_STATUS.md` for current 83% → 100% roadmap

---

**Happy coding with OVSM LISP! 🚀**
