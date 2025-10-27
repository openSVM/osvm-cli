# OVSM Interpreter - Quick Start Guide

**Version**: 1.0.3
**Status**: Production Ready ✅ (356/356 tests passing)
**Last Updated**: October 26, 2025

---

## 🚀 5-Minute Quick Start

### Installation

```bash
# Add to your project
cd /path/to/osvm-cli
cargo build --package ovsm --release

# Verify installation
cargo test --package ovsm
# Should see: 356/356 tests passing (100%)
```

### Your First OVSM Program

```rust
use ovsm::{LispEvaluator, SExprParser, SExprScanner};

fn main() {
    let code = r#"
        (define x 10)
        (define y 20)
        (+ x y)
    "#;

    let mut scanner = SExprScanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let sexpr = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.eval(&sexpr).unwrap();

    println!("Result: {:?}", result);  // Int(30)
}
```

---

## 📚 Essential Features

### 1. Variables & Definitions

```lisp
;; Variables (mutable with set!)
(define name "Alice")
(define age 30)
(define balance 100.50)

;; Update variables
(set! balance 200.00)
```

### 2. Arithmetic & Comparisons

```lisp
;; Arithmetic (variadic - multiple arguments)
(+ 10 20)           ;; 30
(+ 10 20 30 40)     ;; 100 (variadic!)
(* 5 6)             ;; 30
(/ 100 2)           ;; 50

;; Comparisons
(>= age 18)         ;; true if adult
(= x y)             ;; Equality check
(> a b)             ;; Greater than
```

### 3. Control Flow

```lisp
;; IF-THEN-ELSE (returns a value)
(if (>= score 90)
    "A"
    "B")

;; WHILE loop
(define i 0)
(while (< i 10)
  (do
    (set! sum (+ sum i))
    (set! i (+ i 1))))

;; FOR loop
(for (item [1 2 3 4 5])
  (set! total (+ total item)))
```

### 4. Collections

```lisp
;; Arrays
(define numbers [1 2 3 4 5])
(define first (car numbers))  ;; Get first element

;; Objects (keyword syntax)
(define user {:name "Alice" :age 30})
(define user-name (get user :name))

;; Ranges (exclusive end)
(for (i (range 1 11))
  (set! sum (+ sum i)))
```

### 5. ✨ Advanced Features (83% Common Lisp)

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

;; Let bindings
(let ((x 10)
      (y 20))
  (+ x y))  ;; Returns 30

;; Pattern matching
(case value
  (1 "one")
  (2 "two")
  (otherwise "many"))
```

### 6. Logging

```lisp
;; Log messages and values
(log :message "Processing...")
(log :value result)
(log :message "Result:" :value result)
```

---

## 🎯 Common Patterns

### Pattern 1: Safe Data Access

```lisp
(define data [1 2 3])
(define index 5)

(if (< index (length data))
    (nth data index)
    null)  ;; Safe fallback
```

### Pattern 2: Input Validation

```lisp
(if (= email null)
    "Email required"
    (if (< age 18)
        "Must be 18 or older"
        (if (not terms-accepted)
            "Must accept terms"
            "Registration successful")))
```

### Pattern 3: Accumulator Pattern

```lisp
(define sum 0)
(for (num [1 2 3 4 5])
  (set! sum (+ sum num)))
sum  ;; Returns 15
```

### Pattern 4: Configuration with Defaults

```lisp
(define config {:timeout 30 :retries 3})

(define timeout (if (null? (get config :timeout))
                    60
                    (get config :timeout)))

(define retries (if (null? (get config :retries))
                    5
                    (get config :retries)))
```

---

## 📖 Complete Example

Here's a real-world example combining multiple features:

```lisp
;; User authentication and validation
(define username "alice")
(define password "secret123")
(define age 25)

;; Validation
(if (= username null)
    {:success false :error "Username required"}
    (if (= password null)
        {:success false :error "Password required"}
        (if (< age 18)
            {:success false :error "Must be 18+"}
            (do
              ;; All checks passed
              (define user {:id 123 :name username})
              (define profile {:name "Alice" :level 1})
              {:success true :user user :profile profile}))))
```

---

## 🐛 Troubleshooting

### Common Errors

**1. "Undefined variable"**
```lisp
;; ❌ Wrong
x  ;; Error if x not defined

;; ✅ Correct
(define x 10)
x
```

**2. "Division by zero"**
```lisp
;; ❌ Wrong
(/ 10 0)  ;; Error!

;; ✅ Correct
(if (= divisor 0)
    0
    (/ 10 divisor))
```

**3. "Index out of bounds"**
```lisp
;; ❌ Wrong
(define arr [1 2 3])
(nth arr 10)  ;; Error!

;; ✅ Correct
(if (< index (length arr))
    (nth arr index)
    null)
```

---

## 🧪 Testing Your Code

### Unit Test Example

```rust
#[test]
fn test_my_ovsm_code() {
    let code = r#"
        (define x 10)
        (if (> x 0)
            x
            -1)
    "#;

    let mut scanner = SExprScanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let sexpr = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    let result = evaluator.eval(&sexpr).unwrap();

    assert_eq!(result, ovsm::Value::Int(10));
}
```

---

## 📚 Further Reading

- **OVSM_LISP_SYNTAX_SPEC.md** - Complete language specification
- **FEATURES_STATUS.md** - Current feature status (83% → 100%)
- **FINAL_LISP_IMPLEMENTATION_REPORT.md** - Implementation details
- **RELEASE_NOTES.md** - Complete changelog
- **CHANGELOG.md** - Version history

### Test Files

- `tests/lisp_e2e_tests.rs` - 297 integration tests
- Unit tests embedded in source files - 59 unit tests

---

## ✅ Quick Reference Card

| Feature | Syntax | Example |
|---------|--------|---------|
| Variable | `(define name value)` | `(define x 10)` |
| Mutation | `(set! name value)` | `(set! x 50)` |
| If/Else | `(if cond then else)` | `(if (> x 0) "pos" "neg")` |
| While | `(while cond body)` | `(while (< i 10) ...)` |
| For | `(for (var seq) body)` | `(for (x [1 2 3]) ...)` |
| Do | `(do expr1 expr2 ...)` | `(do (define x 1) (+ x 2))` |
| Array | `[item1 item2 ...]` | `[1 2 3]` |
| Object | `{:key value ...}` | `{:name "Alice"}` |
| Range | `(range start end)` | `(range 1 10)` |
| Comment | `;; text` | `;; This is a comment` |
| Function | `(lambda (args) body)` | `(lambda (x) (* x 2))` |
| Macro | `(defmacro name ...)` | `(defmacro unless ...)` |
| Let | `(let ((x val)) body)` | `(let ((x 10)) (+ x 5))` |
| Case | `(case val (pat res)...)` | `(case x (1 "one") (2 "two"))` |

---

## 🎯 Best Practices

### ✅ Do

```lisp
;; Use let for local scope
(let ((x 10)
      (y 20))
  (+ x y))

;; Use do for sequential expressions
(do
  (log :message "Starting...")
  (define result (+ 10 20))
  (log :value result)
  result)

;; Use variadic operators
(+ 1 2 3 4 5)  ;; Returns 15
```

### ❌ Don't

```lisp
;; Don't nest IF statements deeply
;; ❌ Avoid this:
(if x
    (if y
        (if z
            ...)))

;; ✅ Do this instead:
(and x y z)  ;; Or use cond/case
```

---

## 🚀 Performance Tips

1. **Use variadic operators**: `(+ 1 2 3 4)` is faster than `(+ (+ (+ 1 2) 3) 4)`
2. **Avoid repeated calculations**: Store results in variables
3. **Use direct iteration**: `(for (x list) ...)` is faster than index-based
4. **Leverage macros**: They expand at parse time, not runtime

---

## 💡 Pro Tips

### Tip 1: Debugging with REPL

```bash
# Start interactive REPL
osvm ovsm repl

# Try expressions live
> (define x 10)
> (+ x 20)
30
> (for (i (range 1 6)) (log :value (* i i)))
```

### Tip 2: Macro Power

```lisp
;; Define custom control structures
(defmacro when (condition &rest body)
  `(if ,condition
       (do ,@body)
       null))

(when (> x 10)
  (log :message "Big!")
  (set! count (+ count 1)))
```

### Tip 3: Closure Patterns

```lisp
;; Counter with closure
(define make-counter
  (lambda ()
    (define count 0)
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter1 (make-counter))
(counter1)  ;; 1
(counter1)  ;; 2
```

---

## 🎉 You're Ready!

You now know enough to:
- ✅ Write OVSM LISP scripts
- ✅ Use 83% of Common Lisp features
- ✅ Handle errors gracefully
- ✅ Write macros and closures
- ✅ Debug with REPL and logs
- ✅ Test your code properly

### 📚 Next Steps

- **[BUILTIN_FUNCTIONS.md](BUILTIN_FUNCTIONS.md)** - Complete glossary of all 91+ built-in functions
- **[USAGE_GUIDE.md](USAGE_GUIDE.md)** - Comprehensive usage guide with examples
- **[docs/COMMON_PATTERNS.md](docs/COMMON_PATTERNS.md)** - Idiomatic patterns and best practices
- **[OVSM_LISP_SYNTAX_SPEC.md](../../OVSM_LISP_SYNTAX_SPEC.md)** - Complete language specification

**Happy coding!** 🚀

---

*Quick Start Guide - OVSM Interpreter v1.0.3*
*356/356 tests passing (100%) - Production Ready*
