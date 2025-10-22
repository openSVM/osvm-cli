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

Shows all arithmetic operators:

```ovsm
$a = 10
$b = 3
$result = $a + $b  // Addition
$result = $a - $b  // Subtraction
$result = $a * $b  // Multiplication
$result = $a / $b  // Division
$result = $a % $b  // Modulo
$result = $a ** 2  // Exponentiation
```

### 04_conditionals.ovsm
**Topics:** IF/THEN/ELSE statements

Conditional logic with branching:

```ovsm
IF $balance > $threshold THEN
    RETURN "Sufficient"
ELSE
    RETURN "Insufficient"
```

### 05_factorial.ovsm
**Topics:** Combining loops and conditionals

A more complex example calculating factorial:

```ovsm
$n = 5
$result = 1

IF $n < 0 THEN
    RETURN "Error"
ELSE
    FOR $i IN [1..$n+1]:
        $result = $result * $i
    RETURN $result
```

## Key OVSM Language Features

### Variables
```ovsm
$variable_name = "value"
$number = 42
```

### Data Types
- **Numbers:** Integer and floating-point
- **Strings:** Text in double quotes
- **Booleans:** `TRUE` and `FALSE`
- **Arrays:** `[item1, item2, item3]`
- **Objects:** `{"key": "value"}`
- **NULL:** Absence of value
- **Ranges:** `[start..end]` (end is exclusive)

### Control Structures
```ovsm
// IF/THEN/ELSE
IF condition THEN
    // code
ELSE
    // code

// FOR loop
FOR $item IN collection:
    // code

// WHILE loop
WHILE condition:
    // code
```

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logical: `AND`, `OR`, `NOT`

### Important Notes

#### Range Behavior
Ranges are **exclusive** of the end value:
```ovsm
[1..5]   // Creates: 1, 2, 3, 4 (NOT 5!)
[1..6]   // Creates: 1, 2, 3, 4, 5
```

#### Indentation
OVSM uses indentation for block structure (similar to Python):
```ovsm
FOR $i IN [1..5]:
    $sum = $sum + $i   // Indented = inside loop
RETURN $sum            // Not indented = after loop
```

## OVSM Test Coverage

The OVSM language interpreter has **97.3% test coverage**, ensuring production-ready reliability.

## Getting Help

```bash
# OVSM help
osvm ovsm --help

# Interactive REPL
osvm ovsm repl

# Quick evaluation
osvm ovsm eval '$x = 10; $y = 20; RETURN $x + $y'

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
