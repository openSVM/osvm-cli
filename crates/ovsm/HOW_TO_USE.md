# How to Use OVSM

This guide shows you all the ways to execute OVSM scripts and programs.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Method 1: File Execution](#method-1-file-execution-recommended)
3. [Method 2: Interactive REPL](#method-2-interactive-repl)
4. [Method 3: Programmatic Usage](#method-3-programmatic-usage)
5. [Method 4: Unit Tests](#method-4-unit-tests)
6. [Common Issues](#common-issues)

---

## Quick Start

**Fastest way to run an OVSM script:**

```bash
cd crates/ovsm
cargo run --example run_file examples/hello_world.ovsm
```

Expected output:
```
🚀 Executing: examples/hello_world.ovsm
============================================================
✅ Result: String("Hello from OVSM! 🚀")
```

---

## Method 1: File Execution (Recommended)

### Step 1: Create a Script File

Create a file named `my_script.ovsm`:

```ovsm
// my_script.ovsm
$sum = 0

FOR $i IN [1..11]:
    $sum = $sum + $i

RETURN $sum
```

### Step 2: Execute It

```bash
cargo run --example run_file my_script.ovsm
```

### Available Example Scripts

```bash
# Simple hello world
cargo run --example run_file examples/hello_world.ovsm

# Calculate factorial
cargo run --example run_file examples/factorial.ovsm

# Fibonacci sequence
cargo run --example run_file examples/fibonacci.ovsm

# Array operations
cargo run --example run_file examples/array_operations.ovsm

# Conditional logic
cargo run --example run_file examples/conditional_logic.ovsm

# Loop control (BREAK/CONTINUE)
cargo run --example run_file examples/loop_control.ovsm
```

---

## Method 2: Interactive REPL

Launch the interactive Read-Eval-Print Loop:

```bash
cargo run --example simple_repl
```

### Example Session

```
╔══════════════════════════════════════════╗
║   OVSM Interactive REPL v1.0.0           ║
╚══════════════════════════════════════════╝

Type OVSM expressions and press Enter.
Type 'exit' or press Ctrl+C to quit.
Type 'help' for examples.

ovsm[1]> RETURN 2 + 3 * 4
  ⇒ Int(14)

ovsm[2]> RETURN 10 > 5
  ⇒ Bool(true)

ovsm[3]> IF 10 > 5 THEN RETURN "yes" ELSE RETURN "no"
  ⇒ String("yes")

ovsm[4]> exit
Goodbye! 👋
```

### REPL Commands

- `help` - Show examples
- `clear` - Clear environment
- `exit` or `quit` - Exit REPL

---

## Method 3: Programmatic Usage

Use OVSM as a library in your Rust programs.

### Create a Binary Project

```bash
cargo new my_ovsm_app
cd my_ovsm_app
```

### Add Dependency

Edit `Cargo.toml`:

```toml
[dependencies]
ovsm = { path = "../osvm-cli/crates/ovsm" }
```

### Write Your Code

Edit `src/main.rs`:

```rust
use ovsm::{Evaluator, Parser, Scanner, Value};

fn execute_ovsm(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    // Tokenize (Scanner)
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;

    // Parse (Parser)
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;

    // Execute (Evaluator)
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

fn main() {
    // Example 1: Simple arithmetic
    let code1 = "RETURN 2 + 3 * 4";
    match execute_ovsm(code1) {
        Ok(result) => println!("Result: {:?}", result), // Int(14)
        Err(err) => eprintln!("Error: {}", err),
    }

    // Example 2: Variables and control flow
    let code2 = r#"
        $x = 10
        $y = 20

        IF $x < $y THEN
            RETURN "x is less"
        ELSE
            RETURN "x is greater or equal"
    "#;

    match execute_ovsm(code2) {
        Ok(result) => println!("Result: {:?}", result),
        Err(err) => eprintln!("Error: {}", err),
    }

    // Example 3: Loop with accumulator
    let code3 = r#"
        $sum = 0
        FOR $i IN [1..11]:
            $sum = $sum + $i
        RETURN $sum
    "#;

    match execute_ovsm(code3) {
        Ok(result) => println!("Sum 1-10: {:?}", result), // Int(55)
        Err(err) => eprintln!("Error: {}", err),
    }
}
```

### Run It

```bash
cargo run
```

Expected output:
```
Result: Int(14)
Result: String("x is less")
Sum 1-10: Int(55)
```

---

## Method 4: Unit Tests

Run existing tests to see more examples:

```bash
cd crates/ovsm

# Run all tests
cargo test

# Run specific test file
cargo test --test test_comparisons

# Run with output
cargo test -- --show-output

# Run specific test
cargo test test_if_statement

# Run unit tests only
cargo test --lib --bins
```

### Writing Your Own Tests

Create `tests/my_test.rs`:

```rust
use ovsm::{Evaluator, Parser, Scanner, Value};

fn execute(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

#[test]
fn test_my_feature() {
    let result = execute(r#"
        $result = 0
        FOR $i IN [1..6]:
            $result = $result + $i
        RETURN $result
    "#).unwrap();

    assert_eq!(result, Value::Int(15)); // 1+2+3+4+5
}
```

Run it:
```bash
cargo test test_my_feature
```

---

## Common Issues

### Issue 1: "Undefined variable" Error

**Problem:**
```ovsm
FOR $i IN [1..5]:
    $sum = $sum + $i  // ❌ $sum not defined!
```

**Solution:**
```ovsm
$sum = 0  // ✅ Define first
FOR $i IN [1..5]:
    $sum = $sum + $i
```

### Issue 2: "Unexpected token" After Loop

**Problem:**
```ovsm
FOR $i IN [1..5]:
    IF $i > 3 THEN
        $result = "found"

RETURN $result  // ❌ Parser might consume this!
```

**Solution A - Use BREAK:**
```ovsm
FOR $i IN [1..5]:
    IF $i > 3 THEN
        $result = "found"
        BREAK  // ✅ Signals end of loop

RETURN $result
```

**Solution B - Use ELSE:**
```ovsm
FOR $i IN [1..5]:
    IF $i > 3 THEN
        $result = "found"
    ELSE
        $result = "searching"

RETURN $result  // ✅ Unambiguous
```

**Solution C - Move RETURN Inside:**
```ovsm
$result = "not found"
FOR $i IN [1..5]:
    IF $i > 3 THEN
        RETURN "found"  // ✅ Early return

RETURN $result
```

### Issue 3: Division by Zero

**Problem:**
```ovsm
RETURN 10 / 0  // ❌ Runtime error
```

**Solution:**
```ovsm
$x = 10
$y = 0

IF $y == 0 THEN
    RETURN "Error: division by zero"
ELSE
    RETURN $x / $y
```

### Issue 4: Range Confusion

**Important:** Ranges are **exclusive** of the end value!

```ovsm
// [1..5] creates: 1, 2, 3, 4 (NOT 5!)
FOR $i IN [1..5]:
    // Loops 4 times, not 5!
```

To include 5, use `[1..6]`:
```ovsm
FOR $i IN [1..6]:
    // Loops 5 times: 1, 2, 3, 4, 5
```

---

## Language Features Quick Reference

### ✅ Fully Working

- Variables: `$var = value`
- Constants: `CONST NAME = value`
- Control flow: `IF/THEN/ELSE`, `FOR`, `WHILE`
- Loop control: `BREAK`, `CONTINUE`, `BREAK IF`, `CONTINUE IF`
- Operators: `+`, `-`, `*`, `/`, `%`, `**`, `<`, `>`, `==`, `!=`, `AND`, `OR`, `NOT`
- Data types: Int, Float, String, Bool, Null, Arrays, Objects, Ranges
- Return: `RETURN value`

### ⚠️ Has Issues

- `TRY/CATCH` - Parsed but has block termination bugs

### ❌ Not Implemented

- Lambda functions (`fn:`)
- `DECISION/BRANCH` constructs
- `PARALLEL` execution
- Advanced tools: `MAP`, `FILTER`, `REDUCE`, etc.

---

## Next Steps

1. **Try Examples:** Run all scripts in `examples/` directory
2. **Read Guide:** Check `USAGE_GUIDE.md` for complete language reference
3. **Check Status:** See `TEST_RESULTS_SUMMARY.md` for implementation status
4. **Write Code:** Create your own `.ovsm` scripts
5. **Run Tests:** Explore test files in `tests/` for more examples

---

## Resources

| File | Description |
|------|-------------|
| `USAGE_GUIDE.md` | Complete language reference and syntax |
| `TEST_RESULTS_SUMMARY.md` | Current implementation status |
| `examples/README.md` | Example scripts documentation |
| `tests/` | Unit test examples |

---

**Happy coding with OVSM! 🚀**

For questions or issues, check the test files or create an issue on GitHub.
