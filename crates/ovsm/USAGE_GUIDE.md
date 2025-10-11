# OVSM Usage Guide

## How to Execute OVSM Scripts

OVSM is a library crate, so you can execute scripts in several ways:

### 1. Using the Example Runner (Recommended)

The easiest way to run OVSM scripts from files:

```bash
cargo run --example run_file <script.ovsm>
```

**Example:**
```bash
cd crates/ovsm
cargo run --example run_file examples/hello_world.ovsm
```

### 2. Programmatic Usage

Use OVSM as a library in your Rust programs:

```rust
use ovsm::{Evaluator, Parser, Scanner, Value};

fn execute_ovsm(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    // Tokenize
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;

    // Execute
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

fn main() {
    let code = r#"
        $x = 10
        IF $x > 5 THEN
            RETURN "high"
        ELSE
            RETURN "low"
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
cargo test --test test_comparisons # Comparison operator tests
cargo test -- --show-output        # Show test output
```

---

## Example Scripts

### Hello World (`examples/hello_world.ovsm`)

```ovsm
// Simple Hello World example
$message = "Hello from OVSM! 🚀"
RETURN $message
```

**Output:** `String("Hello from OVSM! 🚀")`

---

### Factorial (`examples/factorial.ovsm`)

```ovsm
// Calculate factorial of a number
$n = 5
$result = 1

IF $n < 0 THEN
    RETURN "Error: Factorial undefined for negative numbers"
ELSE
    FOR $i IN [1..$n]:
        $result = $result * $i
    RETURN $result
```

**Output:** `Int(120)` (5! = 120)
**Note:** Currently returns 24 due to range being 1..5 (exclusive end)

---

### Conditional Logic (`examples/conditional_logic.ovsm`)

```ovsm
// Complex conditional logic
$score = 85

IF $score >= 90 THEN
    RETURN "Grade: A - Excellent!"
ELSE
    IF $score >= 80 THEN
        RETURN "Grade: B - Good job!"
    ELSE
        IF $score >= 70 THEN
            RETURN "Grade: C - Average"
        ELSE
            IF $score >= 60 THEN
                RETURN "Grade: D - Needs improvement"
            ELSE
                RETURN "Grade: F - Failed"
```

**Output:** `String("Grade: B - Good job!")`

---

### Array Operations (`examples/array_operations.ovsm`)

```ovsm
// Array iteration and operations
$numbers = [1, 2, 3, 4, 5]
$sum = 0
$count = 0
$average = 0

FOR $num IN $numbers:
    $sum = $sum + $num
    $count = $count + 1
    $average = $sum / $count

RETURN $average
```

**Output:** `Int(3)` (average of 1,2,3,4,5)

---

### Loop Control (`examples/loop_control.ovsm`)

```ovsm
// Loop control with BREAK and CONTINUE
$sum = 0

FOR $i IN [1..20]:
    // Skip even numbers
    IF $i % 2 == 0 THEN
        CONTINUE
    ELSE
        // Stop at 15
        IF $i > 15 THEN
            BREAK
        ELSE
            $sum = $sum + $i

RETURN $sum
```

**Output:** `Int(64)` (sum of odd numbers 1-15)

---

## OVSM Language Features

### Supported Features ✅

#### Control Flow
- `IF/THEN/ELSE` - Conditional execution
- `FOR ... IN` - Iterate over arrays, ranges, strings
- `WHILE` - Loop while condition is true
- `BREAK` - Exit loop early (including `BREAK IF condition`)
- `CONTINUE` - Skip to next iteration (including `CONTINUE IF condition`)
- `RETURN` - Return value and exit

#### Data Types
- **Integers:** `42`, `-10`
- **Floats:** `3.14`, `-0.5`
- **Strings:** `"hello"`, `"world"`
- **Booleans:** `TRUE`, `FALSE`
- **Null:** `NULL`
- **Arrays:** `[1, 2, 3]`, `["a", "b"]`
- **Objects:** `{name: "Alice", age: 30}`
- **Ranges:** `[1..10]` (exclusive end)

#### Operators
- **Arithmetic:** `+`, `-`, `*`, `/`, `%`, `**` (power)
- **Comparison:** `<`, `>`, `<=`, `>=`, `==`, `!=`
- **Logical:** `AND`, `OR`, `NOT`
- **Ternary:** `condition ? then : else`
- **Membership:** `IN` (check if item in array/string)

#### Variables
- **Assignment:** `$variable = value`
- **Constants:** `CONST NAME = value`
- **Scoping:** Proper scope chain with shadowing

### Not Yet Implemented ❌

- `TRY/CATCH` - Error handling (parsed but has bugs)
- `DECISION/BRANCH` - Advanced decision structures
- Lambda functions (`fn:` syntax)
- `PARALLEL` execution
- `WAIT_ALL`, `WAIT_ANY`, `RACE` strategies
- `GUARD` statements
- `MATCH` expressions
- Advanced tools: `MAP`, `FILTER`, `REDUCE`, `SUM`, `MEAN`, etc.

---

## Important Syntax Notes

### ⚠️ Block Termination Rules

OVSM lacks explicit block delimiters (braces) or significant indentation. Follow these rules:

#### ✅ DO: Use Explicit ELSE Branches

```ovsm
// GOOD - Clear block boundaries
FOR $i IN [1..10]:
    IF $i > 5 THEN
        $result = "big"
        BREAK
    ELSE
        $result = "small"

RETURN $result  # Unambiguous
```

#### ✅ DO: Calculate Inside Loops

```ovsm
// GOOD - All work happens in loop
$sum = 0
FOR $i IN [1..10]:
    $sum = $sum + $i
    $average = $sum / $i  # Calculate here

RETURN $average
```

#### ❌ DON'T: Put Statements After Loops Without BREAK

```ovsm
// BAD - Parser may consume RETURN into loop!
FOR $i IN [1..10]:
    IF $i > 5 THEN
        $result = "found"

RETURN $result  # ← This might get consumed into FOR body!
```

#### ✅ FIX: Use BREAK to Signal End

```ovsm
// GOOD - BREAK signals end of loop
FOR $i IN [1..10]:
    IF $i > 5 THEN
        $result = "found"
        BREAK  # ← Explicitly ends loop

RETURN $result  # Now unambiguous
```

---

## Testing Your Scripts

### Quick Test

```bash
cargo run --example run_file your_script.ovsm
```

### With Debugging

Add print statements (when implemented) or use RETURN at strategic points:

```ovsm
$x = 10
$y = 20
$sum = $x + $y
RETURN $sum  # Check intermediate value
```

### Running Unit Tests

```bash
# All tests
cargo test

# Specific test file
cargo test --test test_comparisons

# Show output
cargo test -- --show-output --nocapture
```

---

## Common Patterns

### Accumulator Pattern

```ovsm
$sum = 0
FOR $i IN [1..10]:
    $sum = $sum + $i
RETURN $sum
```

### Find Pattern

```ovsm
$found = FALSE
FOR $item IN $array:
    IF $item == $target THEN
        $found = TRUE
        BREAK
RETURN $found
```

### Filter Pattern

```ovsm
$evens = []
FOR $num IN $numbers:
    IF $num % 2 == 0 THEN
        // Note: APPEND not implemented yet
        // For now, just count or accumulate
        $count = $count + 1
RETURN $count
```

### Nested Loops

```ovsm
$result = 0
FOR $i IN [1..5]:
    FOR $j IN [1..5]:
        $result = $result + ($i * $j)
RETURN $result
```

---

## Performance Notes

- **Fast parsing:** Simple recursive descent parser
- **Fast execution:** Direct AST interpretation
- **Memory efficient:** No unnecessary allocations
- **Safe:** Full scope isolation, no undefined behavior

---

## Troubleshooting

### "Undefined variable" Error

Make sure variables are defined before use, and check scope:

```ovsm
// BAD
FOR $i IN [1..5]:
    $sum = $sum + $i  # $sum not defined!

// GOOD
$sum = 0
FOR $i IN [1..5]:
    $sum = $sum + $i
```

### "Unexpected token" Error

Check for missing ELSE branches or ambiguous block termination:

```ovsm
// BAD
IF $x > 5 THEN
    $result = "big"
$other = 1  # Might be parsed as part of IF!

// GOOD
IF $x > 5 THEN
    $result = "big"
ELSE
    $result = "small"
$other = 1  # Now unambiguous
```

### Division by Zero

```ovsm
IF $denominator == 0 THEN
    RETURN "Error: division by zero"
ELSE
    RETURN $numerator / $denominator
```

---

## Next Steps

1. **Explore Examples:** Run all scripts in `examples/` directory
2. **Read Tests:** Check `tests/` for more complex examples
3. **Check Status:** See `TEST_RESULTS_SUMMARY.md` for current implementation status
4. **Report Issues:** File bugs or feature requests on GitHub

---

## Quick Reference

| Feature | Syntax | Example |
|---------|--------|---------|
| Variable | `$name = value` | `$x = 42` |
| Constant | `CONST NAME = value` | `CONST PI = 3.14` |
| If/Else | `IF cond THEN ... ELSE ...` | `IF $x > 5 THEN RETURN "big" ELSE RETURN "small"` |
| For Loop | `FOR $var IN iterable:` | `FOR $i IN [1..10]:` |
| While Loop | `WHILE condition:` | `WHILE $x < 10:` |
| Break | `BREAK` or `BREAK IF cond` | `BREAK IF $found` |
| Continue | `CONTINUE` or `CONTINUE IF cond` | `CONTINUE IF $skip` |
| Return | `RETURN value` | `RETURN $result` |
| Array | `[item1, item2, ...]` | `[1, 2, 3]` |
| Object | `{key: value, ...}` | `{name: "Alice"}` |
| Range | `[start..end]` | `[1..10]` |
| Comment | `//` | `// This is a comment` |

---

**Happy coding with OVSM! 🚀**
