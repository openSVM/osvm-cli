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

### Common Runtime Errors

#### 1. "Undefined variable: $name"

**Cause:** Using a variable before it's been assigned.

**Example error:**
```ovsm
// BAD
FOR $i IN [1..5]:
    $sum = $sum + $i  // $sum not defined!
```

**Solution:** Initialize variables before use.
```ovsm
// GOOD
$sum = 0  // Initialize first
FOR $i IN [1..5]:
    $sum = $sum + $i
```

**Scope note:** Variables defined inside FOR loops are not accessible outside:
```ovsm
FOR $i IN [1..5]:
    $temp = $i * 2

RETURN $temp  // ERROR: $temp not defined in this scope
```

---

#### 2. "Division by zero"

**Cause:** Dividing or taking modulo by zero.

**Example error:**
```ovsm
$result = 10 / 0  // ERROR: Division by zero
$remainder = 5 % 0  // ERROR: Division by zero
```

**Solution:** Always check denominators before division.
```ovsm
// GOOD: Guard clause
GUARD $denominator != 0 ELSE
    ERROR("Cannot divide by zero")

$result = $numerator / $denominator

// GOOD: Conditional check
IF $denominator == 0 THEN
    RETURN "Error: division by zero"
ELSE
    RETURN $numerator / $denominator
```

---

#### 3. "Index out of bounds: {index} for array of length {length}"

**Cause:** Accessing array index beyond array size.

**Example error:**
```ovsm
$arr = [1, 2, 3]
$value = $arr[5]  // ERROR: Index 5 out of bounds (length 3)
```

**Solution:** Check array length before indexing.
```ovsm
// GOOD: Check length
$arr = [1, 2, 3]
$index = 5

IF $index < LEN($arr) THEN
    $value = $arr[$index]
ELSE
    $value = null  // Default value
```

**Prevention:** Use LEN() tool to validate indices.
```ovsm
$length = LEN($arr)
GUARD $index >= 0 AND $index < $length ELSE
    ERROR("Index out of range")
```

---

#### 4. "Type error: expected {expected}, got {got}"

**Cause:** Operation expecting one type but receiving another.

**Example errors:**
```ovsm
$x = "hello" + 5  // ERROR: Cannot add string and number
$y = IF "text" THEN 1 ELSE 0  // ERROR: String cannot be used as boolean
```

**Solution:** Ensure type compatibility.
```ovsm
// GOOD: Consistent types
$x = "hello" + " world"  // String concatenation
$y = 5 + 10  // Number addition

// GOOD: Explicit boolean conditions
$text = "hello"
IF $text != null AND $text != "" THEN
    $result = "valid"
```

**Type checking pattern:**
```ovsm
// Validate number type
TRY:
    $test = $input + 0  // Will fail if not a number
CATCH:
    ERROR("Value must be a number")
```

---

#### 5. "Undefined tool: {name}"

**Cause:** Calling a tool that doesn't exist or isn't implemented yet.

**Example error:**
```ovsm
$result = MAP($array, $func)  // ERROR: MAP not implemented
```

**Solution:** Check available tools in documentation or use workarounds.
```ovsm
// Workaround: Manual mapping
$result = []
FOR $item IN $array:
    $transformed = $item * 2
    $result = $result + [$transformed]
```

**Available stdlib tools:**
- Math: `ABS()`, `SQRT()`, `POW()`
- Statistics: `SUM()`, `MIN()`, `MAX()`, `MEAN()`
- Data: `SORT()`, `FILTER()`
- Utilities: `LOG()`, `ERROR()`, `LEN()`

---

### Common Parse Errors

#### 1. "Syntax error: Expected THEN after IF condition"

**Cause:** Missing THEN keyword in IF statement.

**Example error:**
```ovsm
// BAD
IF $x > 5
    RETURN "big"
```

**Solution:** Always include THEN.
```ovsm
// GOOD
IF $x > 5 THEN
    RETURN "big"
ELSE
    RETURN "small"
```

---

#### 2. "Unexpected token: expected expression, got {token}"

**Cause:** Syntax error in expression or missing operator.

**Example errors:**
```ovsm
$x = + 5  // Missing left operand
$y = [1, 2, 3  // Missing closing bracket
$z = {name: "Alice", age  // Missing value
```

**Solution:** Check expression syntax.
```ovsm
// GOOD
$x = 10 + 5
$y = [1, 2, 3]
$z = {name: "Alice", age: 30}
```

---

#### 3. "Expected ':' after FOR iterable" or "Expected ':' after WHILE condition"

**Cause:** Missing colon after loop declaration.

**Example error:**
```ovsm
// BAD
FOR $i IN [1..10]
    $sum = $sum + $i

WHILE $x < 10
    $x = $x + 1
```

**Solution:** Add colons.
```ovsm
// GOOD
FOR $i IN [1..10]:
    $sum = $sum + $i

WHILE $x < 10:
    $x = $x + 1
```

---

### Block Termination Issues

#### Problem: Statements Consumed Into Loops

**Symptom:** RETURN or other statements after loops don't execute as expected.

**Example:**
```ovsm
// PROBLEMATIC
FOR $i IN [1..10]:
    IF $i > 5 THEN
        $result = "found"

RETURN $result  // May be parsed as part of FOR body!
```

**Solution 1:** Use BREAK to explicitly end loops.
```ovsm
// GOOD
FOR $i IN [1..10]:
    IF $i > 5 THEN
        $result = "found"
        BREAK  // Explicitly ends loop

RETURN $result  // Now unambiguous
```

**Solution 2:** Use ELSE branches.
```ovsm
// GOOD
FOR $i IN [1..10]:
    IF $i > 5 THEN
        $result = "found"
        BREAK
    ELSE
        CONTINUE  // Explicit control flow

RETURN $result
```

**Solution 3:** Return inside loops.
```ovsm
// GOOD
FOR $i IN [1..10]:
    IF $i > 5 THEN
        RETURN "found"  // Exits immediately

RETURN "not found"
```

---

### Performance Issues

#### Slow Execution

**Cause:** Inefficient loop patterns or unnecessary calculations.

**Bad pattern:**
```ovsm
// Calculates length every iteration
FOR $i IN [0..LEN($array)]:
    $item = $array[$i]
    LOG($item)
```

**Good pattern:**
```ovsm
// Calculate once
$length = LEN($array)
FOR $i IN [0..$length]:
    $item = $array[$i]
    LOG($item)

// Or iterate directly
FOR $item IN $array:
    LOG($item)
```

---

### Debugging Tips

#### 1. Use LOG() for debugging

```ovsm
$x = 10
LOG("Value of x:", $x)

FOR $i IN [1..5]:
    LOG("Iteration:", $i)
    $sum = $sum + $i
    LOG("Current sum:", $sum)

RETURN $sum
```

#### 2. Return intermediate values

```ovsm
$a = 10
$b = 20
$sum = $a + $b
RETURN $sum  // Check intermediate result

// Continue with more logic...
```

#### 3. Test with simple cases first

```ovsm
// Test with small array first
$test_array = [1, 2, 3]
// ... test logic ...

// Then scale to larger arrays
$real_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

#### 4. Validate assumptions

```ovsm
// Check assumptions before processing
GUARD $input != null ELSE
    ERROR("Input is null")

GUARD LEN($array) > 0 ELSE
    ERROR("Array is empty")

// Safe to proceed
```

---

### Getting Help

#### Check Documentation

1. **API Docs:** [docs.rs/ovsm](https://docs.rs/ovsm) - Complete API reference
2. **Common Patterns:** See `docs/COMMON_PATTERNS.md` for idiomatic code
3. **Examples:** Check `examples/` directory for working scripts
4. **Test Suite:** Read `tests/` for complex usage examples

#### Verify Implementation Status

Some features are parsed but not fully implemented. Check:
- `TEST_RESULTS_SUMMARY.md` - Test coverage report
- `CHANGELOG.md` - Recent changes and fixes
- GitHub Issues - Known bugs and planned features

#### Report Issues

If you encounter a bug:
1. Create minimal reproduction case
2. Check if it's a known issue
3. File bug report with example code
4. Include error message and expected behavior

---

### Quick Troubleshooting Checklist

- [ ] All variables initialized before use?
- [ ] No division by zero?
- [ ] Array indices within bounds?
- [ ] Type compatibility in operations?
- [ ] Required keywords (THEN, ELSE, colons)?
- [ ] Explicit BREAK/RETURN for loop termination?
- [ ] Tool exists and is implemented?
- [ ] Valid syntax (brackets, braces, parentheses)?
- [ ] Checked examples for similar patterns?
- [ ] Consulted API documentation?

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
