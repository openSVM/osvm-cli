# QA Test Runner - User Guide

**Version**: 1.0.0
**Created**: October 11, 2025
**Purpose**: Automated testing of OVSM code in markdown documentation

---

## Overview

The QA Test Runner is a command-line tool that automatically executes OVSM code blocks found in markdown files. This allows you to:

- **Validate documentation examples** - Ensure code examples in docs actually work
- **Run regression tests** - Test interpreter against known-good examples
- **Generate test reports** - Get pass/fail statistics for QA datasets

---

## Installation

The tool is included as an example in the OVSM crate:

```bash
cd /path/to/osvm-cli/crates/ovsm
cargo build --example qa_test_runner
```

---

## Usage

### Basic Usage

```bash
cargo run --example qa_test_runner -- path/to/file.md
```

### Example

```bash
cargo run --example qa_test_runner -- examples/sample_qa_test.md
```

### Output

```
📖 Reading: examples/sample_qa_test.md

🔍 Found 8 OVSM code blocks:
═══════════════════════════════════════════════

📝 Test #1:
───────────────────────────────────────────────
Code:
$x = 10
$y = 20
RETURN $x + $y
───────────────────────────────────────────────
✅ Result: Int(30)

[... more tests ...]

═══════════════════════════════════════════════
📊 Summary:
   Total tests: 8
   ✅ Passed: 8
   ❌ Failed: 0
   📈 Pass rate: 100.0%
```

---

## Markdown Format

### Code Block Syntax

The tool looks for OVSM code blocks using standard markdown fence syntax:

````markdown
```ovsm
$x = 10
$y = 20
RETURN $x + $y
```
````

### Multiple Tests

You can have multiple code blocks in a single file:

````markdown
# My Tests

## Test 1: Arithmetic

```ovsm
$sum = 10 + 20
RETURN $sum
```

## Test 2: Arrays

```ovsm
$numbers = [1, 2, 3]
$total = SUM($numbers)
RETURN $total
```
````

---

## Features Tested

The QA runner validates all implemented OVSM features:

### ✅ Working Features

- **Variables**: `$x = 10`
- **Arithmetic**: `$sum = $x + $y`
- **Comparisons**: `$x > 0`
- **Logic**: `$a AND $b`
- **IF-THEN-ELSE**: Conditional execution
- **WHILE loops**: Iteration
- **FOR loops**: Iteration with ranges
- **GUARD clauses**: Early exit validation
- **TRY-CATCH**: Error handling
- **Arrays**: `[1, 2, 3]`
- **Objects**: `{name: "Alice", age: 30}`
- **Ranges**: `[1..10]`
- **Built-in Tools**: SUM, MAX, MIN, COUNT, etc.

### ⚠️ Expected Errors

These features return `NotImplemented` errors:

- **Lambda functions**: `MAP($arr, $x => $x * 2)`
- **PARALLEL**: `PARALLEL { ... }`
- **DECISION**: `DECISION "choice": ...`
- **WAIT**: `WAIT exponential_backoff`

---

## Error Handling

The runner categorizes errors into three types:

### 1. Scanner Errors

**Cause**: Lexical errors in code
**Example**: Unclosed string literal

```ovsm
$str = "unclosed
RETURN $str
```

**Output**: `❌ Error: Scanner error: SyntaxError { ... }`

### 2. Parser Errors

**Cause**: Syntax errors in code
**Example**: Missing THEN keyword

```ovsm
IF $x > 0
    RETURN "positive"
```

**Output**: `❌ Error: Parser error: ParseError(...)`

### 3. Runtime Errors

**Cause**: Execution errors
**Example**: Division by zero

```ovsm
$x = 10 / 0
RETURN $x
```

**Output**: `❌ Error: Runtime error: DivisionByZero`

---

## Use Cases

### 1. Validate Documentation Examples

Test that all code examples in your documentation actually work:

```bash
cargo run --example qa_test_runner -- README.md
cargo run --example qa_test_runner -- QUICK_START.md
cargo run --example qa_test_runner -- examples/*.md
```

### 2. Run QA Test Suites

The `/home/larp/larpdevs/osvm-cli/test_qa_categories/` directory contains comprehensive QA tests:

```bash
# Test specific category
cargo run --example qa_test_runner -- ../../../test_qa_categories/06_token_research/01_basic.md

# Test all files in a directory
for file in ../../../test_qa_categories/06_token_research/*.md; do
    echo "Testing: $file"
    cargo run --example qa_test_runner -- "$file"
done
```

### 3. Regression Testing

After making interpreter changes, run all QA tests to ensure no regressions:

```bash
#!/bin/bash
# regression_test.sh

PASS=0
FAIL=0

for file in test_qa_categories/**/*.md; do
    if cargo run --example qa_test_runner -- "$file" 2>/dev/null | grep -q "100.0%"; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        echo "FAILED: $file"
    fi
done

echo "Regression test complete: $PASS passed, $FAIL failed"
```

---

## Interpreting Results

### Result Format

Each test shows:
- **Test number**: Sequential numbering
- **Code**: The OVSM code being tested
- **Result**: The execution result or error

### Value Formatting

Results are formatted for clarity:

| OVSM Value | Display Format |
|------------|----------------|
| Integer | `Int(42)` |
| Float | `Float(3.14)` |
| String | `String("hello")` |
| Boolean | `Bool(true)` |
| Null | `Null` |
| Array | `Array([Int(1), Int(2), Int(3)])` |
| Object | `Object({name: String("Alice"), age: Int(30)})` |
| Range | `Range(1..10)` |

### Summary Statistics

The final summary shows:
- **Total tests**: Number of code blocks found
- **Passed**: Tests that executed successfully
- **Failed**: Tests that returned errors
- **Pass rate**: Percentage of successful tests

---

## Troubleshooting

### No Code Blocks Found

**Problem**: `⚠️ No OVSM code blocks found in file!`

**Solution**: Ensure blocks are marked with ` ```ovsm ` (not ` ```ovs ` or other variants)

### Parser Errors on Valid Code

**Problem**: Code works in REPL but fails in QA runner

**Solution**: Check for:
- Missing RETURN statement at end
- Trailing whitespace after code
- Unclosed blocks (IF/WHILE/FOR/TRY)

### Unexpected Results

**Problem**: Test passes but result doesn't match expectation

**Solution**: Remember that:
- Assignments return `Null`, not the assigned value
- Use `RETURN` to get expression results
- Some operations modify state without returning values

---

## Advanced Usage

### Batch Testing

Test multiple files with a shell script:

```bash
#!/bin/bash
# batch_test.sh

echo "=== OVSM QA Test Suite ==="
echo ""

for category in test_qa_categories/*; do
    if [ -d "$category" ]; then
        echo "Testing category: $(basename $category)"
        for file in "$category"/*.md; do
            if [ -f "$file" ]; then
                echo "  - $(basename $file)"
                cargo run --example qa_test_runner -- "$file" 2>/dev/null | tail -4
            fi
        done
        echo ""
    fi
done
```

### CI/CD Integration

Use in continuous integration:

```yaml
# .github/workflows/qa-tests.yml
name: QA Tests

on: [push, pull_request]

jobs:
  qa-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run QA Test Suite
        run: |
          cd crates/ovsm
          cargo run --example qa_test_runner -- examples/sample_qa_test.md
          # Add more test files as needed
```

---

## Limitations

### Current Limitations

1. **No result verification**: The tool executes code but doesn't verify results match expected values
2. **No timeout**: Long-running code will hang
3. **No isolation**: Tests share the same process
4. **No setup/teardown**: Each test starts fresh but can't share state

### Future Enhancements

Planned features for v2.0:

- **Expected result blocks**: Compare actual vs expected
  ````markdown
  ```ovsm
  $x = 10 + 20
  RETURN $x
  ```

  ```result
  Int(30)
  ```
  ````

- **Timeout support**: Kill tests that run too long
- **Parallel execution**: Run tests concurrently
- **Test filtering**: Run specific tests by pattern
- **JSON output**: Machine-readable results

---

## Examples

### Example 1: Feature Validation

File: `test_new_features.md`

````markdown
# GUARD Clause Tests

## Test: GUARD passes

```ovsm
$x = 10
GUARD $x > 0 ELSE
    RETURN "negative"
RETURN "positive"
```

## Test: GUARD fails

```ovsm
$x = -5
GUARD $x > 0 ELSE
    RETURN "negative"
RETURN "positive"
```
````

Run:
```bash
cargo run --example qa_test_runner -- test_new_features.md
```

Expected output:
```
✅ Result: String("positive")
✅ Result: String("negative")
Pass rate: 100.0%
```

### Example 2: Error Handling

File: `test_errors.md`

````markdown
# Error Handling Tests

## Test: Division by zero caught

```ovsm
TRY:
    $result = 10 / 0
CATCH:
    $result = -1
RETURN $result
```

## Test: Undefined variable caught

```ovsm
TRY:
    $x = $undefined_var
CATCH:
    $x = null
RETURN $x
```
````

Run:
```bash
cargo run --example qa_test_runner -- test_errors.md
```

Expected output:
```
✅ Result: Int(-1)
✅ Result: Null
Pass rate: 100.0%
```

---

## Best Practices

### 1. One Concept Per Test

❌ **Bad** - Multiple concepts in one test:
```ovsm
$x = 10
$y = 20
$sum = $x + $y
GUARD $sum > 0 ELSE RETURN "invalid"
TRY:
    $result = 100 / $sum
CATCH:
    $result = 0
RETURN $result
```

✅ **Good** - Separate tests for each concept:
```ovsm
// Test 1: Arithmetic
$x = 10
$y = 20
RETURN $x + $y

// Test 2: GUARD validation
$sum = 30
GUARD $sum > 0 ELSE RETURN "invalid"
RETURN "valid"

// Test 3: TRY-CATCH
TRY:
    $result = 100 / 30
CATCH:
    $result = 0
RETURN $result
```

### 2. Clear Test Names

Use descriptive section headers:

````markdown
## Test: GUARD with negative number should fail
## Test: TRY-CATCH recovers from division by zero
## Test: FOR loop sums numbers 1-10
````

### 3. Document Expected Behavior

Add comments explaining what the test validates:

````markdown
## Test: Nested TRY-CATCH - Inner catch handles error

This test verifies that:
1. Inner TRY catches division by zero
2. Inner CATCH sets $x = 1
3. Outer TRY successfully divides by 1
4. Final result is 100

```ovsm
TRY:
    TRY:
        $x = 10 / 0
    CATCH:
        $x = 1
    $result = 100 / $x
CATCH:
    $result = 0
RETURN $result
```
````

---

## Summary

The QA Test Runner provides automated testing for OVSM code in markdown files, enabling:

- ✅ Documentation validation
- ✅ Regression testing
- ✅ Feature verification
- ✅ Error handling validation
- ✅ Pass/fail reporting

**Usage**:
```bash
cargo run --example qa_test_runner -- path/to/file.md
```

**Success criteria**:
- All code blocks execute
- Pass rate shown
- Clear error messages for failures

---

*QA Test Runner Guide - OVSM Interpreter v1.1.0*
*Created: October 11, 2025*
*Tool Status: Production Ready ✅*
