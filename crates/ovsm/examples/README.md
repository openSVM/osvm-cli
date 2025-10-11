# OVSM Examples

This directory contains example OVSM scripts and Rust programs demonstrating the interpreter.

## Running Examples

### Execute OVSM Script Files

```bash
cargo run --example run_file <script.ovsm>
```

### Available Scripts

| Script | Description | Output |
|--------|-------------|--------|
| `hello_world.ovsm` | Simple hello world | `String("Hello from OVSM! 🚀")` |
| `factorial.ovsm` | Calculate 5! with FOR loop | `Int(120)` |
| `fibonacci.ovsm` | Calculate 10th Fibonacci number | `Int(55)` |
| `array_operations.ovsm` | Array iteration and average | `Int(3)` |
| `conditional_logic.ovsm` | Nested IF/ELSE for grading | `String("Grade: B...")` |
| `loop_control.ovsm` | BREAK and CONTINUE demo | `Int(64)` |

### Run All Examples

```bash
# Hello World
cargo run --example run_file examples/hello_world.ovsm

# Factorial (5! = 120)
cargo run --example run_file examples/factorial.ovsm

# Fibonacci (10th number = 55)
cargo run --example run_file examples/fibonacci.ovsm

# Array operations (average = 3)
cargo run --example run_file examples/array_operations.ovsm

# Conditional logic (grade based on score)
cargo run --example run_file examples/conditional_logic.ovsm

# Loop control (BREAK/CONTINUE)
cargo run --example run_file examples/loop_control.ovsm
```

## Example Rust Programs

| File | Description |
|------|-------------|
| `run_file.rs` | Execute OVSM scripts from files |
| `complete_demo.rs` | Comprehensive feature demonstration |
| `comprehensive_tools.rs` | Tool system examples |
| `qa_test_runner.rs` | QA test suite runner |
| `showcase_new_features.rs` | Feature showcase |
| `tools_demo.rs` | Standard library tools |

### Running Rust Examples

```bash
cargo run --example complete_demo
cargo run --example tools_demo
# etc.
```

## Creating Your Own Scripts

### Basic Template

```ovsm
// your_script.ovsm
$result = 0

FOR $i IN [1..10]:
    $result = $result + $i

RETURN $result
```

### Run It

```bash
cargo run --example run_file your_script.ovsm
```

## Important Notes

1. **Block Termination:** Use explicit ELSE branches or BREAK statements to avoid ambiguity
2. **Variable Scope:** Define variables before loops that need them
3. **Ranges:** `[1..10]` creates range 1-9 (exclusive end)
4. **Comments:** Use `//` for single-line comments

## See Also

- `../USAGE_GUIDE.md` - Complete language reference
- `../TEST_RESULTS_SUMMARY.md` - Implementation status
- `../tests/` - Unit test examples
