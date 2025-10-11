# OVSM Example Scripts

This directory contains example OVSM (Open Versatile Seeker Mind) scripts demonstrating core language features.

## Running the Examples

```bash
# Run a specific example
osvm ovsm run examples/ovsm_scripts/01_hello_world.ovsm

# Check syntax without running
osvm ovsm check examples/ovsm_scripts/02_control_flow.ovsm

# Evaluate inline code
osvm ovsm eval '$x = 42; RETURN $x'

# Run all examples
for script in examples/ovsm_scripts/*.ovsm; do
    echo "=== Running $script ==="
    osvm ovsm run "$script"
    echo
done
```

## Example Scripts

### 01_hello_world.ovsm
**Topics:** Variables, RETURN statement

The simplest OVSM script showing variable assignment and return values.

```ovsm
$greeting = "Hello from OVSM!"
RETURN $greeting
```

### 02_control_flow.ovsm
**Topics:** FOR loops, ranges

Demonstrates loop iteration with ranges:

```ovsm
$sum = 0
FOR $i IN [1..6]:  // Range [1..6) = 1,2,3,4,5
    $sum = $sum + $i
RETURN $sum  // Returns 15
```

### 03_arithmetic.ovsm
**Topics:** Math operations

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
