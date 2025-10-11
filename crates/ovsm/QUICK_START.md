# OVSM Interpreter - Quick Start Guide

**Version**: 1.1.0
**Status**: Production Ready ✅
**Last Updated**: October 10, 2025

---

## 🚀 5-Minute Quick Start

### Installation

```bash
# Add to your project
cd /path/to/osvm-cli
cargo build --package ovsm --release

# Verify installation
cargo test --package ovsm
# Should see: 108 tests passing
```

### Your First OVSM Program

```rust
use ovsm::{Evaluator, Parser, Scanner};

fn main() {
    let code = r#"
        $x = 10
        $y = 20
        RETURN $x + $y
    "#;

    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = Evaluator::new();
    let result = evaluator.execute(&program).unwrap();

    println!("Result: {:?}", result);  // Int(30)
}
```

---

## 📚 Essential Features

### 1. Variables & Constants

```ovsm
// Variables (mutable)
$name = "Alice"
$age = 30
$balance = 100.50

// Constants (immutable)
CONST PI = 3.14159
CONST MAX_USERS = 1000
```

### 2. Arithmetic & Comparisons

```ovsm
// Arithmetic
$sum = 10 + 20
$product = 5 * 6
$power = 2 ** 8  // 256

// Comparisons
$is_adult = $age >= 18
$is_equal = $x == $y
$is_greater = $a > $b
```

### 3. Control Flow

```ovsm
// IF-THEN-ELSE
IF $score >= 90 THEN
    $grade = "A"
ELSE
    $grade = "B"

// WHILE loop
$i = 0
WHILE $i < 10:
    $sum = $sum + $i
    $i = $i + 1

// FOR loop
FOR $item IN [1, 2, 3, 4, 5]:
    $total = $total + $item
```

### 4. ✨ GUARD Clauses (NEW!)

```ovsm
// Early exit pattern
$amount = 150
$balance = 100

GUARD $balance >= $amount ELSE
    RETURN "Insufficient funds"

$balance = $balance - $amount
RETURN "Success"
```

### 5. ✨ TRY-CATCH (NEW!)

```ovsm
// Error handling
TRY:
    $result = 10 / 0
CATCH:
    $result = 0

RETURN $result  // Returns 0
```

### 6. Collections

```ovsm
// Arrays
$numbers = [1, 2, 3, 4, 5]
$first = $numbers[0]

// Objects
$user = {name: "Alice", age: 30}
$name = $user.name

// Ranges
FOR $i IN [1..10]:
    $sum = $sum + $i
```

### 7. Tools (Built-in Functions)

```ovsm
// Array operations
$sum = SUM([1, 2, 3, 4, 5])        // 15
$max = MAX([5, 2, 8, 1, 9])        // 9
$sorted = SORT([5, 2, 8, 1, 9])    // [1, 2, 5, 8, 9]

// Math operations
$abs_value = ABS(-10)               // 10
$sqrt = SQRT(16)                    // 4
$rounded = ROUND(3.7)               // 4

// String operations
$joined = JOIN(["a", "b", "c"], ",")  // "a,b,c"
$split = SPLIT("a,b,c", ",")          // ["a", "b", "c"]
```

---

## 🎯 Common Patterns

### Pattern 1: Safe Data Access

```ovsm
$data = [1, 2, 3]
$index = 10

TRY:
    $value = $data[$index]
CATCH:
    $value = null

RETURN $value  // Returns null (safe fallback)
```

### Pattern 2: Input Validation

```ovsm
GUARD $email != null ELSE
    RETURN "Email required"

GUARD $age >= 18 ELSE
    RETURN "Must be 18 or older"

GUARD $terms_accepted ELSE
    RETURN "Must accept terms"

// All guards passed - proceed with registration
RETURN "Registration successful"
```

### Pattern 3: Error Recovery

```ovsm
// Try primary source
TRY:
    $data = FETCH_FROM_API()
CATCH:
    // Fallback to cache
    TRY:
        $data = READ_FROM_CACHE()
    CATCH:
        // Final fallback to defaults
        $data = []

RETURN $data  // Always has a value
```

### Pattern 4: Configuration with Defaults

```ovsm
$config = {timeout: 30, retries: 3}

TRY:
    $timeout = $config.timeout
CATCH:
    $timeout = 60  // Default

TRY:
    $retries = $config.retries
CATCH:
    $retries = 5  // Default

RETURN {timeout: $timeout, retries: $retries}
```

---

## ⚠️ Important Limitations

### Not Yet Implemented

These features will return clear `NotImplemented` errors:

```ovsm
// ❌ Lambda functions
$doubled = MAP($numbers, $x => $x * 2)  // Error!

// ❌ PARALLEL execution
PARALLEL {
    $task1 = FETCH_DATA()
    $task2 = PROCESS_DATA()
}  // Error!

// ❌ DECISION points
DECISION "Choose strategy":
    BRANCH "aggressive": $speed = "fast"
    BRANCH "safe": $speed = "slow"
// Error!
```

**Workarounds**:
- Use FOR loops instead of MAP/FILTER
- Use sequential code instead of PARALLEL
- Use IF-THEN-ELSE instead of DECISION

---

## 🐛 Troubleshooting

### Common Errors

**1. "Undefined variable"**
```ovsm
// ❌ Wrong
RETURN $x  // Error if $x not defined

// ✅ Correct
$x = 10
RETURN $x
```

**2. "Division by zero"**
```ovsm
// ❌ Wrong
$result = 10 / 0  // Error!

// ✅ Correct
TRY:
    $result = 10 / $divisor
CATCH:
    $result = 0
```

**3. "Index out of bounds"**
```ovsm
// ❌ Wrong
$arr = [1, 2, 3]
$val = $arr[10]  // Error!

// ✅ Correct
TRY:
    $val = $arr[10]
CATCH:
    $val = null
```

**4. "Constant reassignment"**
```ovsm
// ❌ Wrong
CONST MAX = 100
MAX = 200  // Error!

// ✅ Correct
$max = 100  // Use variable if it needs to change
$max = 200
```

---

## 📖 Complete Example

Here's a real-world example combining multiple features:

```ovsm
// User authentication and validation
$username = "alice"
$password = "secret123"
$age = 25

// Validation with GUARD clauses
GUARD $username != null ELSE
    RETURN {success: false, error: "Username required"}

GUARD $password != null ELSE
    RETURN {success: false, error: "Password required"}

GUARD $age >= 18 ELSE
    RETURN {success: false, error: "Must be 18+"}

// Try to authenticate
TRY:
    $user = AUTHENTICATE($username, $password)
CATCH:
    RETURN {success: false, error: "Invalid credentials"}

// Try to load user data
TRY:
    $profile = LOAD_PROFILE($user.id)
CATCH:
    $profile = {name: "Unknown", level: 1}  // Defaults

// Success!
RETURN {
    success: true,
    user: $user,
    profile: $profile
}
```

---

## 🧪 Testing Your Code

### Unit Test Example

```rust
#[test]
fn test_my_ovsm_code() {
    let code = r#"
        $x = 10
        GUARD $x > 0 ELSE
            RETURN -1
        RETURN $x
    "#;

    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = Evaluator::new();
    let result = evaluator.execute(&program).unwrap();

    assert_eq!(result, ovsm::Value::Int(10));
}
```

---

## 📚 Further Reading

- **FIXES_SUMMARY.md** - Technical details of recent fixes
- **THIRD_ASSESSMENT.md** - Quality assessment journey
- **RELEASE_NOTES.md** - Complete changelog
- **IMPLEMENTATION_STATUS.md** - Full feature list

### Examples

- `examples/test_guard.rs` - GUARD clause examples
- `examples/test_try_catch.rs` - TRY-CATCH examples
- `examples/showcase_new_features.rs` - 7 real-world patterns
- `examples/comprehensive_tools.rs` - All 34 tools demonstrated

---

## 🎯 Best Practices

### ✅ Do

```ovsm
// Use GUARD for preconditions
GUARD $input != null ELSE
    RETURN "Invalid input"

// Use TRY-CATCH for error handling
TRY:
    $result = RISKY_OPERATION()
CATCH:
    $result = SAFE_DEFAULT()

// Chain multiple GUARDs
GUARD $authenticated ELSE RETURN "Not logged in"
GUARD $authorized ELSE RETURN "Not authorized"
```

### ❌ Don't

```ovsm
// Don't nest IF statements when GUARD works better
// ❌ Avoid this:
IF $x > 0 THEN
    IF $y > 0 THEN
        // ... deep nesting ...

// ✅ Do this instead:
GUARD $x > 0 ELSE RETURN "Invalid X"
GUARD $y > 0 ELSE RETURN "Invalid Y"
// ... clear linear code ...

// Don't ignore errors
// ❌ Avoid this:
$result = 10 / $divisor  // Might error!

// ✅ Do this:
TRY:
    $result = 10 / $divisor
CATCH:
    $result = 0
```

---

## 🚀 Performance Tips

1. **Use constants**: `CONST` is faster than `$variable` for immutable values
2. **Avoid deep nesting**: Use GUARD clauses to keep code flat
3. **Reuse collections**: Arrays/objects use Arc, so cloning is cheap
4. **Use tools**: Built-in tools are optimized (SUM, MAX, etc.)

---

## 💡 Pro Tips

### Tip 1: Error Messages

```ovsm
GUARD $valid ELSE
    RETURN "Descriptive error message here"
```

### Tip 2: Nested TRY-CATCH

```ovsm
TRY:
    TRY:
        $data = PRIMARY_SOURCE()
    CATCH:
        $data = BACKUP_SOURCE()
    $result = PROCESS($data)
CATCH:
    $result = null
```

### Tip 3: Tool Chaining

```ovsm
$result = TOP_N(
    SORT(
        FILTER($data, ...),
        ...
    ),
    5
)
```

---

## ✅ Quick Reference Card

| Feature | Syntax | Example |
|---------|--------|---------|
| Variable | `$name = value` | `$x = 10` |
| Constant | `CONST NAME = value` | `CONST PI = 3.14` |
| IF | `IF cond THEN ... ELSE ...` | `IF $x > 0 THEN $y = 1 ELSE $y = 0` |
| WHILE | `WHILE cond: ...` | `WHILE $i < 10: $i = $i + 1` |
| FOR | `FOR $var IN list: ...` | `FOR $x IN [1,2,3]: $sum = $sum + $x` |
| GUARD | `GUARD cond ELSE stmt` | `GUARD $x > 0 ELSE RETURN -1` |
| TRY | `TRY: ... CATCH: ...` | `TRY: $x = 1/0 CATCH: $x = 0` |
| Array | `[1, 2, 3]` | `$arr = [1, 2, 3]` |
| Object | `{k: v}` | `$obj = {name: "Alice"}` |
| Range | `[start..end]` | `FOR $i IN [1..10]: ...` |

---

## 🎉 You're Ready!

You now know enough to:
- ✅ Write OVSM scripts
- ✅ Handle errors gracefully
- ✅ Validate inputs with GUARD
- ✅ Use 34 built-in tools
- ✅ Debug common issues

**Happy coding!** 🚀

---

*Quick Start Guide - OVSM Interpreter v1.1.0*
*For more details, see the full documentation*
