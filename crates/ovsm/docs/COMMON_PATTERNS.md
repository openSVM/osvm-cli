# OVSM Common Patterns Guide

A collection of idiomatic patterns and best practices for OVSM scripting.

## Table of Contents

1. [Error Handling Patterns](#error-handling-patterns)
2. [Collection Manipulation](#collection-manipulation)
3. [Loop Patterns](#loop-patterns)
4. [Conditional Logic](#conditional-logic)
5. [Data Validation](#data-validation)
6. [Performance Patterns](#performance-patterns)
7. [Testing Patterns](#testing-patterns)

---

## Error Handling Patterns

### Early Return (Guard Clauses)

**Pattern**: Validate inputs early and return/exit on failure.

```ovsm
// Bad: Nested conditionals
IF $input != null THEN
    IF $input > 0 THEN
        RETURN $input * 2
    ELSE
        ERROR("Input must be positive")
ELSE
    ERROR("Input cannot be null")

// Good: Guard clauses
GUARD $input != null ELSE
    ERROR("Input cannot be null")

GUARD $input > 0 ELSE
    ERROR("Input must be positive")

RETURN $input * 2
```

### Try-Catch for Recoverable Errors

**Pattern**: Use TRY-CATCH for operations that may fail but shouldn't crash the program.

```ovsm
TRY:
    $result = RISKY_OPERATION($data)
    RETURN $result
CATCH:
    LOG("Operation failed, using default")
    RETURN $default_value
```

### Safe Division

**Pattern**: Always check for zero before division.

```ovsm
// Bad: May crash on zero
$result = $numerator / $denominator

// Good: Check first
IF $denominator == 0 THEN
    ERROR("Cannot divide by zero")

$result = $numerator / $denominator
```

---

## Collection Manipulation

### Filtering Arrays

**Pattern**: Collect items that match a condition.

```ovsm
$numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
$evens = []

FOR $num IN $numbers:
    IF $num % 2 == 0 THEN
        $evens = $evens + [$num]

RETURN $evens  // [2, 4, 6, 8, 10]
```

### Mapping (Transforming Elements)

**Pattern**: Transform each element in a collection.

```ovsm
$prices = [10, 20, 30, 40]
$with_tax = []
$tax_rate = 1.08

FOR $price IN $prices:
    $final = $price * $tax_rate
    $with_tax = $with_tax + [$final]

RETURN $with_tax
```

### Finding Maximum/Minimum

**Pattern**: Use built-in tools for efficiency.

```ovsm
$numbers = [42, 17, 99, 3, 54]

// Using built-in tools (preferred)
$max_value = MAX($numbers)
$min_value = MIN($numbers)

// Manual approach (if needed)
$max = $numbers[0]
FOR $num IN $numbers:
    IF $num > $max THEN
        $max = $num
```

### Array Aggregation (Sum, Average)

**Pattern**: Accumulate values from a collection.

```ovsm
$scores = [85, 92, 78, 90, 88]

// Sum using built-in
$total = SUM($scores)

// Average
$count = LEN($scores)
$average = $total / $count

RETURN {total: $total, average: $average, count: $count}
```

---

## Loop Patterns

### Early Exit with BREAK IF

**Pattern**: Exit loop when condition is met.

```ovsm
$found = null

FOR $item IN $items:
    IF $item.id == $target_id THEN
        $found = $item
        BREAK

IF $found == null THEN
    ERROR("Item not found")

RETURN $found
```

### Skip Invalid Items with CONTINUE IF

**Pattern**: Skip processing for items that don't meet criteria.

```ovsm
$valid_items = []

FOR $item IN $all_items:
    // Skip null or invalid items
    CONTINUE IF $item == null
    CONTINUE IF $item.status != "active"

    $valid_items = $valid_items + [$item]

RETURN $valid_items
```

### Range-Based Counting

**Pattern**: Use ranges for numeric iterations.

```ovsm
// Count from 1 to 10
$sum = 0
FOR $i IN [1..11]:
    $sum = $sum + $i

RETURN $sum  // 55 (1+2+3+...+10)
```

### Nested Loop with Early Exit

**Pattern**: Search in nested structures.

```ovsm
$found = false
$result = null

FOR $group IN $groups:
    FOR $item IN $group.items:
        IF $item.id == $target THEN
            $result = $item
            $found = true
            BREAK

    BREAK IF $found

RETURN $result
```

---

## Conditional Logic

### Ternary Operator for Simple Choices

**Pattern**: Use ternary for concise conditional assignment.

```ovsm
// Bad: Verbose
IF $score >= 60 THEN
    $status = "pass"
ELSE
    $status = "fail"

// Good: Concise
$status = $score >= 60 ? "pass" : "fail"
```

### Multi-Level Classification

**Pattern**: Classify values into multiple categories.

```ovsm
$score = 85

IF $score >= 90 THEN
    $grade = "A"
ELSE
    IF $score >= 80 THEN
        $grade = "B"
    ELSE
        IF $score >= 70 THEN
            $grade = "C"
        ELSE
            IF $score >= 60 THEN
                $grade = "D"
            ELSE
                $grade = "F"

RETURN $grade
```

### Boolean Flag Pattern

**Pattern**: Use boolean flags for state tracking.

```ovsm
$has_errors = false
$errors = []

FOR $item IN $items:
    IF $item.value < 0 THEN
        $has_errors = true
        $errors = $errors + ["Negative value: " + $item.name]

IF $has_errors THEN
    LOG("Found errors:", $errors)
    RETURN null
ELSE
    RETURN "All valid"
```

---

## Data Validation

### Required Field Validation

**Pattern**: Ensure all required fields are present.

```ovsm
$user = {name: "Alice", email: "alice@example.com"}

// Validate required fields
GUARD $user.name != null ELSE
    ERROR("Name is required")

GUARD $user.email != null ELSE
    ERROR("Email is required")

RETURN "Validation passed"
```

### Type Checking Pattern

**Pattern**: Validate data types before operations.

```ovsm
// Check if value is a number
$value = 42

// Type checking via operations
TRY:
    $test = $value + 0  // Will fail if not a number
CATCH:
    ERROR("Value must be a number")

// Range validation
GUARD $value >= 0 AND $value <= 100 ELSE
    ERROR("Value must be between 0 and 100")
```

### Array Length Validation

**Pattern**: Validate collection size before operations.

```ovsm
$items = [1, 2, 3]

$length = LEN($items)

GUARD $length > 0 ELSE
    ERROR("Array cannot be empty")

GUARD $length <= 100 ELSE
    ERROR("Array too large (max 100)")

// Safe to proceed
$first = $items[0]
```

---

## Performance Patterns

### Early Loop Termination

**Pattern**: Stop processing once result is found.

```ovsm
// Bad: Continues even after finding
$found = false
FOR $item IN $large_array:
    IF $item == $target THEN
        $found = true

// Good: Exits immediately
$found = false
FOR $item IN $large_array:
    IF $item == $target THEN
        $found = true
        BREAK
```

### Minimize Nested Loops

**Pattern**: Flatten logic when possible.

```ovsm
// Bad: O(n²) complexity
$duplicates = []
FOR $i IN [0..LEN($array)]:
    FOR $j IN [$i+1..LEN($array)]:
        IF $array[$i] == $array[$j] THEN
            $duplicates = $duplicates + [$array[$i]]

// Better: Use sets/tracking (when available)
// Or break early when possible
```

### Avoid Repeated Calculations

**Pattern**: Cache computed values.

```ovsm
// Bad: Calculates length repeatedly
FOR $i IN [0..LEN($array)]:
    $item = $array[$i]
    LOG($item)

// Good: Calculate once
$length = LEN($array)
FOR $i IN [0..$length]:
    $item = $array[$i]
    LOG($item)
```

---

## Testing Patterns

### Test Data Setup Pattern

**Pattern**: Create reusable test data structures.

```ovsm
// Setup
$test_users = [
    {id: 1, name: "Alice", role: "admin"},
    {id: 2, name: "Bob", role: "user"},
    {id: 3, name: "Charlie", role: "user"}
]

// Test: Find admin users
$admins = []
FOR $user IN $test_users:
    IF $user.role == "admin" THEN
        $admins = $admins + [$user]

// Assert
$admin_count = LEN($admins)
GUARD $admin_count == 1 ELSE
    ERROR("Expected 1 admin, got " + $admin_count)

RETURN "Test passed"
```

### Edge Case Testing Pattern

**Pattern**: Test boundary conditions.

```ovsm
// Test empty array
$empty = []
GUARD LEN($empty) == 0 ELSE ERROR("Empty array test failed")

// Test single item
$single = [42]
GUARD LEN($single) == 1 ELSE ERROR("Single item test failed")

// Test null handling
$null_value = null
GUARD $null_value == null ELSE ERROR("Null test failed")

RETURN "All edge case tests passed"
```

### Result Validation Pattern

**Pattern**: Validate function outputs.

```ovsm
// Function under test
$result = CALCULATE_SCORE([90, 85, 95])

// Validate type
TRY:
    $test = $result + 0
CATCH:
    ERROR("Result should be a number")

// Validate range
GUARD $result >= 0 AND $result <= 100 ELSE
    ERROR("Result out of valid range")

RETURN "Validation passed"
```

---

## Best Practices Summary

### ✅ DO

- **Use guard clauses** for early validation
- **Break loops early** when result is found
- **Cache computed values** to avoid repeated calculations
- **Use built-in tools** (SUM, MAX, MIN, etc.) when available
- **Validate inputs** before processing
- **Use meaningful variable names** ($user_count, not $x)
- **Add comments** for complex logic

### ❌ DON'T

- **Don't ignore division by zero** - always check denominators
- **Don't mutate loop variables** - can cause infinite loops
- **Don't nest deeply** - extract to separate logic blocks
- **Don't repeat calculations** - store in variables
- **Don't skip error handling** - validate inputs and outputs
- **Don't use magic numbers** - define constants

---

## Real-World Examples

### 1. Data Processing Pipeline

```ovsm
// Load data
$raw_data = [
    {score: 85, status: "active"},
    {score: null, status: "active"},
    {score: 92, status: "inactive"},
    {score: 78, status: "active"}
]

// Filter and transform
$valid_scores = []

FOR $record IN $raw_data:
    // Skip invalid records
    CONTINUE IF $record.status != "active"
    CONTINUE IF $record.score == null

    // Add to results
    $valid_scores = $valid_scores + [$record.score]

// Calculate statistics
$count = LEN($valid_scores)
GUARD $count > 0 ELSE
    RETURN {error: "No valid data"}

$total = SUM($valid_scores)
$average = $total / $count
$max = MAX($valid_scores)
$min = MIN($valid_scores)

RETURN {
    count: $count,
    average: $average,
    max: $max,
    min: $min,
    data: $valid_scores
}
```

### 2. Search and Filter

```ovsm
// Search configuration
$query = "alice"
$min_score = 70
$max_results = 10

// Data
$users = [
    {name: "Alice", score: 85},
    {name: "Bob", score: 65},
    {name: "Alice Smith", score: 90},
    {name: "Charlie", score: 75}
]

// Search
$results = []

FOR $user IN $users:
    // Check score threshold
    CONTINUE IF $user.score < $min_score

    // Check name match (case-insensitive simulation)
    // Note: Real implementation would need lowercase comparison
    CONTINUE IF $user.name != $query AND $user.name != "Alice Smith"

    $results = $results + [$user]

    // Limit results
    BREAK IF LEN($results) >= $max_results

RETURN {
    query: $query,
    found: LEN($results),
    results: $results
}
```

### 3. Validation and Normalization

```ovsm
// Input data
$input = {
    email: "user@example.com",
    age: 25,
    scores: [85, 90, 78]
}

// Validate email
GUARD $input.email != null ELSE
    ERROR("Email is required")

// Validate age
GUARD $input.age >= 18 AND $input.age <= 120 ELSE
    ERROR("Invalid age")

// Validate scores
$score_count = LEN($input.scores)
GUARD $score_count > 0 ELSE
    ERROR("At least one score required")

// Normalize scores
$normalized = []
FOR $score IN $input.scores:
    GUARD $score >= 0 AND $score <= 100 ELSE
        ERROR("Score out of range: " + $score)

    $normalized = $normalized + [$score]

// Calculate average
$total = SUM($normalized)
$average = $total / $score_count

RETURN {
    email: $input.email,
    age: $input.age,
    average_score: $average,
    total_scores: $score_count
}
```

---

## Pattern Selection Guide

| Use Case | Recommended Pattern | Example |
|----------|-------------------|---------|
| Input validation | Guard clauses | `GUARD $x > 0 ELSE ERROR(...)` |
| Array filtering | FOR + CONTINUE IF | `CONTINUE IF $item.status != "active"` |
| Array transformation | FOR + accumulator | `$result = $result + [transform($item)]` |
| Finding first match | FOR + BREAK IF | `BREAK IF $item.id == $target` |
| Error recovery | TRY-CATCH | `TRY: risky() CATCH: fallback()` |
| Simple conditions | Ternary operator | `$x = $y > 0 ? "pos" : "neg"` |
| Complex conditions | IF-ELSE chains | Multi-level classification |
| Statistics | Built-in tools | `SUM()`, `MAX()`, `MIN()`, `MEAN()` |

---

## Additional Resources

- [USAGE_GUIDE.md](../USAGE_GUIDE.md) - Complete language reference
- [HOW_TO_USE.md](../HOW_TO_USE.md) - Getting started guide
- [API Documentation](https://docs.rs/ovsm) - Full API reference
- [Examples](../examples/) - Sample scripts

---

**Need more patterns?** Check the [examples directory](../examples/) for real-world scripts, or consult the [API documentation](https://docs.rs/ovsm) for detailed tool usage.
