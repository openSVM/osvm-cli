# Sample QA Test File

This file contains sample OVSM code blocks for testing.

## Test 1: Basic Arithmetic

```ovsm
$x = 10
$y = 20
RETURN $x + $y
```

## Test 2: GUARD Clause

```ovsm
$value = 5
GUARD $value > 0 ELSE
    RETURN "negative"
RETURN "positive"
```

## Test 3: TRY-CATCH

```ovsm
TRY:
    $result = 10 / 0
CATCH:
    $result = -1
RETURN $result
```

## Test 4: Array Operations

```ovsm
$numbers = [1, 2, 3, 4, 5]
$sum = SUM($numbers)
RETURN $sum
```

## Test 5: Object Creation

```ovsm
$user = {name: "Alice", age: 30}
RETURN $user.name
```

## Test 6: FOR Loop

```ovsm
$total = 0
FOR $i IN [1..5]:
    $total = $total + $i
RETURN $total
```

## Test 7: Nested TRY-CATCH

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

## Test 8: Multiple GUARDs

```ovsm
$a = 10
$b = 20

GUARD $a > 0 ELSE
    RETURN "a invalid"

GUARD $b > $a ELSE
    RETURN "b invalid"

RETURN "all valid"
```
