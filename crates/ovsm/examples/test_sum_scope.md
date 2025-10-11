# Test Variable Scope in Loop

## Test: Update global variable from loop

```ovsm
$sum = 0
FOR $i IN [1..5]:
    $sum = $sum + $i
RETURN $sum
```

Expected: 10
