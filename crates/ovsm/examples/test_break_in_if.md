# Test BREAK inside IF

## Test: BREAK inside IF should work

```ovsm
$sum = 0
FOR $i IN [1..10]:
    IF $i > 5 THEN
        BREAK
    $sum = $sum + $i
RETURN $sum
```

Expected: 15
