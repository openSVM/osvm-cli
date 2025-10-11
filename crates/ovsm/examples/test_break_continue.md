# Test BREAK and CONTINUE

## Test: Simple FOR without BREAK

```ovsm
$sum = 0
FOR $i IN [1..5]:
    $sum = $sum + $i
RETURN $sum
```

## Test: FOR with BREAK

```ovsm
$sum = 0
FOR $i IN [1..10]:
    IF $i > 5 THEN
        BREAK
    $sum = $sum + $i
RETURN $sum
```

## Test: Simpler BREAK

```ovsm
FOR $i IN [1..10]:
    IF $i > 5 THEN
        BREAK
RETURN "done"
```
