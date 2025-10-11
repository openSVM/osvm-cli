# Test Simple Comparison

## Test 1: Direct comparison

```ovsm
$x = 1
$y = 5
IF $x > $y THEN
    RETURN "x is bigger"
RETURN "x is smaller"
```

Expected: x is smaller

## Test 2: Literal comparison

```ovsm
IF 1 > 5 THEN
    RETURN "1 is bigger"
RETURN "1 is smaller"
```

Expected: 1 is smaller

## Test 3: Loop variable comparison

```ovsm
FOR $i IN [1..3]:
    IF $i > 2 THEN
        RETURN "found big"
RETURN "all small"
```

Expected: all small
