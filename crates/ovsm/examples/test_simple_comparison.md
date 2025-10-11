# Test Simple Comparison

## Test 1: Direct comparison

```ovsm
$x = 1
$y = 5
IF $x > $y THEN
    RETURN "x is bigger"
ELSE
    RETURN "x is smaller"
```

Expected: x is smaller

## Test 2: Literal comparison

```ovsm
IF 1 > 5 THEN
    RETURN "1 is bigger"
ELSE
    RETURN "1 is smaller"
```

Expected: 1 is smaller

## Test 3: Loop variable comparison

```ovsm
$result = "all small"
FOR $i IN [1..3]:
    IF $i > 2 THEN
        $result = "found big"
        BREAK
RETURN $result
```

Expected: all small
