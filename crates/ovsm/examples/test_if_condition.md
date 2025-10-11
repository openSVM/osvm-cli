# Test IF condition

## Test 1: Simple IF condition

```ovsm
FOR $i IN [1..10]:
    IF $i > 5 THEN
        RETURN "big"
    RETURN "small"
```

Expected: small

## Test 2: Test range

```ovsm
FOR $i IN [1..3]:
    RETURN $i
```

Expected: 1

## Test 3: IF with comparison

```ovsm
$i = 3
IF $i > 5 THEN
    RETURN "big"
RETURN "small"
```

Expected: small

## Test 4: IF with comparison true

```ovsm
$i = 7
IF $i > 5 THEN
    RETURN "big"
RETURN "small"
```

Expected: big
