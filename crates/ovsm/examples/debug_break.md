# Debug BREAK Bug

## Test 1: Can we access $i in IF?

```ovsm
FOR $i IN [1..5]:
    IF $i > 3 THEN
        $dummy = 1
    RETURN $i
```

## Test 2: Can we access $i after IF?

```ovsm
$result = 0
FOR $i IN [1..5]:
    IF $i > 3 THEN
        $dummy = 1
    $result = $i
RETURN $result
```

## Test 3: Can we use $i in condition?

```ovsm
FOR $i IN [1..5]:
    $test = $i > 3
    RETURN $test
```

## Test 4: Simple BREAK

```ovsm
FOR $i IN [1..5]:
    BREAK
RETURN "done"
```

## Test 5: BREAK with variable reference AFTER

```ovsm
FOR $i IN [1..10]:
    IF $i > 5 THEN
        BREAK
    $nothing = 1
RETURN "done"
```
