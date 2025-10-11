# Debug IF Condition in FOR

## Test 1: Use $i in IF condition

```ovsm
FOR $i IN [1..5]:
    IF $i > 0 THEN
        $dummy = 1
RETURN "done"
```

## Test 2: Don't use $i in IF condition

```ovsm
FOR $i IN [1..5]:
    IF true THEN
        $x = $i
RETURN "done"
```

## Test 3: Use $i AFTER IF (not in condition)

```ovsm
FOR $i IN [1..5]:
    IF true THEN
        $dummy = 1
    $x = $i
RETURN "done"
```
