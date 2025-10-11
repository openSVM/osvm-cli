# Minimal Reproduction

## Test 1: FOR with assignment (works)

```ovsm
FOR $i IN [1..3]:
    $x = 1
RETURN "ok"
```

## Test 2: FOR with IF (fails?)

```ovsm
FOR $i IN [1..3]:
    IF true THEN
        $x = 1
RETURN "ok"
```

## Test 3: Access $i before IF

```ovsm
FOR $i IN [1..3]:
    $before = $i
    IF true THEN
        $x = 1
RETURN "ok"
```

## Test 4: Access $i before AND after IF

```ovsm
FOR $i IN [1..3]:
    $before = $i
    IF true THEN
        $x = 1
    $after = $i
RETURN "ok"
```
