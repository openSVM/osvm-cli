# Test WHILE with IF

## Test 1: WHILE with IF, access variable after

```ovsm
$i = 0
WHILE $i < 3:
    $before = $i
    IF true THEN
        $dummy = 1
    $after = $i
    $i = $i + 1
RETURN "ok"
```

## Test 2: Simple WHILE

```ovsm
$i = 0
WHILE $i < 3:
    $x = $i
    $i = $i + 1
RETURN "ok"
```
