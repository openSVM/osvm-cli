# Debug FOR + RETURN

## Test 1: Simple FOR with RETURN

```ovsm
FOR $i IN [1..5]:
    RETURN $i
```

## Test 2: FOR with assignment then RETURN

```ovsm
FOR $i IN [1..5]:
    $x = $i
    RETURN $x
```

## Test 3: FOR without RETURN

```ovsm
$result = 0
FOR $i IN [1..5]:
    $result = $i
RETURN $result
```

## Test 4: FOR with IF but no RETURN inside loop

```ovsm
$result = 0
FOR $i IN [1..5]:
    IF $i > 3 THEN
        $result = 100
    ELSE
        $result = $i
RETURN $result
```
