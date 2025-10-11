# Minimal Trace Test

## Test: FOR loop with IF, trace all operations

```ovsm
$after = 0
FOR $i IN [1..3]:
    $before = $i
    IF true THEN
        $x = 1
    $after = $i
RETURN $after
```

Expected: 2
