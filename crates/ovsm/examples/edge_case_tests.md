# Edge Case Tests for QA Runner

## Test 1: Empty code block

```ovsm
```

## Test 2: Syntax error

```ovsm
$x = 10 +
RETURN $x
```

## Test 3: Runtime error

```ovsm
$x = 10 / 0
RETURN $x
```

## Test 4: Code with comments

```ovsm
// This is a comment
$x = 10
$y = 20
RETURN $x + $y  // Should return 30
```

## Test 5: Multiline expressions

```ovsm
$result = 10 +
          20 +
          30
RETURN $result
```

## Test 6: Not implemented feature

```ovsm
PARALLEL {
    $task1 = 1
    $task2 = 2
}
RETURN $task1
```
