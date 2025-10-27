# OVSM Built-in Functions Glossary

Complete reference guide for all 91+ built-in functions in the OVSM LISP interpreter.

**Version:** 1.0.0
**Last Updated:** 2025-10-27
**Test Coverage:** 100% (356/356 tests passing)

---

## Table of Contents

1. [Control Flow](#1-control-flow)
2. [Variables & Assignment](#2-variables--assignment)
3. [Functions & Closures](#3-functions--closures)
4. [Macros & Code Generation](#4-macros--code-generation)
5. [Logical Operations](#5-logical-operations)
6. [Type Predicates](#6-type-predicates)
7. [Assertions](#7-assertions)
8. [Cryptography & Encoding](#8-cryptography--encoding)
9. [String Operations](#9-string-operations)
10. [Math Operations](#10-math-operations)
11. [Collection Operations (Map-Reduce Stack)](#11-collection-operations-map-reduce-stack)
12. [Object Operations](#12-object-operations)
13. [Advanced Features](#13-advanced-features)
14. [Error Handling](#14-error-handling)
15. [Utilities](#15-utilities)
16. [Syntax Reference](#16-syntax-reference)

---

## 1. Control Flow

### `if`
**Signature:** `(if condition then-expr else-expr)`
**Description:** Conditional execution - always returns a value
**Returns:** Value of `then-expr` if condition is truthy, otherwise `else-expr`

```lisp
(if (> x 10)
    "large"
    "small")
```

---

### `when`
**Signature:** `(when condition body...)`
**Description:** Execute body only if condition is true
**Returns:** Result of last expression in body, or `null` if condition is false

```lisp
(when (> balance 1000)
  (log :message "High balance")
  (send-alert))
```

---

### `unless`
**Signature:** `(unless condition body...)`
**Description:** Execute body only if condition is false (inverse of `when`)
**Returns:** Result of last expression in body, or `null` if condition is true

```lisp
(unless (null? data)
  (process-data data))
```

---

### `cond`
**Signature:** `(cond (test1 expr1) (test2 expr2) ... (else default))`
**Description:** Multi-way conditional branching
**Returns:** Result of first matching clause

```lisp
(cond
  ((>= score 90) "A")
  ((>= score 80) "B")
  ((>= score 70) "C")
  (else "F"))
```

---

### `case`
**Signature:** `(case value (pattern1 result1) (pattern2 result2) ... (else default))`
**Description:** Pattern matching by value equality
**Returns:** Result of first matching pattern
**Note:** Supports multiple values in patterns `[val1 val2 ...]`

```lisp
(case day
  (1 "Monday")
  (2 "Tuesday")
  ([6 7] "Weekend")
  (else "Weekday"))
```

---

### `typecase`
**Signature:** `(typecase value (type1 result1) (type2 result2) ... (else default))`
**Description:** Pattern matching by type
**Returns:** Result of first matching type
**Types:** `int`, `float`, `string`, `bool`, `array`, `object`, `function`, `null`

```lisp
(typecase x
  (int "integer")
  (string "text")
  ([float int] "numeric")
  (else "other"))
```

---

### `while`
**Signature:** `(while condition body...)`
**Description:** Loop while condition is true
**Returns:** `null`

```lisp
(define i 0)
(while (< i 10)
  (log :value i)
  (set! i (+ i 1)))
```

---

### `for`
**Signature:** `(for (var collection) body...)`
**Description:** Iterate over arrays, ranges, or sequences
**Returns:** `null`

```lisp
;; Iterate over array
(for (num [1 2 3 4 5])
  (log :value (* num num)))

;; Iterate over range
(for (i (range 1 11))
  (log :value i))
```

---

### `do`
**Signature:** `(do expr1 expr2 ... exprN)`
**Description:** Sequential execution of expressions
**Returns:** Value of last expression
**Alias:** `progn`

```lisp
(do
  (define x 10)
  (set! x (* x 2))
  (+ x 5))  ; Returns 25
```

---

### `prog1`
**Signature:** `(prog1 expr1 expr2 ... exprN)`
**Description:** Execute all expressions, return value of first
**Returns:** Value of first expression

```lisp
(prog1
  (+ 1 2)    ; Returns 3
  (log :message "after"))
```

---

### `prog2`
**Signature:** `(prog2 expr1 expr2 expr3 ... exprN)`
**Description:** Execute all expressions, return value of second
**Returns:** Value of second expression

```lisp
(prog2
  (log :message "first")
  (+ 10 20)  ; Returns 30
  (log :message "third"))
```

---

## 2. Variables & Assignment

### `define`
**Signature:** `(define name value)`
**Description:** Define a new immutable variable (can be shadowed in nested scopes)
**Returns:** The defined value

```lisp
(define x 10)
(define greeting "Hello")
(define factorial (lambda (n) ...))
```

---

### `set!`
**Signature:** `(set! name value)`
**Description:** Mutate an existing variable
**Returns:** The new value
**Error:** Throws error if variable is not defined

```lisp
(define counter 0)
(set! counter (+ counter 1))
```

---

### `setf`
**Signature:** `(setf place value)`
**Description:** Generalized assignment (can set fields, indices, variables)
**Returns:** The new value

```lisp
;; Set variable
(setf x 10)

;; Set object field
(setf (get obj :field) "new value")

;; Set array element
(setf (nth arr 0) 42)
```

---

### `const`
**Signature:** `(const name value)`
**Description:** Define a constant (same as `define`, naming convention)
**Returns:** The defined value

```lisp
(const PI 3.14159)
(const MAX_RETRIES 5)
```

---

### `defvar`
**Signature:** `(defvar name value)`
**Description:** Define a dynamic (special) variable
**Returns:** The defined value
**Note:** Dynamic variables have special scoping in Common Lisp tradition

```lisp
(defvar *debug-mode* false)
```

---

## 3. Functions & Closures

### `defun`
**Signature:** `(defun name (params...) body...)`
**Description:** Define a named function
**Returns:** The function value
**Alias:** `defn`

```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

---

### `lambda`
**Signature:** `(lambda (params...) body...)`
**Description:** Create an anonymous function (closure)
**Returns:** Function value capturing current environment

```lisp
(define square (lambda (x) (* x x)))
(map (lambda (x) (* x 2)) [1 2 3 4 5])
```

---

### `let`
**Signature:** `(let ((var1 val1) (var2 val2) ...) body...)`
**Description:** Create local bindings (parallel - vars can't reference each other)
**Returns:** Value of last expression in body

```lisp
(let ((x 10)
      (y 20))
  (+ x y))  ; Returns 30
```

---

### `let*`
**Signature:** `(let* ((var1 val1) (var2 val2) ...) body...)`
**Description:** Create local bindings (sequential - later vars can reference earlier ones)
**Returns:** Value of last expression in body

```lisp
(let* ((x 10)
       (y (* x 2))    ; y can reference x
       (z (+ x y)))   ; z can reference x and y
  z)  ; Returns 30
```

---

### `flet`
**Signature:** `(flet ((name1 (params...) body...) (name2 ...)) body...)`
**Description:** Define local functions (non-recursive)
**Returns:** Value of last expression in body

```lisp
(flet ((square (x) (* x x))
       (double (x) (* x 2)))
  (+ (square 3) (double 4)))  ; Returns 17
```

---

### `labels`
**Signature:** `(labels ((name1 (params...) body...) (name2 ...)) body...)`
**Description:** Define local recursive functions
**Returns:** Value of last expression in body

```lisp
(labels ((factorial (n)
           (if (<= n 1)
               1
               (* n (factorial (- n 1))))))
  (factorial 5))  ; Returns 120
```

---

## 4. Macros & Code Generation

### `defmacro`
**Signature:** `(defmacro name (params...) body...)`
**Description:** Define a macro for code generation
**Returns:** The macro value

```lisp
(defmacro when (condition &rest body)
  `(if ,condition
       (do ,@body)
       null))
```

---

### `gensym`
**Signature:** `(gensym)` or `(gensym prefix)`
**Description:** Generate a unique symbol for hygienic macros
**Returns:** Unique symbol string

```lisp
(define temp (gensym))     ; "G__1"
(define temp (gensym "X")) ; "X__1"
```

---

### `macroexpand`
**Signature:** `(macroexpand form)`
**Description:** Expand a macro call to see generated code
**Returns:** Expanded form

```lisp
(macroexpand '(when (> x 10) (log :value x)))
```

---

### `eval`
**Signature:** `(eval expression)`
**Description:** Evaluate an expression at runtime
**Returns:** Result of evaluation

```lisp
(eval '(+ 1 2 3))  ; Returns 6
```

---

## 5. Logical Operations

### `not`
**Signature:** `(not expr)`
**Description:** Logical negation
**Returns:** `true` if expr is falsy, `false` otherwise

```lisp
(not true)      ; => false
(not false)     ; => true
(not null)      ; => true
(not 0)         ; => false (0 is truthy in OVSM)
```

---

### `and`
**Signature:** `(and expr1 expr2 ...)`
**Description:** Logical AND (short-circuits on first falsy value)
**Returns:** First falsy value, or last value if all truthy

```lisp
(and true true true)       ; => true
(and true false true)      ; => false
(and (> x 0) (< x 100))    ; Range check
```

---

### `or`
**Signature:** `(or expr1 expr2 ...)`
**Description:** Logical OR (short-circuits on first truthy value)
**Returns:** First truthy value, or last value if all falsy

```lisp
(or false false true)      ; => true
(or null false 42)         ; => 42
(or (null? x) (empty? x))  ; Null or empty check
```

---

## 6. Type Predicates

All type predicates return `true` or `false`.

### `null?`
**Signature:** `(null? value)`
**Description:** Check if value is null

```lisp
(null? null)    ; => true
(null? 0)       ; => false
```

---

### `empty?`
**Signature:** `(empty? collection)`
**Description:** Check if array or string is empty

```lisp
(empty? [])        ; => true
(empty? "")        ; => true
(empty? [1 2 3])   ; => false
```

---

### `int?`
**Signature:** `(int? value)`
**Description:** Check if value is an integer

```lisp
(int? 42)      ; => true
(int? 3.14)    ; => false
```

---

### `float?`
**Signature:** `(float? value)`
**Description:** Check if value is a float

```lisp
(float? 3.14)  ; => true
(float? 42)    ; => false
```

---

### `number?`
**Signature:** `(number? value)`
**Description:** Check if value is either int or float

```lisp
(number? 42)     ; => true
(number? 3.14)   ; => true
(number? "text") ; => false
```

---

### `string?`
**Signature:** `(string? value)`
**Description:** Check if value is a string

```lisp
(string? "hello")  ; => true
(string? 42)       ; => false
```

---

### `bool?`
**Signature:** `(bool? value)`
**Description:** Check if value is a boolean

```lisp
(bool? true)   ; => true
(bool? false)  ; => true
(bool? 1)      ; => false
```

---

### `array?`
**Signature:** `(array? value)`
**Description:** Check if value is an array

```lisp
(array? [1 2 3])  ; => true
(array? "text")   ; => false
```

---

### `object?`
**Signature:** `(object? value)`
**Description:** Check if value is an object

```lisp
(object? {:name "Alice"})  ; => true
(object? [1 2 3])          ; => false
```

---

### `function?`
**Signature:** `(function? value)`
**Description:** Check if value is a function

```lisp
(function? (lambda (x) x))  ; => true
(function? 42)              ; => false
```

---

## 7. Assertions

### `assert`
**Signature:** `(assert condition message)`
**Description:** Assert condition is true, throw error with message if false
**Returns:** `true` if assertion passes
**Error:** Throws error with message if condition is false

```lisp
(assert (> x 0) "x must be positive")
(assert (not (empty? data)) "data cannot be empty")
```

---

### `assert-type`
**Signature:** `(assert-type value expected-type message)`
**Description:** Assert value has expected type
**Returns:** `true` if type matches
**Error:** Throws error with message if type doesn't match

```lisp
(assert-type age "int" "age must be an integer")
(assert-type name "string" "name must be a string")
```

---

## 8. Cryptography & Encoding

### `base58-encode`
**Signature:** `(base58-encode string)`
**Description:** Encode string to Base58 format (Solana address encoding)
**Returns:** Base58-encoded string

```lisp
(base58-encode "HelloSolana")
; => "JxF12TsNv5tEWpp"
```

---

### `base58-decode`
**Signature:** `(base58-decode base58-string)`
**Description:** Decode Base58 string back to original
**Returns:** Decoded string
**Error:** Throws error if invalid Base58 format

```lisp
(base58-decode "JxF12TsNv5tEWpp")
; => "HelloSolana"
```

---

### `base64-encode`
**Signature:** `(base64-encode string)`
**Description:** Encode string to Base64 format
**Returns:** Base64-encoded string

```lisp
(base64-encode "Hello Solana")
; => "SGVsbG8gU29sYW5h"
```

---

### `base64-decode`
**Signature:** `(base64-decode base64-string)`
**Description:** Decode Base64 string back to original
**Returns:** Decoded string
**Error:** Throws error if invalid Base64 format

```lisp
(base64-decode "SGVsbG8gU29sYW5h")
; => "Hello Solana"
```

---

### `hex-encode`
**Signature:** `(hex-encode string)`
**Description:** Encode string to hexadecimal format
**Returns:** Hex-encoded string (lowercase)

```lisp
(hex-encode "solana")
; => "736f6c616e61"
```

---

### `hex-decode`
**Signature:** `(hex-decode hex-string)`
**Description:** Decode hex string back to original
**Returns:** Decoded string
**Error:** Throws error if invalid hex format

```lisp
(hex-decode "736f6c616e61")
; => "solana"
```

---

### `sha256`
**Signature:** `(sha256 string)`
**Description:** Compute SHA-256 cryptographic hash
**Returns:** 64-character hex string

```lisp
(sha256 "test transaction data")
; => "5e304fe75c1bd8dab6e975bcf8f160d95a04b2a1b8119be88feef0e5506561be"
```

---

### `sha512`
**Signature:** `(sha512 string)`
**Description:** Compute SHA-512 cryptographic hash
**Returns:** 128-character hex string

```lisp
(sha512 "test data")
; => "8aa66c657b7ff40d0238f4ce9f1ae951a240374390b2f8e821b7153447ad8778..."
```

---

## 9. String Operations

### `str`
**Signature:** `(str value1 value2 ...)`
**Description:** Convert values to strings and concatenate
**Returns:** Concatenated string

```lisp
(str "Balance: " balance " SOL")
; => "Balance: 1000 SOL"
```

---

### `format`
**Signature:** `(format template args...)`
**Description:** Format string with placeholders
**Returns:** Formatted string

```lisp
(format "User {} has {} points" "Alice" 100)
; => "User Alice has 100 points"
```

---

### `split`
**Signature:** `(split string delimiter)`
**Description:** Split string by delimiter
**Returns:** Array of substrings

```lisp
(split "a,b,c,d" ",")
; => ["a" "b" "c" "d"]
```

---

### `join`
**Signature:** `(join array separator)`
**Description:** Join array elements into string with separator
**Returns:** Joined string

```lisp
(join ["a" "b" "c"] ", ")
; => "a, b, c"
```

---

### `replace`
**Signature:** `(replace string old new)`
**Description:** Replace all occurrences of substring
**Returns:** String with replacements

```lisp
(replace "hello world" "world" "OVSM")
; => "hello OVSM"
```

---

### `trim`
**Signature:** `(trim string)`
**Description:** Remove leading and trailing whitespace
**Returns:** Trimmed string

```lisp
(trim "  hello  ")
; => "hello"
```

---

### `upper`
**Signature:** `(upper string)`
**Description:** Convert string to uppercase
**Returns:** Uppercase string

```lisp
(upper "hello")
; => "HELLO"
```

---

### `lower`
**Signature:** `(lower string)`
**Description:** Convert string to lowercase
**Returns:** Lowercase string

```lisp
(lower "HELLO")
; => "hello"
```

---

## 10. Math Operations

### Arithmetic Operators

All arithmetic operators are **variadic** (accept multiple arguments).

#### `+` (Addition)
**Signature:** `(+ num1 num2 ...)`
**Description:** Add numbers together
**Returns:** Sum

```lisp
(+ 1 2 3 4 5)  ; => 15
```

---

#### `-` (Subtraction)
**Signature:** `(- num1 num2 ...)`
**Description:** Subtract numbers sequentially
**Returns:** Difference

```lisp
(- 100 20 10)  ; => 70
```

---

#### `*` (Multiplication)
**Signature:** `(* num1 num2 ...)`
**Description:** Multiply numbers together
**Returns:** Product

```lisp
(* 2 3 4)  ; => 24
```

---

#### `/` (Division)
**Signature:** `(/ num1 num2 ...)`
**Description:** Divide numbers sequentially
**Returns:** Quotient
**Error:** Division by zero

```lisp
(/ 100 2 5)  ; => 10
```

---

#### `%` (Modulo)
**Signature:** `(% num1 num2)`
**Description:** Get remainder of division
**Returns:** Remainder
**Error:** Division by zero

```lisp
(% 17 5)  ; => 2
```

---

### Comparison Operators

#### `=` (Equal)
**Signature:** `(= val1 val2)`
**Description:** Check equality
**Returns:** Boolean

```lisp
(= 5 5)        ; => true
(= "a" "a")    ; => true
```

---

#### `!=` (Not Equal)
**Signature:** `(!= val1 val2)`
**Description:** Check inequality
**Returns:** Boolean

```lisp
(!= 5 10)  ; => true
```

---

#### `<` (Less Than)
**Signature:** `(< num1 num2)`
**Description:** Check if first is less than second
**Returns:** Boolean

```lisp
(< 5 10)  ; => true
```

---

#### `<=` (Less Than or Equal)
**Signature:** `(<= num1 num2)`
**Description:** Check if first is less than or equal to second
**Returns:** Boolean

```lisp
(<= 5 5)  ; => true
```

---

#### `>` (Greater Than)
**Signature:** `(> num1 num2)`
**Description:** Check if first is greater than second
**Returns:** Boolean

```lisp
(> 10 5)  ; => true
```

---

#### `>=` (Greater Than or Equal)
**Signature:** `(>= num1 num2)`
**Description:** Check if first is greater than or equal to second
**Returns:** Boolean

```lisp
(>= 10 10)  ; => true
```

---

### Advanced Math

### `abs`
**Signature:** `(abs number)`
**Description:** Absolute value
**Returns:** Non-negative number

```lisp
(abs -42)   ; => 42
(abs 3.14)  ; => 3.14
```

---

### `sqrt`
**Signature:** `(sqrt number)`
**Description:** Square root
**Returns:** Square root as float

```lisp
(sqrt 16)   ; => 4.0
(sqrt 2)    ; => 1.414...
```

---

### `pow`
**Signature:** `(pow base exponent)`
**Description:** Exponentiation (base^exponent)
**Returns:** Power result

```lisp
(pow 2 8)    ; => 256
(pow 10 -2)  ; => 0.01
```

---

### `min`
**Signature:** `(min num1 num2 ...)`
**Description:** Find minimum value
**Returns:** Smallest number

```lisp
(min 5 2 8 1)  ; => 1
```

---

### `max`
**Signature:** `(max num1 num2 ...)`
**Description:** Find maximum value
**Returns:** Largest number

```lisp
(max 5 2 8 1)  ; => 8
```

---

## 11. Collection Operations (Map-Reduce Stack)

### Core Higher-Order Functions

### `map`
**Signature:** `(map function array)`
**Description:** Transform each element in array
**Returns:** New array with transformed elements

```lisp
(map (lambda (x) (* x 2)) [1 2 3 4 5])
; => [2 4 6 8 10]
```

---

### `filter`
**Signature:** `(filter function array)`
**Description:** Keep elements that satisfy predicate
**Returns:** New array with filtered elements

```lisp
(filter (lambda (x) (> x 5)) [1 3 5 7 9])
; => [7 9]
```

---

### `reduce`
**Signature:** `(reduce function array initial)`
**Description:** Accumulate array into single value
**Returns:** Accumulated result

```lisp
(reduce + [1 2 3 4 5] 0)
; => 15

(reduce (lambda (acc x) (+ acc x)) [1 2 3] 0)
; => 6
```

---

### Array Accessors

### `first`
**Signature:** `(first array)`
**Description:** Get first element
**Returns:** First element or null if empty
**Alias:** `car` (Common Lisp)

```lisp
(first [1 2 3])  ; => 1
(first [])       ; => null
```

---

### `rest`
**Signature:** `(rest array)`
**Description:** Get all elements except first
**Returns:** Array without first element
**Alias:** `cdr` (Common Lisp)

```lisp
(rest [1 2 3 4])  ; => [2 3 4]
(rest [1])        ; => []
```

---

### `last`
**Signature:** `(last array)`
**Description:** Get last element
**Returns:** Last element or null if empty

```lisp
(last [1 2 3])  ; => 3
(last [])       ; => null
```

---

### `nth`
**Signature:** `(nth array index)`
**Description:** Get element at index (0-based)
**Returns:** Element at index
**Error:** Index out of bounds

```lisp
(nth [10 20 30] 1)  ; => 20
```

---

### `slice`
**Signature:** `(slice array start end)`
**Description:** Extract subarray from start to end (exclusive)
**Returns:** New array with sliced elements

```lisp
(slice [1 2 3 4 5] 1 4)  ; => [2 3 4]
```

---

### Array Constructors

### `cons`
**Signature:** `(cons element array)`
**Description:** Prepend element to array
**Returns:** New array with element at front

```lisp
(cons 0 [1 2 3])  ; => [0 1 2 3]
```

---

### `append`
**Signature:** `(append array1 array2 ...)`
**Description:** Concatenate arrays
**Returns:** New combined array

```lisp
(append [1 2] [3 4] [5])  ; => [1 2 3 4 5]
```

---

### `range`
**Signature:** `(range start end)` or `(range end)`
**Description:** Generate array of integers from start to end (exclusive)
**Returns:** Array of integers
**Note:** `(range n)` is shorthand for `(range 0 n)`

```lisp
(range 1 5)   ; => [1 2 3 4] (5 is excluded!)
(range 5)     ; => [0 1 2 3 4]
```

---

### Map-Reduce Stack (Functional Programming)

### `find`
**Signature:** `(find array predicate)`
**Description:** Find first element satisfying predicate
**Returns:** First matching element or `null`

```lisp
(find [1 2 3 4 5] (lambda (x) (> x 3)))
; => 4
```

---

### `distinct`
**Signature:** `(distinct array)`
**Description:** Remove duplicate elements
**Returns:** New array with unique elements

```lisp
(distinct [1 2 2 3 3 3 4])
; => [1 2 3 4]
```

---

### `flatten`
**Signature:** `(flatten array)`
**Description:** Flatten nested arrays one level
**Returns:** Flattened array

```lisp
(flatten [[1 2] [3 4] [5 6]])
; => [1 2 3 4 5 6]
```

---

### `reverse`
**Signature:** `(reverse array)`
**Description:** Reverse array order
**Returns:** New reversed array

```lisp
(reverse [1 2 3 4 5])
; => [5 4 3 2 1]
```

---

### `some`
**Signature:** `(some array predicate)`
**Description:** Check if any element satisfies predicate
**Returns:** Boolean

```lisp
(some [1 2 3 4] (lambda (x) (> x 3)))
; => true
```

---

### `every`
**Signature:** `(every array predicate)`
**Description:** Check if all elements satisfy predicate
**Returns:** Boolean

```lisp
(every [2 4 6 8] (lambda (x) (= (% x 2) 0)))
; => true
```

---

### `partition`
**Signature:** `(partition array predicate)`
**Description:** Split array into [matching, non-matching]
**Returns:** Two-element array `[matches non-matches]`

```lisp
(partition [1 2 3 4 5 6] (lambda (x) (= (% x 2) 0)))
; => [[2 4 6] [1 3 5]]
```

---

### `take`
**Signature:** `(take n array)`
**Description:** Take first n elements
**Returns:** Array with first n elements

```lisp
(take 3 [1 2 3 4 5])
; => [1 2 3]
```

---

### `drop`
**Signature:** `(drop n array)`
**Description:** Drop first n elements
**Returns:** Array without first n elements

```lisp
(drop 2 [1 2 3 4 5])
; => [3 4 5]
```

---

### `zip`
**Signature:** `(zip array1 array2)`
**Description:** Combine two arrays into array of pairs
**Returns:** Array of two-element arrays

```lisp
(zip [1 2 3] ["a" "b" "c"])
; => [[1 "a"] [2 "b"] [3 "c"]]
```

---

### `compact`
**Signature:** `(compact array)`
**Description:** Remove null values from array
**Returns:** Array without nulls

```lisp
(compact [1 null 2 null 3])
; => [1 2 3]
```

---

### `pluck`
**Signature:** `(pluck array field)`
**Description:** Extract field from array of objects
**Returns:** Array of field values

```lisp
(pluck [
  {:name "Alice" :age 30}
  {:name "Bob" :age 25}
] "name")
; => ["Alice" "Bob"]
```

---

### `group-by`
**Signature:** `(group-by array key-fn)`
**Description:** Group elements by key function result
**Returns:** Object mapping keys to arrays of elements

```lisp
(group-by [
  {:type "A" :val 1}
  {:type "B" :val 2}
  {:type "A" :val 3}
] (lambda (x) (. x type)))
; => {:A [{:type "A" :val 1} {:type "A" :val 3}]
;     :B [{:type "B" :val 2}]}
```

---

### `count-by`
**Signature:** `(count-by array key-fn)`
**Description:** Count elements by key function result
**Returns:** Object mapping keys to counts

```lisp
(count-by ["apple" "banana" "apricot" "berry"]
          (lambda (s) (first s)))
; => {"a" 2 "b" 2}
```

---

### Array Utilities

### `length`
**Signature:** `(length collection)`
**Description:** Get length of array or string
**Returns:** Integer length

```lisp
(length [1 2 3])     ; => 3
(length "hello")     ; => 5
```

---

### `sort`
**Signature:** `(sort array)` or `(sort array comparator)`
**Description:** Sort array (ascending by default)
**Returns:** New sorted array

```lisp
(sort [3 1 4 1 5])
; => [1 1 3 4 5]

(sort [3 1 4] (lambda (a b) (> a b)))
; => [4 3 1]
```

---

## 12. Object Operations

### `get`
**Signature:** `(get object key)` or `(get object key default)`
**Description:** Get value from object by key
**Returns:** Value or default if key not found

```lisp
(get {:name "Alice" :age 30} :name)
; => "Alice"

(get {:x 10} :y 0)
; => 0 (default)
```

---

### `keys`
**Signature:** `(keys object)`
**Description:** Get all keys from object
**Returns:** Array of keys

```lisp
(keys {:name "Alice" :age 30 :active true})
; => ["name" "age" "active"]
```

---

### `merge`
**Signature:** `(merge obj1 obj2 ...)`
**Description:** Merge objects (right-most wins on conflicts)
**Returns:** New merged object

```lisp
(merge {:a 1 :b 2} {:b 3 :c 4})
; => {:a 1 :b 3 :c 4}
```

---

### Field Access

**Syntax:** `(. object field)`
**Description:** Access object field
**Returns:** Field value

```lisp
(define user {:name "Alice" :age 30})
(. user name)   ; => "Alice"
(. user age)    ; => 30
```

---

## 13. Advanced Features

### Multiple Values

### `values`
**Signature:** `(values val1 val2 ...)`
**Description:** Return multiple values
**Returns:** Multiple values container

```lisp
(defun divmod (a b)
  (values (/ a b) (% a b)))
```

---

### `multiple-value-bind`
**Signature:** `(multiple-value-bind (var1 var2 ...) values-expr body...)`
**Description:** Bind multiple return values to variables
**Returns:** Result of body

```lisp
(multiple-value-bind (quot rem) (values 17 5)
  (log :message "Quotient:" :value quot)
  (log :message "Remainder:" :value rem))
```

---

## 14. Error Handling

### `try`
**Signature:** `(try body... catch body...)`
**Description:** Try-catch error handling
**Returns:** Result of body, or catch block if error occurs

```lisp
(try
  (/ 10 x)
  (catch
    (log :message "Division error")
    0))
```

---

### `error`
**Signature:** `(error message)`
**Description:** Throw an error with message
**Returns:** Never returns (throws error)

```lisp
(if (< x 0)
    (error "x must be non-negative")
    (process x))
```

---

## 15. Utilities

### `log`
**Signature:** `(log :message msg)` or `(log :value val)` or `(log :message msg :value val)`
**Description:** Log message or value for debugging
**Returns:** `null`

```lisp
(log :message "Processing transaction")
(log :value balance)
(log :message "Balance:" :value balance)
```

---

### `now`
**Signature:** `(now)`
**Description:** Get current Unix timestamp
**Returns:** Integer timestamp (seconds since epoch)

```lisp
(define cutoff (- (now) 3600))  ; 1 hour ago
```

---

## 16. Syntax Reference

### Data Types

```lisp
;; Numbers
42                     ; Integer
3.14159               ; Float
-100                  ; Negative

;; Strings
"hello world"         ; String
"multi\nline"         ; With escape sequences

;; Booleans
true                  ; Boolean true
false                 ; Boolean false

;; Null
null                  ; Null value

;; Arrays
[1 2 3 4 5]          ; Array of integers
["a" "b" "c"]        ; Array of strings
[]                    ; Empty array

;; Objects
{:name "Alice" :age 30 :active true}  ; Object with keywords
{}                                      ; Empty object

;; Ranges
(range 1 10)          ; [1 2 3 4 5 6 7 8 9] (10 excluded)
(range 5)             ; [0 1 2 3 4]
```

---

### Comments

```lisp
;; Single-line comment
; Also valid

;; Multi-line: use multiple semicolons
;; on each line
```

---

### Keywords

Keywords start with `:` and evaluate to themselves as strings.

```lisp
:name        ; => "name"
:message     ; => "message"
:value       ; => "value"
```

Used for object keys and named parameters:

```lisp
(log :message "Hello" :value 42)
(get obj :name)
```

---

### Quoting

```lisp
;; Quote (prevent evaluation)
'(+ 1 2)              ; => Expression, not evaluated

;; Quasiquote (template with evaluation)
`(list ,x ,(+ 1 2))   ; Evaluate x and (+ 1 2), but not list

;; Unquote (evaluate within quasiquote)
`(+ 1 ,x)             ; Evaluates x

;; Splice (unquote and splice array)
`(list ,@items)       ; Splices items array into list
```

---

### Special Forms

OVSM distinguishes between:
- **Special forms**: Evaluated with special rules (e.g., `if`, `define`, `lambda`)
- **Functions**: All arguments evaluated before application (e.g., `+`, `map`, `filter`)

---

## Performance Notes

### Immutable Data Structures

All OVSM data structures are **immutable by default**:
- Operations return new copies
- Original values are never modified
- Use `set!` to rebind variables

### Optimization Tips

1. **Cache expensive operations:**
   ```lisp
   (define len (length large-array))  ; Calculate once
   (for (i (range 0 len)) ...)        ; Use cached value
   ```

2. **Use map-reduce stack for clarity:**
   ```lisp
   ;; Clear and functional
   (map transform (filter predicate data))
   ```

3. **Early exit in loops:**
   ```lisp
   ;; Break early when found
   (for (item items)
     (if (match? item)
         (do (set! result item)
             (break))
         null))
   ```

---

## Common Patterns

### Filter-Map-Reduce Pipeline

```lisp
(define total
  (reduce +
    (map (lambda (x) (. x amount))
      (filter (lambda (x) (. x active))
        transactions))
    0))
```

---

### Find with Default

```lisp
(define user
  (or (find users (lambda (u) (= (. u id) target-id)))
      {:id target-id :name "Unknown"}))
```

---

### Safe Division

```lisp
(define average
  (if (> count 0)
      (/ total count)
      0))
```

---

### Accumulator Pattern

```lisp
(define sum 0)
(for (num numbers)
  (set! sum (+ sum num)))
sum
```

---

## Migration from Old Syntax

OVSM previously used Python-style syntax. It now uses LISP S-expressions exclusively.

**OLD (Python-style - REMOVED):**
```python
$x = 10
IF $x > 5 THEN
    RETURN "large"
```

**NEW (LISP - CURRENT):**
```lisp
(define x 10)
(if (> x 5)
    "large"
    "small")
```

---

## Testing Your Code

```bash
# Run OVSM script
osvm ovsm run script.ovsm

# Evaluate inline
osvm ovsm eval '(+ 1 2 3)'

# Check syntax
osvm ovsm check script.ovsm

# Interactive REPL
osvm ovsm repl
```

---

## Additional Resources

- **[README.md](README.md)** - Overview and quick start
- **[USAGE_GUIDE.md](USAGE_GUIDE.md)** - Comprehensive usage guide
- **[COMMON_PATTERNS.md](docs/COMMON_PATTERNS.md)** - Idiomatic patterns (needs update for LISP)
- **[API Documentation](https://docs.rs/ovsm)** - Full Rust API reference
- **[Example Scripts](../../examples/ovsm_scripts/)** - Real-world examples

---

## Function Count Summary

| Category | Count | Functions |
|----------|-------|-----------|
| Control Flow | 10 | if, when, unless, cond, case, typecase, while, for, do, prog1, prog2 |
| Variables | 5 | define, set!, setf, const, defvar |
| Functions | 5 | defun, defn, lambda, let, let*, flet, labels |
| Macros | 4 | defmacro, gensym, macroexpand, eval |
| Logical | 3 | not, and, or |
| Type Predicates | 10 | null?, empty?, int?, float?, number?, string?, bool?, array?, object?, function? |
| Assertions | 2 | assert, assert-type |
| Crypto/Encoding | 8 | base58-encode/decode, base64-encode/decode, hex-encode/decode, sha256, sha512 |
| String Ops | 8 | str, format, split, join, replace, trim, upper, lower |
| Math | 12 | +, -, *, /, %, =, !=, <, <=, >, >=, abs, sqrt, pow, min, max |
| Collections | 28 | map, filter, reduce, first, rest, last, nth, slice, cons, append, range, find, distinct, flatten, reverse, some, every, partition, take, drop, zip, compact, pluck, group-by, count-by, length, sort |
| Objects | 3 | get, keys, merge |
| Advanced | 2 | values, multiple-value-bind |
| Error Handling | 2 | try, error |
| Utilities | 2 | log, now |
| **Total** | **91+** | Production-ready blockchain scripting language |

---

**Last Updated:** 2025-10-27
**OVSM Version:** 1.0.0
**Test Coverage:** 100% (356/356 tests passing)

---

*Made with ❤️ by the OpenSVM team*

*OVSM: Where blockchain meets LISP elegance* 🚀
