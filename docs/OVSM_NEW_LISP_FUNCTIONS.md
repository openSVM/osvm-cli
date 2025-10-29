# New LISP Functions in OVSM

This document describes the new Common Lisp-compatible functions added to the OVSM LISP interpreter.

## Overview

The following functions have been added to provide full Common Lisp compatibility:

### Type Predicates
- `IS-ARRAY?` - Check if value is an array
- `IS-OBJECT?` - Check if value is an object
- `IS-STRING?` - Check if value is a string
- `IS-NUMBER?` - Check if value is a number (int or float)
- `IS-BOOL?` - Check if value is a boolean
- `IS-NULL?` - Check if value is null

### List Accessors (Common Lisp Style)
- `CAR` - Get first element of list (same as FIRST)
- `CDR` - Get all elements except first (same as REST)
- `REST` - Alias for CDR
- `CADR` - Get second element (CAR of CDR)
- `CDDR` - Get all elements except first two (CDR of CDR)
- `CAAR` - Get first element of first element (CAR of CAR)
- `CDAR` - Get rest of first element (CDR of CAR)

### List Constructors
- `CONS` - Construct new list by prepending an element
- `LIST` - Create a list from arguments
- `LENGTH` - Get length of array, string, or object

### Utility Functions
- `TYPEOF` - Returns the type of a value as a string
- `KEYS` - Returns the keys of an object as an array

---

## Type Predicates

### IS-ARRAY?

Check if a value is an array.

**Syntax:**
```lisp
(is-array? value)
```

**Examples:**
```lisp
(is-array? [1 2 3])        ;; → true
(is-array? "hello")        ;; → false
(is-array? {:key "value"}) ;; → false
```

### IS-OBJECT?

Check if a value is an object (key-value map).

**Syntax:**
```lisp
(is-object? value)
```

**Examples:**
```lisp
(is-object? {:name "Alice" :age 30}) ;; → true
(is-object? [1 2 3])                 ;; → false
(is-object? "hello")                 ;; → false
```

### IS-STRING?

Check if a value is a string.

**Syntax:**
```lisp
(is-string? value)
```

**Examples:**
```lisp
(is-string? "hello")  ;; → true
(is-string? 42)       ;; → false
(is-string? [1 2 3])  ;; → false
```

### IS-NUMBER?

Check if a value is a number (either integer or float).

**Syntax:**
```lisp
(is-number? value)
```

**Examples:**
```lisp
(is-number? 42)      ;; → true
(is-number? 3.14)    ;; → true
(is-number? "hello") ;; → false
(is-number? true)    ;; → false
```

### IS-BOOL?

Check if a value is a boolean.

**Syntax:**
```lisp
(is-bool? value)
```

**Examples:**
```lisp
(is-bool? true)   ;; → true
(is-bool? false)  ;; → true
(is-bool? 1)      ;; → false
(is-bool? "yes")  ;; → false
```

### IS-NULL?

Check if a value is null.

**Syntax:**
```lisp
(is-null? value)
```

**Examples:**
```lisp
(is-null? null)  ;; → true
(is-null? 0)     ;; → false
(is-null? false) ;; → false
(is-null? "")    ;; → false
```

---

## List Accessors

### CAR

Get the first element of a list. This is the traditional Common Lisp name for `FIRST`.

**Syntax:**
```lisp
(car list)
```

**Examples:**
```lisp
(car [10 20 30])  ;; → 10
(car [5])         ;; → 5
(car [])          ;; → Error: EmptyCollection
```

**Errors:**
- `EmptyCollection` - if the list is empty

### CDR

Get all elements except the first (the "rest" of the list). This is the traditional Common Lisp name for `REST`.

**Syntax:**
```lisp
(cdr list)
```

**Examples:**
```lisp
(cdr [10 20 30 40])  ;; → [20, 30, 40]
(cdr [5])            ;; → []
(cdr [])             ;; → []
```

### REST

Alias for CDR - get all elements except the first.

**Syntax:**
```lisp
(rest list)
```

**Examples:**
```lisp
(rest [10 20 30])  ;; → [20, 30]
(rest [5])         ;; → []
```

### CADR

Get the second element of a list (equivalent to `(car (cdr list))`).

**Syntax:**
```lisp
(cadr list)
```

**Examples:**
```lisp
(cadr [10 20 30])  ;; → 20
(cadr [5 10])      ;; → 10
(cadr [5])         ;; → Error: IndexOutOfBounds
```

**Errors:**
- `IndexOutOfBounds` - if the list has fewer than 2 elements

### CDDR

Get all elements except the first two (equivalent to `(cdr (cdr list))`).

**Syntax:**
```lisp
(cddr list)
```

**Examples:**
```lisp
(cddr [10 20 30 40])  ;; → [30, 40]
(cddr [10 20])        ;; → []
(cddr [10])           ;; → []
```

### CAAR

Get the first element of the first element (equivalent to `(car (car list))`).

**Syntax:**
```lisp
(caar nested-list)
```

**Examples:**
```lisp
(caar [[1 2 3] [4 5 6]])  ;; → 1
(caar [[10] [20]])        ;; → 10
```

**Errors:**
- `EmptyCollection` - if the outer list or first inner list is empty
- `TypeError` - if the first element is not an array

### CDAR

Get the rest of the first element (equivalent to `(cdr (car list))`).

**Syntax:**
```lisp
(cdar nested-list)
```

**Examples:**
```lisp
(cdar [[1 2 3] [4 5 6]])  ;; → [2, 3]
(cdar [[10] [20]])        ;; → []
```

**Errors:**
- `EmptyCollection` - if the outer list is empty
- `TypeError` - if the first element is not an array

---

## List Constructors

### CONS

Construct a new list by prepending an element to an existing list.

**Syntax:**
```lisp
(cons element list)
```

**Examples:**
```lisp
(cons 0 [1 2 3])        ;; → [0, 1, 2, 3]
(cons "a" ["b" "c"])    ;; → ["a", "b", "c"]
(cons 5 [])             ;; → [5]
```

**Parameters:**
- `element` - The value to prepend
- `list` - The existing list

### LIST

Create a new list from the provided arguments.

**Syntax:**
```lisp
(list arg1 arg2 arg3 ...)
```

**Examples:**
```lisp
(list 1 2 3)           ;; → [1, 2, 3]
(list "a" "b" "c")     ;; → ["a", "b", "c"]
(list)                 ;; → []
(list 1)               ;; → [1]
```

**Parameters:**
- Accepts any number of arguments

### LENGTH

Get the length of a collection (array, string, or object).

**Syntax:**
```lisp
(length collection)
```

**Examples:**
```lisp
(length [1 2 3 4 5])         ;; → 5
(length "hello")             ;; → 5
(length {:a 1 :b 2 :c 3})    ;; → 3
(length [])                  ;; → 0
```

**Errors:**
- `TypeError` - if the argument is not an array, string, or object

---

## Utility Functions

### TYPEOF

Returns the type of a value as a string.

**Syntax:**
```lisp
(typeof value)
```

**Return Values:**
- `"null"` - for null values
- `"bool"` - for booleans
- `"int"` - for integers
- `"float"` - for floating-point numbers
- `"string"` - for strings
- `"array"` - for arrays
- `"object"` - for objects
- `"function"` - for functions
- `"range"` - for ranges
- `"macro"` - for macros

**Examples:**
```lisp
(typeof [1 2 3])           ;; → "array"
(typeof "hello")           ;; → "string"
(typeof 42)                ;; → "int"
(typeof 3.14)              ;; → "float"
(typeof {:key "value"})    ;; → "object"
(typeof true)              ;; → "bool"
(typeof null)              ;; → "null"
```

### KEYS

Returns the keys of an object as an array of strings.

**Syntax:**
```lisp
(keys object)
```

**Examples:**
```lisp
(keys {:name "Alice" :age 30})     ;; → ["name", "age"]
(keys {:a 1 :b 2 :c 3})            ;; → ["a", "b", "c"]
(keys {})                          ;; → []
(keys [1 2 3])                     ;; → [] (not an object)
```

**Note:** If the argument is not an object, returns an empty array.

---

## Complete Example

Here's a comprehensive example using many of the new functions:

```lisp
(do
  ;; Create test data
  (define data [[1 2 3] [4 5 6] [7 8 9]])
  (define person {:name "Alice" :age 30 :city "NYC"})

  ;; Type checking
  (if (is-array? data)
      (log :message "data is an array")
      (log :message "data is not an array"))

  (if (is-object? person)
      (log :message "person is an object")
      (log :message "person is not an object"))

  ;; List manipulation
  (define first-row (car data))           ;; [1 2 3]
  (define rest-rows (cdr data))           ;; [[4 5 6] [7 8 9]]
  (define first-elem (caar data))         ;; 1
  (define rest-first (cdar data))         ;; [2, 3]

  ;; List construction
  (define new-row [0 0 0])
  (define with-header (cons new-row data)) ;; [[0 0 0] [1 2 3] [4 5 6] [7 8 9]]

  ;; Get information
  (define row-count (length data))        ;; 3
  (define person-fields (keys person))    ;; ["name", "age", "city"]
  (define data-type (typeof data))        ;; "array"

  ;; Return summary
  {:first-row first-row
   :first-elem first-elem
   :row-count row-count
   :person-fields person-fields
   :data-type data-type}
)
```

---

## Migration Guide

If you have existing OVSM code that uses evaluator built-ins like `first` and `rest`, these now work as both:
1. **Special forms** (handled by the evaluator) - used with `(first list)` syntax
2. **Tool functions** - can be referenced as variables

The new tool-based approach allows for more flexibility:

```lisp
;; Both work the same way:
(first [1 2 3])          ;; Evaluator built-in
(car [1 2 3])            ;; Tool function (same as first)

;; But tools can be used in more contexts:
(define my-func car)     ;; Now possible with tools
(my-func [1 2 3])        ;; → 1
```

---

## Notes

1. All type predicates use the question mark (`?`) naming convention from Scheme/Lisp
2. List accessor names (CAR, CDR, etc.) follow Common Lisp conventions
3. All functions are case-insensitive (you can use `car`, `CAR`, or `Car`)
4. The hyphen in predicate names (e.g., `is-array?`) is part of the identifier
5. Empty collections return `false` for all predicates (except `is-null?`)

---

## See Also

- [OVSM LISP Syntax Specification](../OVSM_LISP_SYNTAX_SPEC.md)
- [OVSM Usage Guide](../crates/ovsm/USAGE_GUIDE.md)
- [OVSM README](../crates/ovsm/README.md)
