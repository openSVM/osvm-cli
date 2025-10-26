# OVSM LISP Dialect Specification

## Overview

OVSM LISP is a Lisp-1 dialect (functions and variables share the same namespace) designed for blockchain automation, with first-class support for Solana RPC operations.

## Syntax Design Principles

1. **S-expressions only** - All code is parenthesized expressions
2. **Prefix notation** - Operators come first: `(+ 1 2)` not `1 + 2`
3. **Explicit blocks** - No indentation-based parsing
4. **Keyword arguments** - Support named parameters: `(log :message "Hello")`
5. **Immutable by default** - Use `(set! var value)` for mutation

## Core Syntax

### Variables

```lisp
;; Constants (immutable)
(const PUMPFUN "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(const MAX_PER_CALL 1000)

;; Variables (mutable with set!)
(define counter 0)
(set! counter (+ counter 1))

;; Let bindings (lexically scoped)
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

### Data Types

```lisp
;; Numbers
42                  ; Integer
3.14                ; Float

;; Strings
"Hello, World!"

;; Booleans
true
false
nil                 ; Null/None

;; Lists (arrays)
'(1 2 3 4 5)
(list 1 2 3 4 5)

;; Ranges
(range 1 11)        ; [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

;; Objects (maps)
{:name "Alice" :age 30}
(object :name "Alice" :age 30)
```

### Control Flow

#### If Expression

```lisp
;; Basic if
(if (== x 0)
    "zero"
    "non-zero")

;; Multi-line branches
(if (>= score 90)
    (do
      (log :message "Excellent!")
      "A")
    (if (>= score 80)
        "B"
        "C"))

;; Cond (better for multiple conditions)
(cond
  ((>= score 90) "A")
  ((>= score 80) "B")
  ((>= score 70) "C")
  (else "F"))
```

#### Loops

```lisp
;; While loop
(while (!= done 0)
  (set! pages (+ pages 1))
  (log :message "Processing...")
  (when (>= pages 10)
    (set! done 1)))

;; For loop
(for (sig signatures)
  (when (>= (.blockTime sig) cutoff)
    (set! total-count (+ total-count 1))))

;; For with index
(for-indexed ((i sig) signatures)
  (log :message (format "Processing {} at index {}" sig i)))

;; Loop with break/continue
(while true
  (let ((item (get-next)))
    (when (null? item)
      (break))
    (when (< (.priority item) 5)
      (continue))
    (process item)))
```

### Functions

```lisp
;; Named function definition
(defn factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Lambda (anonymous function)
(lambda (x) (* x x))
((lambda (x) (* x x)) 5)  ; => 25

;; Function with multiple params
(defn add-three (a b c)
  (+ a b c))

;; Function with keyword arguments
(defn get-sigs (:address addr :limit lim :before bef)
  (getSignaturesForAddress
    :address addr
    :limit lim
    :before bef))
```

### Operators

```lisp
;; Arithmetic
(+ 1 2 3)           ; => 6 (variadic)
(- 10 3)            ; => 7
(* 2 3 4)           ; => 24
(/ 10 2)            ; => 5
(% 10 3)            ; => 1 (modulo)

;; Comparison
(== x y)            ; Equal
(!= x y)            ; Not equal
(< x y)             ; Less than
(<= x y)            ; Less than or equal
(> x y)             ; Greater than
(>= x y)            ; Greater than or equal

;; Logical
(and a b c)         ; Logical AND (short-circuit)
(or a b c)          ; Logical OR (short-circuit)
(not x)             ; Logical NOT

;; Property/field access
(. object field)    ; Get object property
(. sig blockTime)   ; => timestamp
(. user name)       ; => "Alice"
```

### Collections

```lisp
;; List operations
(first '(1 2 3))                ; => 1
(rest '(1 2 3))                 ; => (2 3)
(cons 0 '(1 2 3))              ; => (0 1 2 3)
(append '(1 2) '(3 4))         ; => (1 2 3 4)
(length '(1 2 3 4))            ; => 4
(nth 2 '(a b c d))             ; => c

;; Array indexing
([] array 0)                    ; First element
([] array 2)                    ; Third element (0-indexed)
([] array -1)                   ; Last element (if supported)

;; Combined example
(define nums [10 20 30 40 50])
([] nums 0)                     ; => 10
([] nums 3)                     ; => 40

;; Field/property access
(define obj {:name "Alice" :age 30 :balance 1000})
(. obj name)                    ; => "Alice"
(. obj age)                     ; => 30
(. obj balance)                 ; => 1000

;; Nested access
(define data {:user {:name "Bob" :scores [95 87 92]}})
(. (. data user) name)          ; => "Bob"
([] (. (. data user) scores) 0) ; => 95

;; Map operations
(get {:a 1 :b 2} :a)           ; => 1
(assoc {:a 1} :b 2)            ; => {:a 1 :b 2}
(dissoc {:a 1 :b 2} :b)        ; => {:a 1}
(keys {:a 1 :b 2})             ; => (:a :b)
(values {:a 1 :b 2})           ; => (1 2)
```

### Solana RPC Operations

```lisp
;; Get signatures
(getSignaturesForAddress
  :address "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
  :limit 1000
  :before previous-sig)

;; Get transaction
(getTransaction
  :signature "5j7s6N..."
  :encoding "jsonParsed")

;; Get block
(getBlock
  :slot 123456
  :transactionDetails "full")
```

### Control Flow Helpers

```lisp
;; Do block (sequential execution, returns last value)
(do
  (log :message "Step 1")
  (log :message "Step 2")
  (+ 1 2))  ; => 3

;; When (if without else)
(when (> x 10)
  (log :message "x is large")
  (process x))

;; Unless (inverted when)
(unless (null? data)
  (process data))

;; Guard (assertion)
(guard (!= batch nil) "Batch cannot be nil")
```

## Complete Example: Pumpfun 1-Minute Counter

### Old Python-style (Buggy)

```ovsm
CONST PUMPFUN = "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
CONST MAX_PER_CALL = 1000

$now = NOW()
$cutoff = $now - 60
$before = null
$done = 0
$total_count = 0
$pages = 0

WHILE $done == 0:
    $pages = $pages + 1

    IF $before == null THEN
        $batch = getSignaturesForAddress(address: PUMPFUN, limit: MAX_PER_CALL)
    ELSE
        $batch = getSignaturesForAddress(address: PUMPFUN, limit: MAX_PER_CALL, before: $before)

    $size = COUNT($batch)

    IF $size == 0 THEN
        $done = 1
    ELSE
        FOR $sig IN $batch:
            IF $sig.blockTime >= $cutoff THEN
                $total_count = $total_count + 1
            ELSE
                $done = 1

        IF $done == 0 THEN
            $last_idx = $size - 1
            $last = $batch[$last_idx]
            $before = $last.signature

LOG(message: "===== 1-MINUTE COUNT =====")
LOG(message: "Pages fetched")
LOG(message: $pages)
LOG(message: "Transactions in last 1 minute")
LOG(message: $total_count)

RETURN $total_count
```

### New LISP Syntax (Clean)

```lisp
;; Constants
(const PUMPFUN "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(const MAX_PER_CALL 1000)

;; Main logic
(let ((now (now))
      (cutoff (- now 60))
      (before nil)
      (done false)
      (total-count 0)
      (pages 0))

  ;; Pagination loop
  (while (not done)
    (set! pages (+ pages 1))

    ;; Fetch batch
    (let ((batch (if (null? before)
                     (getSignaturesForAddress
                       :address PUMPFUN
                       :limit MAX_PER_CALL)
                     (getSignaturesForAddress
                       :address PUMPFUN
                       :limit MAX_PER_CALL
                       :before before))))

      (let ((size (length batch)))
        (if (== size 0)
            (set! done true)
            (do
              ;; Count recent transactions
              (for (sig batch)
                (if (>= (.blockTime sig) cutoff)
                    (set! total-count (+ total-count 1))
                    (set! done true)))

              ;; Set up next page
              (when (not done)
                (let ((last-idx (- size 1)))
                  (set! before (.signature ([] batch last-idx))))))))))

  ;; Log results
  (log :message "===== 1-MINUTE COUNT =====")
  (log :message "Pages fetched")
  (log :message pages)
  (log :message "Transactions in last 1 minute")
  (log :message total-count)

  ;; Return count
  total-count)
```

### Even Cleaner LISP with Helper Functions

```lisp
(const PUMPFUN "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(const MAX_PER_CALL 1000)

(defn fetch-batch (before-sig)
  "Fetch a batch of signatures, optionally starting from before-sig"
  (getSignaturesForAddress
    :address PUMPFUN
    :limit MAX_PER_CALL
    :before before-sig))

(defn count-recent (signatures cutoff-time)
  "Count signatures newer than cutoff-time"
  (count (filter (lambda (sig) (>= (.blockTime sig) cutoff-time))
                 signatures)))

(defn count-pumpfun-txs-in-window (seconds)
  "Count pumpfun transactions in the last N seconds"
  (let ((cutoff (- (now) seconds))
        (results '())
        (before nil))

    ;; Collect all recent signatures
    (while true
      (let ((batch (fetch-batch before)))
        (when (empty? batch)
          (break))

        ;; Split batch at cutoff point
        (let ((recent (take-while (lambda (s) (>= (.blockTime s) cutoff))
                                  batch)))
          (when (empty? recent)
            (break))

          (set! results (append results recent))
          (set! before (.signature (last batch))))))

    ;; Return count
    (length results)))

;; Usage
(let ((count (count-pumpfun-txs-in-window 60)))
  (log :message (format "Transactions in last minute: {}" count))
  count)
```

## Syntax Comparison Table

| Feature | Python-style (Old) | LISP (New) |
|---------|-------------------|------------|
| Variable | `$var = 10` | `(define var 10)` or `(let ((var 10)) ...)` |
| Constant | `CONST X = 5` | `(const X 5)` |
| If/Else | `IF x THEN a ELSE b` | `(if x a b)` |
| While | `WHILE cond: body` | `(while cond body)` |
| For | `FOR $x IN list: body` | `(for (x list) body)` |
| Function call | `func(arg: val)` | `(func :arg val)` |
| Arithmetic | `$a + $b` | `(+ a b)` |
| Property | `$obj.prop` | `(.prop obj)` |
| Index | `$arr[$i]` | `([] arr i)` |

## Migration Strategy

1. **Phase 1**: Implement S-expression parser (this document)
2. **Phase 2**: Update evaluator to handle both syntaxes
3. **Phase 3**: Deprecate Python-style with warnings
4. **Phase 4**: Remove Python-style parser (breaking change in v2.0)

## Implementation Notes

### Token Changes Needed

Add to `TokenKind`:
```rust
LeftParen,      // (
RightParen,     // )
Quote,          // '
Backtick,       // `
Comma,          // ,
At,             // @ (for splice)
```

### Parser Architecture

```rust
pub struct SExprParser {
    tokens: Vec<Token>,
    current: usize,
}

impl SExprParser {
    fn parse(&mut self) -> Result<Vec<Expr>> {
        let mut exprs = vec![];
        while !self.is_at_end() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.peek().kind {
            TokenKind::LeftParen => self.parse_list(),
            TokenKind::Quote => self.parse_quoted(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr> {
        self.consume(TokenKind::LeftParen)?;
        let mut elements = vec![];

        while !self.check(&TokenKind::RightParen) && !self.is_at_end() {
            elements.push(self.parse_expr()?);
        }

        self.consume(TokenKind::RightParen)?;
        Ok(Expr::List(elements))
    }
}
```

## Benefits of This Design

1. **No indentation bugs** - Explicit delimiters
2. **Homoiconicity** - Code is data, enables macros
3. **Simple parser** - ~200 lines instead of 2000+
4. **Familiar** - Lisp is a proven 60-year-old design
5. **Powerful** - Can add macros, pattern matching, etc.
6. **Unambiguous** - No parser edge cases

## Standard Library Functions

```lisp
;; Core
(define name value)
(const name value)
(set! name value)
(lambda (args) body)
(defn name (args) body)

;; Control
(if test then else)
(cond (test result) ...)
(when test body)
(unless test body)
(while test body)
(for (var collection) body)
(do expr ...)
(begin expr ...)
(guard test message)

;; Logic
(and a b ...)
(or a b ...)
(not x)
(== a b)
(!= a b)
(< a b)
(<= a b)
(> a b)
(>= a b)

;; Arithmetic
(+ a b ...)
(- a b ...)
(* a b ...)
(/ a b)
(% a b)

;; Lists
(list ...)
(first list)
(rest list)
(cons item list)
(append list1 list2)
(length list)
(nth index list)
(filter pred list)
(map fn list)
(reduce fn init list)
(range start end)

;; Type checks
(null? x)
(number? x)
(string? x)
(list? x)
(object? x)
(function? x)

;; I/O
(log :message msg)
(print expr)
(format template args)

;; Solana RPC
(getSignaturesForAddress :address a :limit l :before b)
(getTransaction :signature s)
(getBlock :slot s)
(now)  ; Current timestamp
```

## Next Steps

See implementation tasks in TODO list above.

## Macro System (Common Lisp Style)

### Overview

OVSM supports Common Lisp-style macros for compile-time code generation. Macros are functions that run at expansion time and generate code to be evaluated.

### Defining Macros

```lisp
(defmacro name (params...) body)
```

Example - Simple `when` macro:
```lisp
(defmacro when (condition & body)
  `(if ,condition
       (do ,@body)
       nil))
```

### Quasiquote System

**Backtick (`)** - Create code template:
```lisp
`(+ 1 2)  ; Template: (+ 1 2)
```

**Comma (,)** - Unquote (evaluate inside template):
```lisp
(define x 10)
`(+ ,x 2)  ; Expands to: (+ 10 2)
```

**Comma-at (,@)** - Unquote-splice (flatten array into template):
```lisp
(define args (list 1 2 3))
`(+ ,@args)  ; Expands to: (+ 1 2 3)
```

### Macro Hygiene

**gensym** - Generate unique symbols to prevent variable capture:
```lisp
(define temp (gensym "temp"))  ; → "temp__0"
(gensym)                        ; → "G__1"
```

Example - Hygienic macro:
```lisp
(defmacro swap (a b)
  (define temp (gensym "temp"))
  `(do
     (define ,temp ,a)
     (set! ,a ,b)
     (set! ,b ,temp)))
```

### Debugging Macros

**macroexpand** - Expand macro once for inspection:
```lisp
(defmacro double (x) `(* ,x 2))
(macroexpand (double 5))  ; Shows expansion
```

### Complete Macro Example

```lisp
;; Define a `unless` macro (opposite of `when`)
(defmacro unless (condition & body)
  `(if (not ,condition)
       (do ,@body)
       nil))

;; Use it
(define x 5)
(unless (> x 10)
  (log :message "x is not greater than 10")
  (set! x 0))
```

### Limitations

- Macro expansion is simplified compared to full Common Lisp
- No `&rest`, `&optional`, or `&key` parameter destructuring yet
- Code-as-data representation is basic (not full s-expressions)
- No macro shadowing or local macros (`macrolet`)

### When to Use Macros

✅ **Good use cases:**
- Domain-specific mini-languages
- Eliminating boilerplate
- Adding new control flow constructs
- Code generation patterns

❌ **Avoid macros for:**
- Simple abstractions (use functions instead)
- Runtime behavior (functions are simpler)
- When hygiene is hard to maintain

