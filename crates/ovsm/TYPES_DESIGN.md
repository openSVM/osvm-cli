# OVSM Type System: The Lisp Way

## You're Right - Lisp Doesn't Have Static Types!

Traditional Lisp is **dynamically typed**. Instead of static type annotations, Lisp uses:

1. **Runtime type checking**
2. **Type predicates** 
3. **Contracts and assertions**
4. **Generic functions with method dispatch**

Let's keep OVSM true to Lisp principles!

## The Lisp Approach to Types

### 1. Runtime Type Predicates (Already Exists!)

```lisp
;; Check types at runtime
(null? x)        ;; Is x null?
(int? x)         ;; Is x an integer?
(string? x)      ;; Is x a string?
(array? x)       ;; Is x an array?
(object? x)      ;; Is x an object?
(function? x)    ;; Is x a function?

;; Get type name
(type-of x)      ;; Returns "int", "string", etc.
```

### 2. Contracts and Assertions

```lisp
;; Assert types at runtime
(define (divide a b)
  (assert (int? a) "a must be an integer")
  (assert (int? b) "b must be an integer")
  (assert (!= b 0) "b cannot be zero")
  (/ a b))

;; Pre and post conditions
(define (get-recent-transactions addr cutoff)
  (pre-condition (string? addr) "addr must be a string")
  (pre-condition (int? cutoff) "cutoff must be an integer")
  
  (define result (getSignaturesForAddress addr {:limit 1000}))
  
  (post-condition (array? result) "result must be an array")
  result)
```

### 3. Pattern Matching (Common Lisp style)

```lisp
;; Match on type and structure
(define (process-value val)
  (cond
    ((null? val) "got null")
    ((int? val) (str "got integer: " val))
    ((string? val) (str "got string: " val))
    ((array? val) (str "got array of length: " (length val)))
    (true "unknown type")))
```

### 4. Multi-Methods (CLOS style)

```lisp
;; Define generic function
(defgeneric add (a b))

;; Specialize for integers
(defmethod add ((a int) (b int))
  (+ a b))

;; Specialize for strings
(defmethod add ((a string) (b string))
  (str a b))

;; Usage - dispatches based on runtime types
(add 1 2)         ;; → 3
(add "hello" " world")  ;; → "hello world"
```

### 5. Struct Types (Common Lisp style)

```lisp
;; Define a struct type
(defstruct Transaction
  signature
  blockTime
  err)

;; Create instance
(define tx (make-Transaction
  :signature "abc123"
  :blockTime 1234567890
  :err null))

;; Access fields
(Transaction-signature tx)  ;; → "abc123"
(Transaction-blockTime tx)  ;; → 1234567890

;; Type check
(Transaction? tx)  ;; → true
```

### 6. Clojure-style Specs (Runtime Validation)

```lisp
;; Define a spec
(spec Transaction
  {:signature string?
   :blockTime int?
   :err (optional? object?)})

;; Validate data against spec
(valid? Transaction tx)  ;; → true or false

;; Explain validation failure
(explain Transaction {:signature 123})
;; → "Field :signature expected string?, got int"
```

## What We Should Actually Implement

Instead of static types, add these **Lisp-idiomatic features**:

### Phase 1: Type Predicates (Easy - 1 week)

Add missing predicates:

```lisp
(int? x)
(float? x)
(number? x)     ;; int or float
(string? x)
(bool? x)
(array? x)
(object? x)
(function? x)
(null? x)       ;; already exists
(empty? x)      ;; already exists
```

### Phase 2: Assertions (Easy - 1 week)

```lisp
;; Basic assertion
(assert condition message)

;; Type assertion
(assert-type value expected-type)

;; Example
(define (divide a b)
  (assert-type a int?)
  (assert-type b int?)
  (assert (!= b 0) "Division by zero")
  (/ a b))
```

### Phase 3: Struct Types (Medium - 2 weeks)

```lisp
;; Define struct
(defstruct User
  name
  age
  email)

;; Constructor
(define u (make-User :name "Alice" :age 30 :email "alice@example.com"))

;; Accessors
(User-name u)    ;; → "Alice"
(User-age u)     ;; → 30

;; Predicate
(User? u)        ;; → true
```

### Phase 4: Pattern Matching (Medium - 2 weeks)

```lisp
;; Match expression
(match value
  [null "got null"]
  [(? int?) (str "integer: " value)]
  [(? string?) (str "string: " value)]
  [(? array?) (str "array length: " (length value))]
  [_ "unknown"])

;; Destructuring
(match transaction
  [{:signature sig :blockTime time}
   (log :message sig :value time)])
```

### Phase 5: Specs (Hard - 3 weeks)

```lisp
;; Define specs
(defspec ::address string?)
(defspec ::timestamp int?)

(defspec ::transaction
  (object {:signature ::address
           :blockTime ::timestamp
           :err (optional? object?)}))

;; Validate
(valid? ::transaction tx)

;; Instrumented functions
(defn get-recent-txs [addr cutoff]
  :args [::address ::timestamp]
  :ret (array? ::transaction)
  
  (getSignaturesForAddress addr {:limit 1000}))
```

## Comparison: Static Types vs Lisp Types

| Feature | Static Types (Non-Lisp) | Lisp Approach |
|---------|------------------------|---------------|
| Checking | Compile-time | Runtime |
| Flexibility | Less flexible | Very flexible |
| Performance | Faster (optimized) | Slower (checks at runtime) |
| Metaprogramming | Limited | Excellent |
| Learning curve | Steeper | Gentler |
| Lisp philosophy | ❌ Not Lisp-like | ✅ True to Lisp |

## Recommendation

**Keep OVSM true to Lisp principles:**

1. ✅ **Add type predicates** (int?, string?, etc.)
2. ✅ **Add assertions** (assert, assert-type)
3. ✅ **Add struct types** (defstruct, make-X, X?)
4. ✅ **Add pattern matching** (match expression)
5. ✅ **Add specs** (Clojure-style runtime validation)

**DON'T add:**
- ❌ Static type annotations like `(define (x : Int) 42)`
- ❌ Compile-time type checking
- ❌ Type inference algorithms

## Benefits of the Lisp Approach

1. **Stays true to Lisp philosophy** - dynamic, flexible, metaprogrammable
2. **Easier to implement** - no complex type inference needed
3. **More flexible** - can change types at runtime
4. **Better for REPL** - no compilation step
5. **Better error messages** - runtime checks provide exact values

## Example: Blockchain Query (Lisp Style)

```lisp
;; Define struct for transactions
(defstruct Transaction
  signature
  blockTime
  err)

;; Function with runtime checks
(define (get-recent-transactions addr cutoff)
  (assert-type addr string?)
  (assert-type cutoff int?)
  
  (define sigs (getSignaturesForAddress addr {:limit 1000}))
  (define result [])
  
  (for (sig sigs)
    (when (>= (. sig blockTime) cutoff)
      (define tx (make-Transaction
        :signature (. sig signature)
        :blockTime (. sig blockTime)
        :err (. sig err)))
      (set! result (APPEND result tx))))
  
  (assert (array? result) "Result must be an array")
  result)

;; Usage
(define addr "pvv4fu1RvQBkKXozyH5A843sp1mt6gTy9rPoZrBBAGS")
(define cutoff (- (now) 3600))
(define txs (get-recent-transactions addr cutoff))

;; Process with pattern matching
(for (tx txs)
  (match tx
    [(? Transaction?)
     (log :message (Transaction-signature tx)
          :value (Transaction-blockTime tx))]))
```

## Conclusion

**You're absolutely right** - Lisp doesn't have static types, and OVSM shouldn't either!

Instead, we should implement:
1. Type predicates (1 week)
2. Assertions (1 week)
3. Struct types (2 weeks)
4. Pattern matching (2 weeks)
5. Specs (3 weeks)

**Total: 9 weeks** - and stays true to Lisp principles!

This is the **Lisp way**: dynamic, flexible, runtime-checked, and perfect for a REPL-based blockchain scripting language.
