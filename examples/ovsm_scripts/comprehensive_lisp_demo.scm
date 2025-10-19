;; Comprehensive OVSM LISP Syntax Demo
;; This file demonstrates all major features of LISP-style OVSM

;; ============================================================================
;; 1. CONSTANTS AND VARIABLES
;; ============================================================================

(const PI 3.14159)
(const MAX_RETRY 5)
(const API_URL "https://api.mainnet-beta.solana.com")

(define counter 0)
(define total 0)
(define result nil)

;; ============================================================================
;; 2. BASIC ARITHMETIC (Variadic Operators)
;; ============================================================================

;; Multiple operands - operators are variadic
(+ 1 2 3 4 5)          ; => 15
(* 2 3 4)              ; => 24
(- 100 10 5)           ; => 85

;; Nested arithmetic
(+ (* 2 3) (/ 10 2))   ; => 11

;; ============================================================================
;; 3. CONDITIONAL EXPRESSIONS
;; ============================================================================

;; Simple if
(if (> counter 10)
    "High"
    "Low")

;; Nested if
(if (> counter 100)
    "Very High"
    (if (> counter 50)
        "Medium"
        "Low"))

;; Cond (better for multiple conditions)
(cond
  ((== counter 0) "Zero")
  ((< counter 10) "Small")
  ((< counter 100) "Medium")
  (else "Large"))

;; When (if without else)
(when (> counter 0)
  (log :message "Counter is positive")
  (set! result "positive"))

;; ============================================================================
;; 4. LOOPS - NO INDENTATION BUGS!
;; ============================================================================

;; While loop with IF-THEN-ELSE inside (THE CRITICAL TEST!)
(while (< counter 10)
  (log :message counter)

  (if (== (% counter 2) 0)
      (log :message "Even")
      (log :message "Odd"))

  (set! counter (+ counter 1)))  ; This executes correctly!

;; For loop
(for (item '(1 2 3 4 5))
  (set! total (+ total item)))

;; Nested loops with conditions
(for (i (range 0 5))
  (for (j (range 0 5))
    (when (== i j)
      (log :message "Diagonal"))))

;; ============================================================================
;; 5. FUNCTIONS AND LAMBDAS
;; ============================================================================

;; Named function
(defn factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Lambda function
(lambda (x) (* x x))

;; Higher-order functions with lambdas
(map (lambda (x) (* x 2)) '(1 2 3 4 5))
; => (2 4 6 8 10)

(filter (lambda (x) (> x 5)) '(1 3 5 7 9 11))
; => (7 9 11)

(reduce (lambda (acc x) (+ acc x)) 0 '(1 2 3 4 5))
; => 15

;; ============================================================================
;; 6. LET BINDINGS (Lexically Scoped)
;; ============================================================================

(let ((x 10)
      (y 20))
  (+ x y))  ; => 30

;; Nested let
(let ((outer 100))
  (let ((inner 50))
    (+ outer inner)))  ; => 150

;; ============================================================================
;; 7. DATA STRUCTURES
;; ============================================================================

;; Lists (quoted)
'(1 2 3 4 5)

;; Lists (constructed)
(list 1 2 3 4 5)

;; Arrays
[1 2 3 4 5]

;; Objects/Maps
{:name "Alice" :age 30 :city "SF"}

;; Nested structures
{:user {:name "Bob" :balance 1000}
 :transactions '(100 200 300)}

;; ============================================================================
;; 8. SOLANA RPC CALLS (Keyword Arguments)
;; ============================================================================

;; Get signatures for address
(getSignaturesForAddress
  :address "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
  :limit 1000
  :before nil)

;; Get transaction
(getTransaction
  :signature "5j7s6N..."
  :encoding "jsonParsed"
  :maxSupportedTransactionVersion 0)

;; Get block
(getBlock
  :slot 123456789
  :transactionDetails "full"
  :encoding "json")

;; ============================================================================
;; 9. COMPLEX REAL-WORLD EXAMPLE
;; ============================================================================

;; Pumpfun transaction counter with pagination
(defn count-recent-transactions (address window-seconds)
  "Count transactions for an address in the last N seconds"

  (let ((cutoff (- (now) window-seconds))
        (before nil)
        (done false)
        (total 0)
        (pages 0))

    ;; Pagination loop
    (while (not done)
      (set! pages (+ pages 1))

      ;; Fetch batch (conditional inside loop - NO BUG!)
      (let ((batch (if (null? before)
                       (getSignaturesForAddress
                         :address address
                         :limit 1000)
                       (getSignaturesForAddress
                         :address address
                         :limit 1000
                         :before before))))

        ;; Process batch
        (if (empty? batch)
            (set! done true)
            (do
              ;; Count recent transactions
              (for (sig batch)
                (when (>= (.blockTime sig) cutoff)
                  (set! total (+ total 1))))

              ;; Check if we've gone too far back
              (let ((last-sig (last batch)))
                (if (< (.blockTime last-sig) cutoff)
                    (set! done true)
                    (set! before (.signature last-sig))))))))

    ;; Return results
    {:total total :pages pages}))

;; Usage
(let ((result (count-recent-transactions
                "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"
                60)))
  (log :message "=== TRANSACTION COUNT ===")
  (log :message (format "Total: {}" (:total result)))
  (log :message (format "Pages: {}" (:pages result)))
  result)

;; ============================================================================
;; 10. PATTERN MATCHING (Future Enhancement)
;; ============================================================================

;; This syntax would support pattern matching in the future:
; (match value
;   ((Just x) x)
;   (Nothing 0))

;; Or destructuring:
; (let (((x y z) '(1 2 3)))
;   (+ x y z))  ; => 6

;; ============================================================================
;; CONCLUSION
;; ============================================================================

;; LISP syntax provides:
;; 1. NO parser bugs with nested blocks
;; 2. Explicit, unambiguous structure
;; 3. Homoiconicity (code is data)
;; 4. Foundation for macros and metaprogramming
;; 5. Cleaner, more consistent syntax

;; The indentation in this file is for readability only - the parser
;; doesn't care about it! Only parentheses matter.
