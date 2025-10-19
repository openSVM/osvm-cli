;; Test IF-THEN-ELSE inside WHILE loop - LISP SYNTAX
;; This would work perfectly with S-expression parser!

(let ((done false)
      (count 0))

  (while (not done)
    (log :message "Loop iteration")
    (log :message count)

    (if (== count 0)
        (log :message "First iteration")
        (log :message "Second iteration"))

    (log :message "After IF-THEN-ELSE")  ;; This WILL execute!
    (set! count (+ count 1))

    (when (>= count 2)
      (set! done true)))

  (log :message "Final count")
  (log :message count)
  count)
