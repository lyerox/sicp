;;The Fermat test
;;Fermat's little Theorem: If n is a prime number and a is any positive integer less than n, then a raised to the n th power is congruent to a modulo n.

;;a procedure that computes the exponential of a number modulo another number
(define (square n) (* n n))
(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp)
    (remainder
     (square (expmod base (/ exp 2) m))
     m))
   (else
    (remainder
     (* base (expmod base (- exp 1) m))
     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
