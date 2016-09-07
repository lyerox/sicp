(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001 ))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;;Alyssa P.Hacker doesn't see why if needs to be provided as a special form
;;new-if applied to sqrt function as below
(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x) x)))

(define (sqrt-new x)
  (sqrt-iter-new 1.0 x))


(define (new-if predicate then-clause else-clause)
  (cond
   (predicate then-caluse)
   (else else-clause)))
