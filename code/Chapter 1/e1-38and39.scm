(define (e k)
  (define (N i)
    1)
  (define (D i)
    (if (= 0 (remainder (add1 i) 3))
        (* 2 (div (add1 i) 3))
        1))
  (+ 2
     (con-frac N D k)))


;;e1-39.scm tan-cf

(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
        x
        ((square x))))
  (define (D i)
    (sub1 (* 2 i)))
  (exact->inexact (cont-frac N D k)))
