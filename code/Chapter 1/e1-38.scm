(define (e k)
  (define (N i)
    1)
  (define (D i)
    (if (= 0 (remainder (add1 i) 3))
        (* 2 (div (add1 i) 3))
        1))
  (+ 2
     (con-frac N D k)))
