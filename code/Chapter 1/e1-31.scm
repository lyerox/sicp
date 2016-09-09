;;like sum generate product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (term a)
    (let ([k (+ (* 2 a) 1)])
      (/ (* (- k 1) (+ k 1))
         (* k k))))
  (product term 1 add1 n))
