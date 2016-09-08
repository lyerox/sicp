(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (let ([h (/ (- b a) n)])
    (define (term x)
      (cond
       ((or (= x 0) (= x n)) (f (+ a (* x h))))
       ((even? x)
        (* 2 (f (+ a (* x h)))))
       (else
        (* 4 (f (+ a (* x h)))))))
    (* (/ h 3)
       (sum term 0 add1 n))))
