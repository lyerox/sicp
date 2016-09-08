(define (Miller-Rabin-test n)
  (define (try-it a)
    (= (expmod a (sub1 n) n) 1))
  (try-it (+ 1 (random (sub1 n)))))


(define (expmod base exp m)
    (cond
     ((= exp 0) 1)
      ((even? exp)
       (let ([square-exp (expmod base (/ exp 2) m)])
         (if (nontrivial-square-root? square-exp m)
            0
            (remainder (square square-exp) m))))
      (else
        (remainder
          (* base (expmod base (sub1 exp) m))
          m))))

(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (sub1 n)))
       (= 1 (remainder (square a) n))))

(define (rabin-prime? n times)
  (cond
   ((= times 0) #t)
   ((Miller-Rabin-test n)
    (rabin-prime? n (sub1 times)))
   (else
    #f)))
