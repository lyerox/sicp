(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
   ((= times 0) #t)
   ((fermat-test n)
    (fast-prime? n (sub1 times)))
   (else
    #f)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (sub1 exp) m))
          m))))

(define (next-odd n)
  (if(odd? n)
     (+ 2 n)
     (+ 1 n)))

(define (search-for-primes n count)
  (cond
   ((= count 0) (display "timesecondtick: "))
   ((prime? n)
    (display n)
    (newline)
    (search-for-primes (next-odd n) (sub1 count)))
   (else
    (search-for-primes (next-odd n) count))))

(define (prime? n)
  (fast-prime? n 10))

(define (search-primes n)
  (let ([start-time (real-time)])
    (search-for-primes n 3)
    (- (real-time) start-time)))
