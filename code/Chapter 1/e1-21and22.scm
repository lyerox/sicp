(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))
(define (square n) (* n n))
(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= (smallest-divisor n) n))


(smallest-divisor 199)   ;;199
(smallest-divisor 1999)  ;;1999
(smallest-divisor 19999) ;;7

;; exercise 1.22
(define (next-odd n)
  (if(odd? n)
     (+ 2 n)
     (+ 1 n)))

;;Search count primes that are bigger than n

(define (search-for-primes n count)
  (cond
   ((= count 0) (display "are all primes."))
   ((prime? n)
     (display n)
     (newline)
     (search-for-primes (next-odd n) (sub1 count)))
   (else
    (search-for-primes (next-odd n) count))))

;;test procedure runtime
(define (search-primes n)
  (let ((start-time (real-time)))
    (search-for-primes n 3)
    (- (real-time) start-time)))
