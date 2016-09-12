;; recursive process version
(define (filter-accumlate filter? combiner null-value term a next b)
  (cond
    ((> a b) null-value)
    (else
      (let ([rest-terms (filter-accumlate filter
                                          combiner
                                          null-value
                                          term
                                          (next a)
                                          next
                                          b)])
        (if (filter? a)
            (combiner (term a) rest-terms)
            rest-terms)))))
;; iterable process version
(define (filter-accumlate filter? combiner null-value term a next b)
  (define (iter i result)
    (cond
     ((> i b) result)
     ((filter? i)
      (iter (next i)(combiner (term i) result)))
     (else
      (iter (next i) result))))
  (iter a null-value))



(load "e1-28.scm")
;; a -> b all primes sum
(define (sum-prime a b)
  (define (prime? n)
    (rabin-prime? n 10))
  (filter-accumlate prime?
                    +
                    0
                    +
                    a
                    add1
                    b))

(define (product-of-coprimes n)
  (define (coprime? a b)
    (and (< a b)
         (= 1 (gcd a b))))
  (filter-accumlate (lambda (x) (coprime? x n))
                    *
                    1
                    (lambda (x) x)
                    1
                    (lambda (x) (+ x 1))
                    n))
