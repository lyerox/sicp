;;Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (add1 k)))
              (next (add1 k))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (add1 low) high))))

;;signal-flow common patterns to function
(define (even-fibs n)
  (accumulate
   cons
   '()
   (filter even? (map fibs (enumerate-interval 0 n)))))

(define (list-fibs-squares n)
  (accumulate
   cons
   '()
   (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-square-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(define (salary-of-highest-paid-programmer records)
  (accumulate max 0 (map salary (filter programer? records))))
