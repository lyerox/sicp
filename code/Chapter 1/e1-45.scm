(load "e1-43.scm")
(load "e1-35.scm")


;;
(define (expt base n)
  (if (= n 0)
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

;; the truth is the fact

(define (average-damp-n-times f n)
  ((repeated average-damp n) f))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))

;;define dampet-nth-root function , it returns a procedure that take x as parameters and returns the x's n-rooth damp-times is a params that define the times the average-damp need to take
(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point
     (average-damp-n-times
      (lambda (y)
        (/ x (expt y (sub1 n))))
      damp-times)
     1.0)))

;;shou nian conditions
