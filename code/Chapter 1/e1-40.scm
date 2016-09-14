;;help function in the book
;; derivative procedure
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x))))
