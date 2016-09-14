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
  (newton-method (lambda (y) (- (square y) x)) 1.0))

;;Define a procedure cubic that can be used together with the newtons-method procedure in experssions of the form
;; (newtons-method (cubic a b c) 1)

(define (cubic a b c)
  (lambda (y) (+ (cube y)
                 (* a (square y))
                 (* b y)
                 c)))
