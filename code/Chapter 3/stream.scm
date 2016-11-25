(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (add1 n))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-evenvs
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-evenvs 100)


(define ones (cons-stream 1 ones))


(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


;;contrast
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))


