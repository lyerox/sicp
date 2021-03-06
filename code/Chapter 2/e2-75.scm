(define (make-from-ma-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle)     y)
          (else "Unknown op -- MAKE_FROM_MAG_ANG" op)))
  (dispatch))

(define (apply-generic op arg) (arg op))
