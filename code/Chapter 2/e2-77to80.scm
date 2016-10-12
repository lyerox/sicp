;;2.77
;;2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum --TYPE_TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum --CONTENT" datum))))


;;2.79
(define (equ? x y)
  (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'schem-number))
  (put 'make 'scheme-number
       (lambda (x)
         (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)
         (= x y)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;for ratia number equ? version
(put 'equ? '(rational rational)
     (lambda (x y)
       (and (= (numer x) (numer y))
            (= (denom x) (denom y)))))
;;for complex ...
(put 'equ? '(complex complex)
     (lambda (x y)
       (and (= (real-part x) (real-part y))
            (= (imag-part x) (imag-part y)))))

; the top-level generic procedure
(define (=zero? x) (apply-generic '=zero? x))

; the scheme-number package
(put '=zero? '(scheme-number) zero?)
; the rational package
(define (=zero? x) (zero? (numer x)))
(put '=zero? '(rational) =zero?)
; the complex package
(define (=zero? z1) (zero? (magnitude z1)))
(put '=zero? '(complex) =zero?)
