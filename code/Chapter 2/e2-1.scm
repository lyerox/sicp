;;2.1
(define (make-rat n d)
  (if (< d 0)
      (cons (-n) (-d))
      (cons n d)))

;;2.2

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))


(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

(define (average a b)
  (/ (+ a b) 2.0))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;2-3
(define (perimeter-retangle r)
  (let ((length (length-of-retangle r))
        (width (width-of-retangle r)))
    (* 2
       (+ length width))))

(define (arca-retangle r)
  (let ((length (length-of-retangle r))
        (width (width-of-retangle r)))
    (* length width)))



;;2-4
(define (cons x y)
  (lambda (m)
    (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;2-5
(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (if (= 0 (remainder z 2))
      (add1 (car (/ z 2)))
      0))
(define (cdr z)
  (if (= 0 (remainder z 3))
      (add1 (cdr (/ z 3)))
      0))
