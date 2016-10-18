;;3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define l31 (list 'a 'b 'c))
(define l41 (list 'b 'c))
(define l42 (list 'a))

(set-car! l41 l42)
(set-car! (cdr l41) l42)
