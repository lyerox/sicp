(define (square n)
  (* n n))

;; exponentiation
;; fast-expt recursive procedure version
(define (fast-expt b n)
  (cond
   ((= n 0) 1)
   ((even? n) (square (fast-expt (/ n 2))))
   (else (* b (fast-expt (sub1 n))))))

;; exponentiation
;; fast-expt iterable procedure version, use a as a middle-variable
(define (fast-expt-new b n)
  (fast-expt-iter b 1 n))

(define (fast-expt-iter b a n)
  (cond
   ((= n 0) a)
   ((even? n) (fast-expt-iter (square b) a (/ n 2)))
   (else (fast-expt-iter b (* b a) (sub1 n)))))
