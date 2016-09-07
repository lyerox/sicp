(define (square n)
  (* n n))

;; Exponentiation
;; fast-expt recursive procedure version
(define (fast-expt b n)
  (cond
   ((= n 0) 1)
   ((even? n) (square (fast-expt (/ n 2))))
   (else (* b (fast-expt (sub1 n))))))

;; Exponentiation
;; fast-expt iterable procedure version, use A as a middle-variable
(define (fast-expt-new b n)
  (fast-expt-iter b 1 n))

(define (fast-expt-iter b A n)
  (cond
   ((= n 0) A)
   ((even? n) (fast-expt-iter (square b) A (/ n 2)))
   (else (fast-expt-iter b (* b A) (sub1 n)))))
