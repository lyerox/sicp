;;logarithmic number of steps
;; a <- bq + aq + ap
;; b <- bp + aq
;; use as fast-expt procedure
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ;;how to transform new p and q
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p)) ;; how to transform a b
                   (+ (* b p) (* a q))
                   p
                   q
                   (sub1 count)))))
