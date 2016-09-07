;; calculate in recursive process
(define (f n)
  (cond
   ((< n 3) n)
   (else (+ (f (sub1 n))
            (* 2 (f (- n 2)))
            (* 3 (f (- n 3)))))))



;; calculate in iteratable process

(define (f-new n)
  (f-iter 2 1 0 n))
(define (f-iter a b c counter)
  (cond
   ((= 0 counter) c)
   (else
    (f-iter (+ a (* 2 b) (* 3 b)) a b
            (- counter 1)))))
