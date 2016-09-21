;;2-17
(define (last-pair-fake items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))


;;2.18 reverse
(define (reverse items)
  (define (iter remainder-items result)
    (if (null? remainder-items)
        result
        (iter (cdr remainder-items)
              (cons (car remainder-items) result))))
  (iter items '()))
;; how does to implement a recursive procedures for reverse method.
