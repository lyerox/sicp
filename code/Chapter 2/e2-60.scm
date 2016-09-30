(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (union-set (cdr set1)
                         (adjoin-set (car set1) set2)))))

(define (intersection-set set1 set2)
  (define (iter set result)
    (if (or (null? set) (null? set2))
        result
        (let ((current-element (car set))
              (remain-element (cdr set)))
          (if (and (element-of-set? current-element set2)
                   (not (element-of-set? current-element result)))
              (iter remain-element (adjoin-set current-element result))
              (iter remain-element result)))))
  (iter set1 '()))
