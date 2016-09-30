;;recursive union-set

(define (union-set set1 set2)
  (define (iter lat1 lat2)
    (cond ((null? lat2) lat1)
          ((null? lat1) lat2)
          (else (let ((x1 (car lat1))
                      (x2 (car lat2)))
                  (cond ((= x1 x2) (cons x1 (iter (cdr lat1)
                                                  (cdr lat2))))
                        ((< x1 x2) (cons x1 (iter (cdr lat1)
                                                  lat2)))
                        ((> x1 x2) (cons x2 (iter lat1
                                                  (cdr lat2)))))))))
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (iter list1 list2))))


;;recursive intersection-set
(define (intersection-set set1 set2)
  (define (iter lat1 lat2)
    (if (or (null? lat1) (null? lat2))
        '()
        (let ((x1 (car lat1))
              (x2 (car lat2)))
          (cond ((= x1 x2) (cons x1 (iter (cdr lat1)
                                          (cdr lat2))))
                ((< x1 x2) (iter (cdr lat1) lat2))
                ((> x1 x2) (iter lat1 (cdr lat2)))))))

  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (iter list1 list2))))
