(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;;2.24
;;(list 1 (list 2 (list 3 4)))

;;2.25

;;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y);;(1 2 3 4 5 6)
(cons x y) ;;((1 2 3) 4 5 6)
(list x y) ;; ((1 2 3) (4 5 6))

;;2.27

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (cond ((null? items) '())
        ((not (pair? (car items))) (append (deep-reverse (cdr items))
                                           (list (car items))))
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))

(define (deep-reverse items)
  (define (iter remainder-items answer)
    (if (null? remainder-items)
        answer
        (iter (cdr remainder-items)
              (cons (if (pair? (car remainder-items))
                        (iter (car remainder-items) '())
                        (car remainder-items))
                    answer))))
  (iter items '()))


;;2.28
(define (fringe tree)
  (cond ((empty-tree? tree) '())
        ((leaf? tree)
         (list tree))
        (else
         (append (fringe (left-branch tree))
                 (fringe (right-branch tree))))))

(define (empty-tree? tree)
  (null? tree))
(define (leaf? tree)
  (not (pair? tree)))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cdr tree))
