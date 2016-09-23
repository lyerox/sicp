;; Mapping over trees
;; Just as map is a powerful abstraction for dealing with sequences,
;; map together with recursion is a powerful abstraction for dealing with trees.

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)))))


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


;;2.30 two definition implementation of square-tree
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (scale-tree (car tree))
                    (scale-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;;2.31 abstraction of scale-list and square-tree
(define (square-tree tree)
  (tree-map square tree))
(define (square x) (* x x))
(define (tree-map f tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (tree-map f sub-tree)
               (f sub-tree)))
         tree))

;; Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:
;;2.32
(define (subsets set)
  (if (null? set)
      (list '())
      (let ((rest (subsets (cdr set))))
        (append rest (map (lambda (x)
                            (cons (car set) x))
                          rest)))))
