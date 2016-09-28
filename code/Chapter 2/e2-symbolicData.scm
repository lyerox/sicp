(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;2.54

(define (equal? a b)
  (cond ((and (not(pair? a)) (not (pair? b)))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else #f)))

;;考虑到equal?是谓词函数，所有的谓词函数最后都可以将之简化为只用基本谓词表达的形式，即将cond -> ("if not or and" 等等形式)
(define (equal? a b)
  (or (and (not (pair? a))
           (not (pair? b))
           (eq? a b))
      (and (pair? a)
           (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

;;cause eq? function has checked input arguments, so we don't need to check first.
(define (equal? a b)
  (or (eq? a b)
      (and (pair? a)
           (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))
