;;Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;; (define (map proc items)
;;   (if (null? items)
;;       '()
;;       (cons (proc (car items))
;;             (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

;;2.21
(define (square-list list)
  (map (lambda (x) (* x x)) list))


(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

;;2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square-list (car things))
                    answer))))
  (iter items '()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square-list (car things))))))
  (iter items '()))

;;2.23
(define (for-each proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))
