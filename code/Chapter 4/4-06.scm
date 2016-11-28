;;in eval , add this:

((let? expr) (eval (let->combination expr) env))

;;let expression
(define (let? expr) (tagged-list? expr 'let))
(define (let-vars expr) (map car (cadr expr)))
(define (let-inits expr) (map cadr (cadr expr)))
(define (let-body expr) (cddr expr))

(define (let->combination expr)
  (list (make-lambda (let-vars expr) (let-body expr))
        (let-inits expr)))

;;''Henr Because the initial values for the vars serve as the remainder of the list (cdr) instead of
;;a separate list, the let->conbination should be defined using 'cons' instead of 'list'.

(define (let->combination expr)
  (cons (make-lambda (let-vars expr) (let-body expr))
        (let-inits expr)))
