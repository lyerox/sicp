;;let* expression

(define (let*? expr) (tagged-list? expr 'let*))
(define (let*-body expr) (cadd expr))
(define (let*-inits expr) (cadr expr))
(define (let*->nested-lets expr)
  (let ((inits (let*-inits expr))
        (body (let*-body expr)))
    (define (make-lets exprs)
      (if (null? exprs)
          body;; always remain the body
          (list 'let (list (car exprs))
                (make-lets (cdr exprs)))))
    (make-lets inits)))
