;;a
;; In eval , insert following code.
((and? expr) (eval-and (and-clauses expr) env))
((or? expr) (eval-or (or-clauses expr) env))

;;In the interpreter, add those functions
(define (and? expr) (tagged-list? expr 'and))
(define (and-clauses expr) (cdr expr))
(define (eval-and exprs env)
  (let ((v (eval (first-exp exprs) env)))
    (cond ((last-exp? exprs)
           (if v v #f))
          (else
           (if v
               (eval-and (rest-exps exprs) env)
               #f)))))

(define (or? expr) (tagged-list? expr 'or))
(define (or-clauses expr) (cdr expr))
(define (eval-or exprs env)
  (let ((v (eval (first-exp exprs) env)))
    (cond ((last-exp? exprs) v)
          (else
           (if v v
               (eval (rest-exps exprs) env))))))


;;b use nexted if implemented "and" and "or" , like this
((and? expr) (eval (and-if expr) env)) ;;add in eval

(define (and-if expr)
  (expand-and-clauses (and-clauses expr)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      (make-if '#t '#t '#f)
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 (expand-and-clauses rest)
                 #f))))

((or? expr) (eval (or-if expr) env)) ;;add in eval
(define (or-if expr)
  (expand-or-clauses (or-clauses expr)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 first
                 (expand-or-clauses rest)))))
