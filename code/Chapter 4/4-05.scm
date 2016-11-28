(define (extend-cond-syntax? clause) (eq? (cadr clause) '=>))
(define (extend-cond-test clause) (car clause))
(define (extend-cond-recipient clause) (caddr clause))

(define (cond-if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
      '#f
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence-exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (extend-cond-syntax? first)
                (make-application (make-lambda '(_cond_parameter)
                                               (make-if _cond_parameter
                                                        (make-application (extend-cond-actions
                                                                           first)
                                                                          _cond_parameter)
                                                        (expand-clauses rest)))
                                  (cond-predicate first))
                (make-if (cond-predicate first)
                         (sequence-exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (extend-cond-actions clauses)
  (caddr clauses))

(define (make-application function parameters)
  (cons function parameter))
