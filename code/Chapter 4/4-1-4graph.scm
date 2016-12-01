;;evaluator's data structure

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))
(define (procedure-parameters procedure)
  (cadr procedure))
(define (procedure-body procedure)
  (caddr procedure))
(define (procedure-enviroment procedure)
  (caddr procedure))

;;operations to environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;;frame expression
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))
(define (add-binding-to-frame! var value frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons value (cdr frame))))

;;extend environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variabl" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-enviroment (setup-environment))





























