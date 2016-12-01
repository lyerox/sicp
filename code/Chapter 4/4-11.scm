;; this solution is based on exercise 4.11
;;search in the frame, and manipulate in condition
(define (lookup-binding-in-frame var frame)
         (cond ((null? frame) (cons false '()))
                   ((eq? (car (car frame)) var)
                    (cons true (cdr (car frame))))
                   (else (lookup-binding-in-frame var (cdr frame)))))

 ;; in frame, set var to val
 (define (set-binding-in-frame! var val frame)
         (cond ((null? frame) false)
                   ((eq? (car (car frame)) var)
                    (set-cdr! (car frame) val)
                    true)
                   (else (set-binding-in-frame! var val (cdr frame)))))

 (define (lookup-variable-value var env)
         (if (eq? env the-empty-environment)
                 (error "Unbound variable" var))
                 (let ((result (lookup-binding-in-frame var (first-frame env))))
                         (if (car result)
                                 (cdr result)
                                 (lookup-variable-value var (enclosing-environment env)))))

 (define (set-variable-value! var val env)
         (if (eq? env the-empty-environment)
                 (error "Unbound variable -- SET" var)
                 (if (set-binding-in-frame! var val (first-frame env))
                         true
                         (set-variable-value! var val (enclosing-environment  env)))))

 (define (define-variable! var val env)
         (let ((frame (first-frame env)))
                 (if (set-binding-in-frame! var val frame)
                         true
                         (set-car! env (cons (cons var val) frame)))))

;;high-order abstract

(define (frame-each var frame proc)
  (define (rest frame)
    (make-frame (cdr (frame-variables frame))
                (cdr (frame-values frame))))
  (cond ((null? frame) false)
        ((eq? var (car (frame-variables frame)))
         (proc (frame-values frame)))
        (else (frame-each var (rest frame) proc))))

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((result (frame-each var
                                (first-frame env)
                                (lambda (vals)
                                  (cons true (car vals))))))
        (if result
            (cdr result)
            (lookup-variable-value var (enclosing-environment env))))))


(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((result (frame-each var
                                (first-frame env)
                                (lambda (vals)
                                  (set-car! vals val)
                                  true))))
        (if result
            true
            (set-variable-value! var val (enclosing-environment env))))))

(define (define-variable! var val env)
  (let ((result (frame-each var
                            (first-frame env)
                            (lambda (vals)
                              (set-car! vals val)
                              true))))
    (if result
        true
        (set! (first-frame env)
              (make-frame (cons var (frame-variables frame))
                          (cons val (frame-values frame)))))))
