;;definition of evaluator
;;core of evaluator is

;;definition of eval
(define (eval exp env)
  (cond ((self-evaluating? exp) enp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown experssion type -- EVAL" exp))))

;;definition of apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unkown procedure type -- APPLY" procedure))))

;;procedure arguments
;;eval use 'list-of-values to generate real arguments.
(define (list-of-values exp env)
  (if (no-operands? exp)
      '()
      (cons (eval (first-operands exp) env)
            (eval (rest-operands exp) env))))
;;conditions
(define (eval-if exp env)
  (if (#t? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;sequence
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;assignment and definition
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;4.1 exercise
;;use let because let expression use eval to eval expression , it has a order
;;evaluation
(define (list-of-values exp env)
  (if (no-operands exp)
      '()
      (let ((left (eval (first-operands exp) env))
            (right (eval (rest-operands exp) env)))
        (cons left right))))


;;4.1.2 expression
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        #f))
(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


;;assignment-eval component
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

;;
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                      (cddr exp))))

;;lambda expression component
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;if expression component
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cadddr exp)))
      (cadddr exp)
      #f))
;;make-if
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;begin expressions
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? seq))
(define (first-exp seq) (car exp))
(define (rest-exps seq) (cdr exp))
;;sequence->exp (use in "cond-if" expressions)
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))
;;now is application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;cond clause expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond-if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


