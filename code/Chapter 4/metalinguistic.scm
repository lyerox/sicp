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
  (cond ((primative-procedure? procedure)
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
  (if (true? (eval (if-predicate exp) env))
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
