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
                                       env))))

;;definition of apply
(define )
