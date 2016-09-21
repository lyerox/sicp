;;general computational strategy known as iterative improvement.
;;two procedures as arguments: a method for telling whether a guess is good enough
;; and a method for improving a guess.
;;should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough
(define (iterative-improve close-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))


;;redefine fixed-point with help of iterative-improve

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve close-enough? improve) first-guess))

;;redefine sqrt with iterative-improve
(define (sqrt n)
  (define dx 0.000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) dx))
  (define (improve guess)
    (average guess (/ n guess)))
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  ((iterative-improve close-enough? improve) 1.0))
