(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (begin
            (display-info next (+ 1 step))
            next)
          (try next (+ 1 step)))))
  (try first-guess 1))

(define (display-info guess step)
  (display "Step: ")
  (display step)

  (display "Guess:")
  (display guess)
  (newline))

;;平均阻尼函数
(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (formula x)
  (/ (log 1000) (log x)))
