;;Smothing a function is an important concept in signal processing.
;;If f is a function and dx is some small number, then the smoothed version of f is the function
;; whose value at a point x is the average of f(x-dx), f(x), f(x+dx)

(load "e1-43.scm")

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3)))

;;recursive version
(define (smooth-n-times f n)
  (if (= n 0)
      f
      (smooth (smooth-n-times f (sub1 n)))))


;;iterable version
(define (smooth-n-times f n)
  (define (iter i smoothed-f)
    (if (= 0 i)
        smoothed-f
        (iter (sub1 i) (smooth smoothed-f))))
  (iter n f))


;;use repeated to implement it
(define (smooth-n-times f n)
  (let ((n-times-smooth (repeated smooth n)))
    (n-times-smooth f)))
