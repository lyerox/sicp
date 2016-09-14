;;

(load "e1-42.scm")

;;recursive version
(define (repeated f n)
  (lambda (x)
    (if (= 1 n)
        (f x)
        (f ((repeated f (sub1 n)) x)))))


;;iterable version
(define (repeated f n)
  (define (iter i repeated-f)
    (if (= i 1)
        repeated-f
        (iter (sub1 i) (lambda (x)
                         (f (repeated-f x))))))
  (iter n f))

;;use compose , recursive version
(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (sub1 n)))))

;;use compose, iterable version
(define (repeated f n)
  (define (iter i repeated-f)
    (if (= i 1)
        repeated-f
        (iter (sub1 i)
              (compose f repeated-f))))
  (iter n f))

;;Hint: You may find it convenient to use compose  from Excesice 42
;;Yes to this note! May be it's an example of how power of abstraction

;;
