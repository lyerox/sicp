(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (sub1 b)))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

;; 1.17 excercise recursive process
(define (fast-multiple a b)
  (cond
   ((= b 0) 0)
   ((even? b) (double (fast-multiple a (halve b))))
   (else
    (+ a (fast-multiple a (sub1 b))))))


;;iterable procedure version, 1.18 excercise
(define (fast-mulip a b)
  (fast-mul-iter a b 0))
(define (fast-mul-iter a b cache)
  (cond
   ((= b 0) cache)
   ((even? b) (fast-mul-iter (double a) (halve b) cache))
   (else
    (fast-mul-iter a (sub1 b) (+ a cache)))))
