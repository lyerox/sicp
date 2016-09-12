;;recursive version
(define (con-frac N D k)
  (define (cf i)
    (if (= k i)
        (/ (N k) (D k))
        (/ (N i)
           (+ (D i) (cf (+ i 1))))))
  (cf 1))


;;iterable version
(define (con-frac N D k)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (sub1 i)
              (/ (N i)
                 (+ (D i) result)))))
  (iter k (/ (N k) (D k))))

;;use con-frac to define golden-ration

(define (golden-ratio k)
  (+ 1
     (con-frac (lambda (i) 1.0)
               (lambda (i) 1.0)
               k)))
