;;2.7
(define (make-interval a b)
  (cons a b))
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))


;;2.8

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))
        )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;;2.9

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define interval1 (make-interval 2 5))
(define interval2 (make-interval 3 4))

;; (= (+ (width interval1) (width interval2))
;;    (width (add-interval interval1 interval2)))

;; (= (* (width interval1) (width interval2))
;;    (width (mul-interval interval1 interval2)))


;;2.10
(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "div a interval across 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
;;2.11
