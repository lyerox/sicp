;;recursive procedure  version
(define (accumlate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumlate combiner null-value term (next a) next b))))


(define (sum term a next b)
  (accumlate + 0 term a next b))

(define (product term a next b)
  (accumlate * 1 term a next b))

;;iterable procedure version
