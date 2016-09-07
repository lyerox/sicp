;;calculate pascal triangle's all number
;; recursive process version
(define (pascal row column)
  (cond
   ((or (< row column) (= column 0)) 'wrong)
   ((= 1 row) 1)
   ((= row column) 1)
   (else
    (+ (pascal (- row 1) (- column 1))
       (pascal (- row 1) column)))))

;; iterable process version
;; 有很多种解法，都是基于pascal的递推公式推到的，组合公式(x + y)^n 的展开项系数即是。
;;
