(define (f g)
  (g 2))

;; 当我们坚持要去让解释器求值(f f), 试着展开可以得到等价的表达式

(f f) => (f 2) => (2 2)

;;Non-procedure 2
