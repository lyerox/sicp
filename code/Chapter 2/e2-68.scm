;;2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let ([leafs (symbols tree)])
    (cond ((not (memq symbol leafs))
           (error "No such symbol"))
          ((leaf? tree) '())
          (else (let([left-leafs (symbols (left-branch tree))]
                     [right-leafs (symbols (right-branch tree))])
                  (cond ((memq symbol left-leafs)
                         (cons 0 (encode-symbol symbol (left-branch tree))))
                        ((memq symbol right-leafs)
                         (cons 1 (encode-symbol symbol (right-branch tree))))))))))
