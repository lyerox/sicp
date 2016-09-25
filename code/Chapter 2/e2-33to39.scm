;;2.33 while accumulate is equal to fold-right
(define (map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (fold-right cons seq2 seq1))

(define (length sequence)
  (fold-right (lambda (x y) (add1 y)) 0 sequence))

;;2.34
(define (horner-eval x coeficient-sequence)
  (fold-right (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coeficient-sequence))


;;2.35
(define (count-leaves tree)
  (fold-right + 0 (map (lambda (subtree)
                         (if (pair? subtree)
                             (count-leaves subtree)
                             1))
                       tree)))

;;2.36
(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      '()
      (cons (fold-right op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))
;;2.37

;;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;;

;;2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
;;fold-righ f <==> (f first (f second ... (f last initial)))
;;fold-left f <==> (f (f... (f initial last) last-2 ... second) first)
