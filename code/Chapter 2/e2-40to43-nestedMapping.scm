;;we can extend the sequence paradigm to include many computations that are commonly expressed using nested loops.
;;

(define (square x) (* x x))
(load "../Chapter 1/e1-24.scm")
(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (add1 a) b))))
;; the combination of mapping and accumulating with append is so common in this sort of program that we will isolate it as a separate procedure;
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap (lambda (i)
                                     (map (lambda (j) (list i j))
                                          (enumerate-interval 1 (sub1 i))))
                                   (enumerate-interval 1 n)))))

;;Nested Mapping are also useful for sequences other than those that enumerate intervals.
;;Suppose we wish to generate all the permutations of a set S
;;For each item x in S, recursively generate the sequence of permulations of S - x, and adjoin x to the front of each one.This yields, for each x in S, the sequence of permutaations of S that begin with x.
;;Combining these sequences for all x gives all the permutations of S:
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= item x))) sequence))

;;2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (sub1 i))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;2.41
;; generate list
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (sub1 i))))
           (enumerate-interval 1 n)))
;; predicate method
(define (triple-sum-equal-to? sum triple)
  (= sum
     (+ (car triple) (cadr triple) (caddr triple))))

(define (triple-sum-equal-to? sum triple)
  (= sum (fold-right + 0 triple)))

;;filter method
(define (remove-triples-not-equal-to sum triples)
  (filter (lambda (current-triple)
            (triple-sum-equal-to? sum current-triple))
          triples))
;;answer
(define (triples-smallThan-n-sumEqual-sum n sum)
  (remove-triples-not-equal-to sum (unique-triples n)))

;;2.42
(define empty-board '())

;;Louis's version ;;2.43
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap (lambda (new-row)
                    (map (lambda (rest-of-queens)
                           (adjoin-position
                            new-row k rest-of-queens))
                         (queen-cols (sub1 k))))
                  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap (lambda (rest-of-queens)
                    (map (lambda (new-row)
                           (adjoin-position
                            new-row k rest-of-queens))
                         (enumerate-interval 1 board-size)))
                  (queen-cols (sub1 k))))))
  (queen-cols board-size))



;;what's the use of k
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

;;what's the use of k
(define (safe? k positions)
  (iter-check (car positions)
              (cdr positions)
              1))

(define (iter-check new-row rest-of-queens step)
  (if (null? rest-of-queens)
      #t
      (let ((current-row (car rest-of-queens)))
        (if (or (= current-row new-row)
                (= (+ step current-row) new-row)
                (= (- current-row step) new-row))
            #f
            (iter-check new-row
                        (cdr rest-of-queens)
                        (add1 step))))))
