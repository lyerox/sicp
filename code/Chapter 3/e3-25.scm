;;3.25 multiple exersice

;;helper fold-left

(define (make-table same-key?)

  ;; (define (assoc key records)
  ;;   (cons ((null? records) #f)
  ;;         ((same-key? key (caar records)) (car records))
  ;;         (else (assoc key (cdr records)))))
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup-record records key)
        (if records
            (let ((record (assoc key records)))
              (if record
                  (cdr record)
                  #f))
            #f))
      (fold-left lookup-record (cdr local-table) key-list))

    (define (insert! keys value)
      (define (descend table key)
        (let ((record (assoc key (cdr table))))
          (if record
              record
              (let ((new (cons (list key)
                               (cdr table))))
                (set-cdr! table new)
                (car new)))))
      (set-cdr! (fold-left descend local-table keys)
                value))

    (define (print)
      (define (indent tabs)
        (for-each (lambda (x) (display #\tab )) (iota tabs)))

      (define (print-record rec level)
        (indent level)
        (display (car rec))
        (display ": ")
        (if (list? rec)
            (begin (newline)
                   (print-table rec (add1 level)))
            (begin (display (cdr rec))
                   (newline))))
      (define (print-table table level)
        (if (null? (cdr table))
            (begin (display "no entries")
                   (newline))
            (for-each (lambda (record)
                        (print-record record level))
                      (cdr table))))
      (print-table local-table 0))


    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print)
            (else (error "Missing dispatch method" m))))

    dispatch))

