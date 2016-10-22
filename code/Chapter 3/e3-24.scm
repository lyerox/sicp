;; ;;two dimension table

;; (define (lookup key-1 key-2 table)
;;   (let ([subtable (assoc key-1 (cdr table))])
;;     (if subtable
;;         (let ((record (assoc key-2 (cdr subtable))))
;;           (if record
;;               (cdr record)
;;               #f))
;;         #f)))

;; (define (assoc key records)
;;   (cond ((null? records) #f)
;;         ((equal? key (caar records)) (car records))
;;         (else (assoc key (cdr records)))))

;; (define (insert! key-1 key-2 value table)
;;   (let ((subtable (assoc key-1 (cdr table))))
;;     (if subtable
;;         (let ((record (assoc key-2 (cdr subtable))))
;;           (if record
;;               (set-cdr! record value)
;;               (set-cdr! subtable (cons (cons key-2 value)
;;                                        (cdr subtable)))))
;;         (set-cdr! table
;;                   (cons (list key-1 (cons key-2 value))
;;                         (cdr table)))))
;;   'ok)

;; ;; procedure version
;; ;; 3.24
;; (define (make-table same-key?)
;;   (let ((local-table (list '*table*)))
;;     (define (lookup key-1 key-2)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (cdr record)
;;                   #f))
;;             #f)))

;;     (define (assoc key records)
;;       (cond ((null? records) #f)
;;             ((same-key? key (caar records)) (car records))
;;             (else (assoc key (cdr records)))))

;;     (define (insert! key-1 key-2 value)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (set-cdr! record value)
;;                   (set-cdr! subtable (cons (cons key-2 value)
;;                                            (cdr subtable)))))
;;             (set-cdr! local-table
;;                       (cons (list key-1 (cons key-2 value))
;;                             (cdr local-table)))))
;;       'ok)

;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             (else (error "Unknown operation -- TABLE" m))))
;;     dispatch))
;;3.25
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (if (null? key-list)
          #f
          (let ((subtable (assoc (car key-list) (cdr local-table))))
            (if subtable
                (if (null? (cdr key-list))
                    (cdr subtable)
                    (let ((local-table subtable))
                      (lookup (cdr key-list))))
                #f))))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert! key-list value)
      (let ((subtable (assoc (car key-list) (cdr local-table))))
        (if subtable
            (if (null? (cdr key-list))
                (set-cdr! subtable value)
                (let ((local-table subtable))
                  (insert! (cdr key-list) value)))
            (set-cdr! local-table
                      (cons (cons key-list value)
                            (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'show) local-table)
            ((eq? m 'set) (lambda (x) (set-cdr! local-table x)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
