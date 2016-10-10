;;2.74
(define (make-hq-file division file)
  (cons division file))

(define (file-division hq-file)
  (car hq-file))
(define (original-file hq-file)
  (cdr hq-file))

(define (get-record employee hq-file)
  ((get 'get-record (file-division hq-file))
   employee (original-file hq-file)))

(define (has-record? employee division)
  ((get 'has-record? division) employee))

;;b
(define (make-hq-record division record)
  (cons division record))

(define (record-division hq-record)
  (car hq-record))
(define (original-record hq-record)
  (cdr hq-record))
(define (get-salary hq-record)
  ((get 'get-salary (file-division hq-record))
   (original-record hq-record)))

;;c
(define (find-employee-record employee files)
  (cond ((null? files) (error "FIND-EMPLOYEE_RECORD:No such employee." employee))
        ((has-record? employee (file-division (car files)))
         (get-record employee (car files)))
        (else (find-employee-record employee (cdr files)))))

;;if get return false or the record, doesn't need has-record
(define (find-employee-record employee files)
  (or (accumulate or #f
                  (lambda (file) (get-record employee file))
                  files)
      (error "No such employee" employee)))


;;d
(define (install-ultra-mega-corp)
  (put 'ultra-mega-corp 'get-record ultra-mega-corp-get-record)
  (put 'ultra-mega-corp 'has-record? ultra-mega-corp-has-record?)
  (put 'ultra-mega-corp 'get-salary ultra-mega-corp-get-salary))
