;;3.21 exercise
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

;;selectors
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-queue queue)))
              queue)))

;;3.22 exercise bn
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    ;;<definitions of internal procedures
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?) (set! front-ptr new-pair)
                            (set! rear-ptr new-pair)
                            front-ptr)
              (else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)
                    front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (display "DELETE called with an empty queue")
             (newline))
            (else (set! front-ptr (cdr front-ptr))
                  front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "Unknown operation --DISPATCH" m))))
    dispatch))
