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

;;3.22 exercise
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    ;;<definitions of internal procedures
    (define empty-queue?
      (null? front-ptr))
    (define front-queue
      (if empty-queue?
          (error "FRONT called with an empty queue" front-queue)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))))))
