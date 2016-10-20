;;3.1
(define (make-accumulator value)
  (lambda (add-value)
    (set! value (+ value add-value))
    value))

;;3.2
(define (make-monitored f)
  (let ([count-call 0])
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count-call)
            ((eq? input 'reset-count) (begin (set! count-call 0)
                                             count-call))
            (else (begin (set! count-call (add1 count-call))
                         (f input)))))))

;;3.3

(define (make-account blance password)
  (define (with-draw amount)
    (if (>= blance amount)
        (begin (set! blance (- blance amount))
               blance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! blance (+ blance amount)))
  (define (display-wrong-password-message useless-arg)
    (display "Incorrect password")
    (newline))
  (define (password-match? given-password)
    (eq? given-password password))
  (define (dispatch given-password mode)
    (if (password-match? given-password)
        (cond ((eq? mode 'with-draw) with-draw)
              ((eq? mode 'deposit) deposit)
              (else (error "Unknown request -- MAKE_ACCOUNT" mode)))
        display-wrong-password-message))
  dispatch)

;;3.4 
(define (make-account blance password)
  (define (with-draw amount)
    (if (>= blance amount)
        (begin (set! blance (- blance amount))
               blance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! blance (+ blance amount)))
  (define (display-wrong-password-message useless-arg)
    (display "Incorrect password")
    (newline))
  (define (call-the-cops useless-arg)
    (display "Calling the cops, you are foo")
    (newline))
  (define (password-match? given-password)
    (eq? given-password password))
  (let ([bad-passwords 7])
    (define (dispatch given-password mode)
        (if (password-match? given-password)
            (begin (set! bad-passwords 7)
                  (cond ((eq? mode 'with-draw) with-draw)
                        ((eq? mode 'deposit) deposit)
                        (else (error "Unknown request -- MAKE_ACCOUNT" mode))))
            (begin (set! bad-passwords (sub1 bad-passwords))
                  (if (zero? bad-passwords)
                      call-the-cops
                      display-wrong-password-message))))
    dispatch))