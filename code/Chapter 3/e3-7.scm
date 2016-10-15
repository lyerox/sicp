;;3.7

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

(define (make-join acc password joint-password)
  (define (dispatch key mode)
    (cond ((not (eq? key joint-password))
           (error "Incorrect password --MAKE_JOINT" joint-password))
          (else (acc password mode))))
  dispatch)

;;3.8
(define f
  (let ((value 0))
    (lambda (x)
      (cond ((zero? value) value)
            (else (set! value x)
                  0)))))

