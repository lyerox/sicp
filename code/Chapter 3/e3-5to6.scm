;;3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (sub1 trials-remaining) (add1 trials-passed)))
          (else (iter (sub1 trials-remaining) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (random range))))

(define (estimate-integral P? x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda ()
                    (P? (random-in-range x1 x2)
                        (random-in-range y1 y2))))
     4.0))

(define (point-in-area? x y)
  (< (+ (square x) (square y)) 1))

(define (square x) (* x x))

(define (get-pi trials)
  (estimate-integral point-in-area?
                     -1.0
                     1.0
                     -1.0
                     1.0
                     trials))

;;3.6
