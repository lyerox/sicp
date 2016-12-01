;;Say, there's another syntax which places the functions name at the end of a list:
;; (1 2 4 +)
;; if we change related functions such as tagged-list? , if? , eval/aplly won't be
;; affacted.

(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))

(define (tagged-list? exp sym)
  (if (pair? exp)
      (let ((last (last-element exp)))
        (eq? last sym))
      #f))

(define (if? exp) (tagged-list? exp 'if)) ;;won't change
;;selectors need small change to select each element of exps
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (cadr exp))

(define (if-alternative exp)
  (if (= (length exp) 4)
      (caddr exp)
      #f))
