;;2.29 A binary mobile consists of two branches, a left branch and a right branch
(define (make-mobile left right)
  (list left right))
;; A branch is constructed from a length together with a structure , which may be either a number a number or another mobile
(define (make-branch length structure)
  (list length structure))

;;write mobile's selectors and branch's selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


;;total-weight that returns the total weight of a mobile

(define (branch-weight branch)
  (let ((next (branch-structure branch)))
    (if (leaf-on-structure? branch)
        next
        (total-weight next))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (leaf-on-structure? branch)
  (number? (branch-structure branch)))

;; when a mobile fullfill three conditions that can be balanced
(define (mobile-balance? mobile)
  (let ((left (left-branch mobile)) (right (right-branch mobile)))
    (and
     (same-torque? left right)
     (branch-balance? left)
     (branch-balance? right))))

(define (same-torque? left right)
  (= (branch-torque left)
     (branch-torque right)))

;;balanced mobile determinal method
(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balance? branch) ;; when a branch is balanced?
  (if (leaf-on-structure? branch)
      #t
      (mobile-balance? (branch-structure branch))))

;; suppose we change that representation of mobiles so that the constructors are

(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

;;right-branch must be fixed and structure
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))
