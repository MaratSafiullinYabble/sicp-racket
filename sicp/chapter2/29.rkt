#lang sicp

(#%require rackunit)

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (mobile-weight mobile)
  (define (branch-weight branch)
    (if (pair? (branch-structure branch))
        (mobile-weight (branch-structure branch))
        (branch-structure branch)))
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (mobile-balanced? mobile)
  (define (branch-r-momentum branch)
    (if (pair? (branch-structure branch))
        (* (branch-length branch) (mobile-weight (branch-structure branch)))
        (* (branch-length branch) (branch-structure branch))))
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (mobile-balanced? (branch-structure branch))
        true))
  (and (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))
       (= (branch-r-momentum (left-branch mobile)) (branch-r-momentum (right-branch mobile)))))

(define mobile1 (make-mobile (make-branch 1 3) (make-branch 2 2)))
(check-equal? 5 (mobile-weight mobile1))
(check-false (mobile-balanced? mobile1))

(define mobile2
  (make-mobile (make-branch 2 (make-mobile (make-branch 1 3) (make-branch 2 2))) (make-branch 5 2)))
(check-equal? 7 (mobile-weight mobile2))
(check-false (mobile-balanced? mobile2))

(define mobile3 (make-mobile (make-branch 1 4) (make-branch 2 2)))
(check-true (mobile-balanced? mobile3))

(define mobile4
  (make-mobile (make-branch 2 9) (make-branch 3 (make-mobile (make-branch 1 4) (make-branch 2 2)))))
(check-true (mobile-balanced? mobile4))
