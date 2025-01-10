#lang sicp

(#%require rackunit)

(define (equal? a b)
  (cond
    [(and (null? a) (null? b)) true]
    [(not (or (list? a) (list? b))) (eq? a b)]
    [(and (list? a) (list? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))]
    [else false]))

(check-true (equal? '(this is a list) '(this is a list)))
(check-false (equal? '(this (is a) list) '(this is a list)))
(check-true (equal? '(this (is a) list) '(this (is a) list)))
