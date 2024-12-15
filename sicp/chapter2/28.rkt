#lang sicp

(#%require rackunit)

(define (fringe tree)
  (if (null? tree)
      nil
      (let ([first (car tree)]
            [rest (cdr tree)])
        (if (list? first)
            (append (fringe first) (fringe rest))
            (cons first (fringe rest))))))

(define flat-list (fringe (list 1 (list 2.1 2.2 (list 2.31 2.32 2.33)) 3)))

(check-equal? 1 (list-ref flat-list 0))
(check-equal? 2.2 (list-ref flat-list 2))
(check-equal? 2.33 (list-ref flat-list 5))
