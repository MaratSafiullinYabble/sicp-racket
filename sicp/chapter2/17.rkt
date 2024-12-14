#lang sicp

(#%require rackunit)

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(check-equal? 1 (car (last-pair (list 1))))
(check-equal? 7 (car (last-pair (list 8 6 7))))
