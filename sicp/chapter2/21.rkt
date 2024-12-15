#lang sicp

(#%require rackunit)
(#%require racket/math)

(define (square-list items)
  (if (null? items)
      nil
      (cons (sqr (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map sqr items))

(define sq1 (square-list (list 2 3)))
(check-equal? 4 (list-ref sq1 0))
(check-equal? 9 (list-ref sq1 1))

(define sq2 (square-list-map (list 2 3)))
(check-equal? 4 (list-ref sq2 0))
(check-equal? 9 (list-ref sq2 1))
