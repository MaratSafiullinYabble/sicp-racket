#lang sicp

(#%require rackunit)
(#%require racket/math)

(define (squares-of-top a b c)
  (cond
    [(and (<= a b) (<= a c)) (+ (sqr b) (sqr c))]
    [(and (<= b a) (<= b c)) (+ (sqr a) (sqr c))]
    [(and (<= c a) (<= c b)) (+ (sqr a) (sqr b))]))

(check-equal? (squares-of-top 1 1 1) 2)
(check-equal? (squares-of-top 1 2 3) 13)
(check-equal? (squares-of-top 3 2 1) 13)
(check-equal? (squares-of-top 2 3 1) 13)
(check-equal? (squares-of-top 1 1 2) 5)
(check-equal? (squares-of-top 2 1 1) 5)
(check-equal? (squares-of-top 1 2 1) 5)
