#lang sicp

(#%require rackunit)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(check-equal? 0 (car (cons 0 1)))
(check-equal? 1 (cdr (cons 0 1)))
