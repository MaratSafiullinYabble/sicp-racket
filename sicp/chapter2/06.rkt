#lang sicp

(#%require rackunit)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (sum a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define three (sum one two))

(define four (sum two two))

(define (inc x)
  (+ x 1))
(check-equal? 0 ((zero inc) 0))
(check-equal? 1 (((add-1 zero) inc) 0))
(check-equal? 1 ((one inc) 0))
(check-equal? 2 ((two inc) 0))
(check-equal? 3 ((three inc) 0))
(check-equal? 4 ((four inc) 0))
