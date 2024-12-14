#lang sicp

(#%require rackunit)
(#%require racket/math)

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

(define s (make-segment (make-point 0 0) (make-point 2 -2)))
(define p (midpoint-segment s))
(check-equal? 1 (x-point p))
(check-equal? -1 (y-point p))
