#lang sicp

(#%require rackunit)
(#%require racket/math)

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rect b-l t-r)
  (cons b-l t-r))

(define (rect-length r)
  (- (x-point (cdr r)) (x-point (car r))))

(define (rect-width r)
  (- (y-point (cdr r)) (y-point (car r))))

(define (rect-square r)
  (* (rect-length r) (rect-width r)))

(define (rect-perimeter r)
  (* (+ (rect-length r) (rect-width r)) 2))

(define r (make-rect (make-point 0 0) (make-point 10 5)))

(check-equal? 50 (rect-square r))
(check-equal? 30 (rect-perimeter r))
