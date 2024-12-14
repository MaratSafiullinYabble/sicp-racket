#lang sicp

(#%require rackunit)

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rect b-l l w)
  (cons b-l (cons l w)))

(define (rect-length r)
 (car (cdr r)))

(define (rect-width r)
  (cdr (cdr r)))

(define (rect-square r)
  (* (rect-length r) (rect-width r)))

(define (rect-perimeter r)
  (* (+ (rect-length r) (rect-width r)) 2))

(define r (make-rect (make-point 0 0) 10 5))

(check-equal? 50 (rect-square r))
(check-equal? 30 (rect-perimeter r))
