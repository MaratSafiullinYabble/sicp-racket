#lang sicp

(#%require rackunit)

(define (make-interval a b)
  (cons (min a b) (max a b)))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ([w (* c (/ p 100))]) (make-interval (- c w) (+ c w))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

(define int1 (make-center-percent 10 10))
(check-equal? 9 (lower-bound int1))
(check-equal? 11 (upper-bound int1))

(define int2 (make-center-percent 8 5))
(check-equal? 5 (percent int2))
