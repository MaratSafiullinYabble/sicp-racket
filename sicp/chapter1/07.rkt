#lang sicp

(#%require rackunit)
(#%require racket/math)

(define precision 0.001)

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
  (< (abs (/ (- prev-guess guess) guess)) precision))



(define (test-iter x limit)
  (check-true (< (abs (/ (- x (sqr (sqrt x))) x)) (sqr precision)))
  (if (>= x limit)
      "Test successful"
      (test-iter (+ x 1) limit)))

(define (test)
  (test-iter 1 1000))

(test)
