#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (sqrt-iter guess prev-guess x)
    (define (good-enough?)
      (< (abs (/ (- prev-guess guess) guess)) 0.000001))

    (define (improved-x)
      (average guess (/ x guess)))
    
    (if (good-enough?)
        guess
        (sqrt-iter (improved-x) guess x)))
  
  (sqrt-iter 1.0 0.0 x))

(sqrt 2)
