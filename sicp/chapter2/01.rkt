#lang sicp

(#%require rackunit)
(#%require racket/math)

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ([rat-gcd (gcd n d)]
        [rat-sign (* (/ n (abs n)) (/ d (abs d)))])
    (cons (* rat-sign (abs (/ n rat-gcd))) (abs (/ d rat-gcd)))))

;(print-rat (make-rat -12 -15))

(define rat1 (make-rat 1 2))
(check-equal? 1 (numer rat1))
(check-equal? 2 (denom rat1))

(define rat2 (make-rat 4 8))
(check-equal? 1 (numer rat2))
(check-equal? 2 (denom rat2))

(define rat3 (make-rat -1 2))
(check-equal? -1 (numer rat3))
(check-equal? 2 (denom rat3))

(define rat4 (make-rat 1 -2))
(check-equal? -1 (numer rat4))
(check-equal? 2 (denom rat4))

(define rat5 (make-rat -1 -2))
(check-equal? 1 (numer rat5))
(check-equal? 2 (denom rat5))
