#lang sicp

(#%require rackunit)

(define (make-interval a b)
  (cons (min a b) (max a b)))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

(define int1 (make-interval 3 7))
(check-equal? 3 (lower-bound int1))
(check-equal? 7 (upper-bound int1))

(define int2 (make-interval 8 4))
(check-equal? 4 (lower-bound int2))
(check-equal? 8 (upper-bound int2))
