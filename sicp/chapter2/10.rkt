#lang sicp

(#%require rackunit)

(define (sign x)
  (cond
    [(> x 0) 1]
    [(= x 0) 0]
    [(< x 0) -1]))

(define (make-interval a b)
  (cons (min a b) (max a b)))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (not (= (sign (lower-bound y)) (sign (upper-bound y))))
      (error "Illegal arguments")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

(define int1 (make-interval 3 7))
(define int2 (make-interval -2 9))

(div-interval int1 int2)
