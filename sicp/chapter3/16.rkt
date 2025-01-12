#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))

;(define p (cons (cons 0 0) (cons 0 0)))
;(count-pairs p)

;(define x (cons 0 0))
;(define y (cons x x))
;(define p (cons y 0))
;(count-pairs p)

(define x (cons 0 0))
(define y (cons x x))
(define p (cons y y))
(count-pairs p)
