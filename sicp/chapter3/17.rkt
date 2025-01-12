#lang racket

(require rackunit)

;-------------------------------------------------------

(define (count-pairs x)
  (define parsed-pairs '())

  (define (in-list pair list)
    (cond
      [(null? list) #f]
      [(eq? pair (car list)) #t]
      [else (in-list pair (cdr list))]))

  (define (add-pair! pair)
    (set! parsed-pairs (cons pair parsed-pairs)))

  (define (count x)
    (cond
      [(not (pair? x)) 0]
      [(in-list x parsed-pairs) 0]
      [else
       (begin
         (add-pair! x)
         (+ (count (car x)) (count (cdr x)) 1))]))

  (count x))

;----------------------------------------------

(define nil '())
(define p1 (cons 'a nil))
(define p2 (cons p1 nil))
(define p3 (cons p1 p2))
(define p4 (cons p2 p1))
(define p5 (cons p1 p1))
(define p6 (cons p5 p5))

(check-equal? (count-pairs p3) 3)
(check-equal? (count-pairs p4) 3)
(check-equal? (count-pairs p6) 3)
(check-equal? (count-pairs '()) 0)
