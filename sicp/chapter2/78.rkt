#lang sicp

(#%require rackunit)

(define (attach-tag type-tag contents)
  (cond
    [(number? contents) contents]
    [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond
    [(number? datum) 'scheme-number]
    [(pair? datum) (car datum)]
    [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(number? datum) datum]
    [(pair? datum) (cdr datum)]
    [else (error "Bad tagged datum -- CONTENTS" datum)]))

;------------------------------------------------------

(check-equal? (attach-tag 'scheme-number 42) 42)
(check-equal? (attach-tag 'complex (cons 1 2)) (cons 'complex (cons 1 2)))
(check-equal? (contents 42) 42)
(check-equal? (contents (cons 'complex (cons 1 2))) (cons 1 2))
(check-equal? (type-tag (cons 'complex (cons 1 2))) 'complex)
(check-equal? (type-tag 10) 'scheme-number)
