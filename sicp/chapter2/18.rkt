#lang sicp

(#%require rackunit)

(define (reverse list)
  (define (reverse-iter r-list list)
    (if (null? list)
        r-list
        (reverse-iter (cons (car list) r-list) (cdr list))))
  (reverse-iter nil list))

(check-equal? 1 (car (reverse (list 1))))
(define r-list (reverse (list 8 6 7)))
(check-equal? 7 (list-ref r-list 0))
(check-equal? 6 (list-ref r-list 1))
(check-equal? 8 (list-ref r-list 2))
