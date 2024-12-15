#lang sicp

(#%require rackunit)

(define (reverse-deep list)
  (define (reverse-iter r-list list)
    (if (null? list)
        r-list
        (let* ([first (car list)]
              [rest (cdr list)]
              [r-first (if (list? first)
                           (reverse-deep first)
                           first)])
          (reverse-iter (cons r-first r-list) rest))))
  (reverse-iter nil list))

(define r-list (reverse-deep (list 0 (list 1 2 3) (cons 2 3) (list 3 4 5) 4)))

(check-equal? 4 (list-ref r-list 0))
(check-equal? 4 (list-ref (list-ref r-list 1) 1))
(check-equal? 3 (list-ref (list-ref r-list 3) 0))
(check-equal? 2 (car (list-ref r-list 2)))
