#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (list? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   tree)))

(check-equal? 7 (count-leaves (list 1 2 3 (list 4 5 (list 6 7)))))
