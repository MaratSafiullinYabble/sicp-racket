#lang sicp

(#%require rackunit)

(define (filter predicate sequence)
  (cond
    [(null? sequence) nil]
    [(predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence)))]
    [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (item acc) (+ acc 1)) 0 sequence))

(check-equal? 3 (list-ref (map (lambda (x) (+ x 1)) (list 1 2 3)) 1))
(check-equal? 5 (list-ref (append (list 1 2 3) (list 4 5 6)) 4))
(check-equal? 3 (length (list 1 2 3)))
