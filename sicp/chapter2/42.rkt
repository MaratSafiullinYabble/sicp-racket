#lang sicp

(#%require rackunit)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond
    [(null? sequence) nil]
    [(predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence)))]
    [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (queens board-size)
  (define empty-board nil)

  (define (adjoin-position row col rest-of-queens)
    (cons (cons row col) rest-of-queens))

  (define (safe? positions)
    (define target (car positions))
    (define rest (cdr positions))
    (define (not-same-row? queen)
      (not (= (car queen) (car target))))
    (define (not-same-col? queen)
      (not (= (cdr queen) (cdr target))))
    (define (not-same-diag? queen)
      (not (= (abs (- (car queen) (car target))) (abs (- (cdr queen) (cdr target))))))
    (accumulate (lambda (queen acc)
                  (and acc (not-same-row? queen) (not-same-col? queen) (not-same-diag? queen)))
                true
                rest))

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))

;(check-equal? 10 (length (unique-pairs 5)))
;(check-equal? 5 (length (prime-sum-pairs 5)))
