#lang sicp

(#%require rackunit)

(define (count-change amount coins-list)
  (define (cc amount coins-list)
    (define (first-denomination)
      (car coins-list))
    (define (except-first-denomination)
      (cdr coins-list))
    (define (no-more-coins?)
      (null? coins-list))
    (cond
      [(= amount 0) 1]
      [(or (< amount 0) (no-more-coins?)) 0]
      [else
       (+ (cc amount (except-first-denomination)) (cc (- amount (first-denomination)) coins-list))]))
  (cc amount coins-list))

(define us-coins (list 50 25 10 5 1))
(check-equal? 4 (count-change 10 us-coins))
(check-equal? 13 (count-change 25 us-coins))
(check-equal? 50 (count-change 50 us-coins))
(check-equal? 112 (count-change 70 us-coins))
(check-equal? 292 (count-change 100 us-coins))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(check-equal? 4 (count-change 2 uk-coins))
(check-equal? 9 (count-change 4 uk-coins))
(check-equal? 13 (count-change 5 uk-coins))
(check-equal? 50 (count-change 10 uk-coins))
(check-equal? 571 (count-change 25 uk-coins))
