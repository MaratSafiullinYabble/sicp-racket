#lang racket

(require rackunit)

;-------------------------------------------------------

(define (make-monitored proc)
  (define count 0)
  (define (mon-proc arg)
    (cond
      [(eq? arg 'how-many-calls?) count]
      [(eq? arg 'reset-count)
       (begin
         (set! count 0)
         count)]
      [else
       (begin
         (set! count (+ count 1))
         (proc arg))]))
  mon-proc)

;-------------------------------------------------------

(define s (make-monitored sqrt))

(check-equal? (s 100) 10)
(check-equal? (s 25) 5)
(check-equal? (s 'how-many-calls?) 2)
(check-equal? (s 'reset-count) 0)
(check-equal? (s 100) 10)
(check-equal? (s 'how-many-calls?) 1)
