#lang racket

(require rackunit)

;-------------------------------------------------------

(define (rand action)
  (define current 0)
  (cond
    [(eq? action 'generate)
     (begin
       (set! current (+ current 1)) ;not random but will do for the case
       current)]
    [(eq? action 'reset) (lambda (x) (set! current x))]
    [else (error "wrong action -- RAND")]))

;-------------------------------------------------------

(define random-value (rand 'generate))
(define test-value-1 (rand 'generate))
(define test-value-2 (rand 'generate))
(define test-value-3 (rand 'generate))

((rand 'reset) random-value)

(check-equal? test-value-1 (rand 'generate))
(check-equal? test-value-2 (rand 'generate))
(check-equal? test-value-3 (rand 'generate))
