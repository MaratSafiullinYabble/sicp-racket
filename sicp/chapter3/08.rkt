#lang racket

(require rackunit)

;-------------------------------------------------------

(define (make-f)
  (define state 0)
  (define old-state 0)
  (lambda (x)
    (begin
      (set! old-state state)
      (set! state x)
      old-state)))

;-------------------------------------------------------

(define f (make-f))

(define left f)

(check-equal? (+ (left 0) (left 1)) 0)

(define right (make-f))

(check-equal? (+ (right 1) (right 0)) 1)
