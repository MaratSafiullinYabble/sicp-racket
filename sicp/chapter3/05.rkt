#lang racket

(require rackunit)

(define (sicp-random n)
  (if (and (exact? n) (integer? n))
      (random n)
      (* n (random))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (/ trials-passed trials)]
      [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (random-in-range low high)
  (let ([range (- high low)]) (+ low (sicp-random range))))

;-------------------------------------------------------

(define (estimate-integral predicate? x1 x2 y1 y2 trials)
  (define (experiment)
    (predicate? (random-in-range x1 x2) (random-in-range y1 y2)))
  (define range-square (* (abs (- x2 x1)) (abs (- y2 y1))))
  (* (monte-carlo trials experiment) range-square))

;-------------------------------------------------------

(define (square x)
  (* x x))

(define (square-predicate? x y)
  (< (+ (square x) (square y)) 1.0))

(define (test-pi trials)
  (exact->inexact (estimate-integral square-predicate? -1.0 1.0 -1.0 1.0 trials)))

(define attempt (floor (* 10 (test-pi 10000))))

(check-true (and (>= attempt 30.0) (<= attempt 32.0)))
