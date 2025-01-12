#lang racket

(require rackunit)
(require compatibility/mlist)

;-------------------------------------------------------

(define (cycle? list)
  (define (has-eq? pair list)
    (cond
      [(null? list) #f]
      [(eq? pair list) #t]
      [else (has-eq? pair (cdr list))]))

  (cond
    [(null? list) #f]
    [(has-eq? list (cdr list)) #t]
    [else (cycle? (cdr list))]))

;----------------------------------------------

(define (cdr x)
  (mcdr x))

(define (set-cdr! rest pair)
  (set-mcdr! rest pair))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (mlist 'a 'b 'c)))

(check-equal? (cycle? z) #t)
(check-equal? (cycle? (mlist 1 2 3)) #f)
