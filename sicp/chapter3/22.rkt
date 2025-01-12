#lang racket

(require rackunit)

;-------------------------------------------------------

(define (make-queue)
  (define front-ptr '())
  (define rear-ptr '())

  (define (empty-queue?)
    (null? front-ptr))

  (define (front-queue)
    (cond
      [(empty-queue?) (error "DELETE! called with an empty queue")]
      [else (mcar front-ptr)]))

  (define (insert-queue! item)
    (let ([new-pair (mcons item '())])
      (cond
        [(empty-queue?)
         (set! front-ptr new-pair)
         (set! rear-ptr new-pair)
         #t]
        [else
         (set-mcdr! rear-ptr new-pair)
         (set! rear-ptr new-pair)
         #t])))

  (define (delete-queue!)
    (cond
      [(empty-queue?) (error "DELETE! called with an empty queue")]
      [else
       (set! front-ptr (mcdr front-ptr))
       #t]))

  (define (dispatch m)
    (cond
      [(eq? m 'front-queue) (front-queue)]
      [(eq? m 'insert-queue!) insert-queue!]
      [(eq? m 'delete-queue!) (delete-queue!)]
      [else (error "Wrong method")]))

  dispatch)

;-------------------------------------------------------

(define q (make-queue))

((q 'insert-queue!) 'a)
(check-equal? (q 'front-queue) 'a)

((q 'insert-queue!) 'b)
(check-equal? (q 'front-queue) 'a)

(q 'delete-queue!)
(check-equal? (q 'front-queue) 'b)
