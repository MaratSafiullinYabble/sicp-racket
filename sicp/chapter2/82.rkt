#lang racket

(define (apply-generic op . args)
  (define (iter tags args)
    (if (null? tags)
        (error "no method")
        (let ([type (car tags)]
              [coerces (filter (lambda (x)
                                 (or (eq? (type-tag x) type) (get-coercion (type-tag x) type)))
                               args)])
          (if (equal? (length args) (length coerces))
              (map (lambda (x)
                     ((get-coercion (type-tag x) type) (contents x))
                     args)
                   (iter (cdr tags) args))))))
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (apply proc (map contents (iter type-tags args)))))))
