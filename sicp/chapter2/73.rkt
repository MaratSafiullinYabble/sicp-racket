#lang sicp

(#%require rackunit)

(define (put op type proc)
  'ok)

(define (get op type)
  (cond
    [(and (eq? op 'deriv) (eq? type '+) deriv-sum)]
    [(and (eq? op 'deriv) (eq? type '*) deriv-product)]
    [(and (eq? op 'deriv) (eq? type '**) deriv-exponentiation)]
    [(and (eq? op 'make) (eq? type '+) make-sum)]
    [(and (eq? op 'make) (eq? type '*) make-product)]
    [(and (eq? op 'make) (eq? type '**) make-exponentiation)]
    [else (error "FOO")]))
;---------------------------------------------------

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [else ((get 'deriv (operator exp)) (operands exp) var)]))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list '+ a1 a2)]))

(define (deriv-sum operands var)
  (let ([addend (car operands)]
        [augend (cadr operands)])
    (make-sum (deriv addend var) (deriv augend var))))

(define (install-sum-package)
  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list '* m1 m2)]))

(define (deriv-product operands var)
  (let ([multiplier (car operands)]
        [multiplicand (cadr operands)]
        [make-sum (get 'make '+)])
    (make-sum (make-product multiplier (deriv multiplicand var))
              (make-product (deriv multiplier var) multiplicand))))

(define (install-product-package)
  (put 'make '* make-product)
  (put 'deriv '* deriv-product))

(define (make-exponentiation b p)
  (cond
    [(=number? p 0) 1]
    [(=number? p 1) b]
    [else (list '** b p)]))

(define (deriv-exponentiation operands var)
  (let ([base (car operands)]
        [exponent (cadr operands)]
        [make-product (get 'make '*)])
    (make-product (make-product exponent (make-exponentiation base (- exponent 1)))
                  (deriv base var))))

(define (install-exponentiation-package)
  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv-exponentiation))

(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv '(+ 3 x) 'x) 1)
(check-equal? (deriv '(* 5 x) 'x) 5)
(check-equal? (deriv '(* x 3) 'x) 3)
(check-equal? (deriv '(** x 1) 'x) 1)

;--------------------------------------------------

;(define (install-sum-package)
;  (define (make-sum a1 a2)
;    (cond
;      [(=number? a1 0) a2]
;      [(=number? a2 0) a1]
;      [(and (number? a1) (number? a2)) (+ a1 a2)]
;      [else (list '+ a1 a2)]))
;
;  (define (deriv-sum operands var)
;    (let ([addend (car operands)]
;          [augend (cadr operands)])
;      (make-sum (deriv addend var) (deriv augend var))))
;
;  (put 'make '+ make-sum)
;  (put 'deriv '+ deriv-sum))
;
;(define (install-product-package)
;  (define (make-product m1 m2)
;    (cond
;      [(or (=number? m1 0) (=number? m2 0)) 0]
;      [(=number? m1 1) m2]
;      [(=number? m2 1) m1]
;      [(and (number? m1) (number? m2)) (* m1 m2)]
;      [else (list '* m1 m2)]))
;
;  (define (deriv-product operands var)
;    (let ([multiplier (car operands)]
;          [multiplicand (cadr operands)]
;          [make-sum (get 'make '+)])
;      (make-sum (make-product multiplier (deriv multiplicand var))
;                (make-product (deriv multiplier var) multiplicand))))
;
;  (put 'make '* make-product)
;  (put 'deriv '* deriv-product))
