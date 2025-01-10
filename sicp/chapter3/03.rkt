#lang racket

(require rackunit)
(require racket/exn)

;-------------------------------------------------------

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        insufficient-message))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pass m)
    (if (eq? pass password)
        (cond
          [(eq? m 'correct?) true]
          [(eq? m 'balance) balance]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown call -- MAKE-ACCOUNT" m)])
        (error incorrect-message)))

  dispatch)

(define (make-joint acc acc-password new-password)
  (define (dispatch pass m)
    (if (eq? pass new-password)
        (acc acc-password m)
        (error incorrect-message)))

  (if (acc acc-password 'correct?) dispatch false))

;-------------------------------------------------------

(define incorrect-acc-pass-message "Wrong account password")

(define incorrect-message "Incorrect password")

(define insufficient-message "Insufficient funds")

(define acc (make-account 100 'secret-password))

(define joint-acc (make-joint acc 'secret-password 'new-password))

(define (wrong-acc-password-attempt)
  (with-handlers ([exn:fail? (lambda (e) (exn->string e))])
    (make-joint acc 'some-other-password 'new-password)))

(define (wrong-password-attempt)
  (with-handlers ([exn:fail? (lambda (e) (exn->string e))])
    ((joint-acc 'some-other-password 'deposit) 50)))

(check-equal? (substring (wrong-acc-password-attempt) 0 18) incorrect-message)
(check-equal? (substring (wrong-password-attempt) 0 18) incorrect-message)
(check-equal? ((joint-acc 'new-password 'withdraw) 40) 60)
(check-equal? ((joint-acc 'new-password 'deposit) 40) 100)
